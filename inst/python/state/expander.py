from __future__ import annotations

import itertools
import random
import warnings

from registry.schema import ComponentRegistry
from state.tensor import StateError

VALID_EDGE_STATUSES = ("causal", "unknown", "non-causal")
VALID_BIDIRECTED_STATUSES = ("present", "absent")
LARGE_EXPANSION_THRESHOLD = 10_000


class ExpansionResult(list):
    """List-compatible expansion output with its pruning diagnostics."""

    def __init__(self, records: list[dict], pruning_report: dict):
        super().__init__(records)
        self.pruning_report = pruning_report


def _has_cycle(directed_edges: set[tuple[str, str]]) -> bool:
    """Return True if the directed graph has a cycle."""
    adj: dict[str, list[str]] = {}
    for src, tgt in directed_edges:
        adj.setdefault(src, []).append(tgt)
        adj.setdefault(tgt, [])

    visiting: set[str] = set()
    visited: set[str] = set()

    def _visit(node: str) -> bool:
        if node in visiting:
            return True
        if node in visited:
            return False
        visiting.add(node)
        for nxt in adj.get(node, []):
            if _visit(nxt):
                return True
        visiting.remove(node)
        visited.add(node)
        return False

    return any(_visit(n) for n in list(adj))


class ModelStateExpander:
    """Generate model-state records from a component registry."""

    @staticmethod
    def expand(
        registry: ComponentRegistry,
        *,
        mode: str = "sampled",
        seed_claims: list[dict] | None = None,
        node_timing: dict[str, int] | None = None,
        timing_options: dict[str, list[int]] | None = None,
        optional_nodes: list[str] | None = None,
        max_models: int = 10_000,
        n_models: int | None = None,
        seed: int | None = None,
        edge_statuses: list[str] | None = None,
        bidirected_statuses: list[str] | None = None,
        node_policy: str = "all-present",
        allow_large: bool = False,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> ExpansionResult:
        """Expand registry components into temporally valid model states.

        ``node_timing`` and ``node_policy`` retain the legacy single-timing,
        node-subset behavior. Supplying ``timing_options`` selects one timing
        per present node in every model. ``optional_nodes`` then limits subset
        variation to those nodes only.
        """
        if edge_statuses is None:
            edge_statuses = list(VALID_EDGE_STATUSES)
        if bidirected_statuses is None:
            bidirected_statuses = list(VALID_BIDIRECTED_STATUSES)

        ModelStateExpander._validate_statuses(
            edge_statuses,
            VALID_EDGE_STATUSES,
            "edge_statuses",
        )
        ModelStateExpander._validate_statuses(
            bidirected_statuses,
            VALID_BIDIRECTED_STATUSES,
            "bidirected_statuses",
        )
        if node_policy not in ("all-present", "vary"):
            raise StateError(
                f"Unknown node_policy: {node_policy!r}. " "Use 'all-present' or 'vary'."
            )
        if max_models < 1:
            raise StateError("max_models must be a positive integer.")

        df = registry.data
        node_comps = df[df["type"] == "node"]
        edge_comps = df[df["type"] == "edge"]
        node_names = dict(zip(node_comps["source"], node_comps["comp_id"]))
        node_names_by_cid = {cid: name for name, cid in node_names.items()}

        edge_ids = edge_comps["comp_id"].tolist()
        edge_sources = edge_comps.set_index("comp_id")["source"].to_dict()
        edge_targets = edge_comps.set_index("comp_id")["target"].to_dict()
        edge_directions = edge_comps.set_index("comp_id")["direction"].to_dict()
        fixed_causal_edge_ids: set[str] = set()
        if "fixed_status" in edge_comps.columns:
            fixed_causal_edge_ids = set(
                edge_comps.loc[
                    edge_comps["fixed_status"] == "causal", "comp_id"
                ].tolist()
            )

        if fixed_causal_edge_ids and "causal" not in edge_statuses:
            raise StateError(
                "Registry contains fixed causal edge(s) but 'causal' is not "
                "in edge_statuses, so the fixed invariant cannot be represented."
            )

        if (exposure is None) != (outcome is None):
            raise StateError(
                "Both or neither of exposure and outcome must be provided."
            )

        required_node_cids: set[str] = set()
        if exposure is not None and outcome is not None:
            if exposure == outcome:
                raise StateError("Exposure and outcome must be distinct nodes.")
            invalid_targets = [n for n in (exposure, outcome) if n not in node_names]
            if invalid_targets:
                raise StateError(f"Invalid exposure/outcome node(s): {invalid_targets}")
            required_node_cids = {node_names[exposure], node_names[outcome]}

        for cid in fixed_causal_edge_ids:
            source = edge_sources[cid]
            target = edge_targets[cid]
            required_node_cids.add(node_names[source])
            required_node_cids.add(node_names[target])

        normalized_timing_options = ModelStateExpander._validate_timing_options(
            timing_options,
            node_names,
        )
        optional_node_cids = ModelStateExpander._optional_node_cids(
            optional_nodes,
            node_names,
            required_node_cids,
        )
        node_subsets = ModelStateExpander._node_subsets(
            node_names,
            required_node_cids,
            node_policy,
            optional_node_cids,
        )

        report = {
            "timing_assignments_considered": 0,
            "timing_assignments_pruned": 0,
            "required_edge_assignments_pruned": 0,
            "temporal_edge_statuses_pruned": 0,
            "temporal_edge_assignments_pruned": 0,
            "cycle_models_pruned": 0,
            "projected_model_count": 0,
            "generated_model_count": 0,
            "warnings": [],
        }
        spaces = ModelStateExpander._build_spaces(
            node_subsets=node_subsets,
            node_names=node_names,
            node_names_by_cid=node_names_by_cid,
            node_timing=node_timing or {},
            timing_options=normalized_timing_options,
            strict_timing=timing_options is not None,
            edge_ids=edge_ids,
            edge_sources=edge_sources,
            edge_targets=edge_targets,
            edge_directions=edge_directions,
            edge_statuses=edge_statuses,
            bidirected_statuses=bidirected_statuses,
            fixed_causal_edge_ids=fixed_causal_edge_ids,
            report=report,
        )
        projected = sum(space["combination_count"] for space in spaces)
        report["projected_model_count"] = projected

        if mode == "exhaustive":
            if projected > max_models:
                raise StateError(
                    f"Exhaustive expansion projected {projected} models, "
                    f"exceeding max_models={max_models}. "
                    "Use mode='sampled' or raise max_models."
                )
            if projected > LARGE_EXPANSION_THRESHOLD:
                if not allow_large:
                    raise StateError(
                        f"Exhaustive expansion projected {projected} models. "
                        "Set allow_large=True to proceed; max_models remains a hard cap."
                    )
                message = (
                    f"Allowing large exhaustive expansion of {projected} models "
                    f"within max_models={max_models}."
                )
                warnings.warn(message)
                report["warnings"].append(message)
            records = ModelStateExpander._expand_exhaustive(
                spaces,
                edge_directions,
                report,
                projected,
            )
        elif mode == "sampled":
            records = ModelStateExpander._expand_sampled(
                spaces,
                edge_directions,
                report,
                n_models or 100,
                seed,
            )
        else:
            raise StateError(
                f"Unknown expansion mode: {mode!r}. "
                "Use 'sampled' or 'exhaustive'. "
                "To inject user theories, pass seed_claims alongside either mode."
            )

        if seed_claims:
            records = ModelStateExpander._integrate_seeds(
                generated_records=records,
                seed_claims=seed_claims,
                registry=registry,
                node_timing=node_timing or {},
                timing_options=normalized_timing_options,
                required_node_cids=required_node_cids,
                fixed_causal_edge_ids=fixed_causal_edge_ids,
            )
        else:
            for record in records:
                record["seeded"] = False

        report["generated_model_count"] = len({r["model_id"] for r in records})
        return ExpansionResult(records, report)

    @staticmethod
    def _validate_statuses(
        statuses: list[str], valid: tuple[str, ...], field: str
    ) -> None:
        invalid = [status for status in statuses if status not in valid]
        if invalid:
            label = "edge status(es)" if field == "edge_statuses" else field
            raise StateError(f"Invalid {label}: {invalid}. Choose from {list(valid)}.")
        if len(set(statuses)) != len(statuses):
            raise StateError(f"Duplicate values in {field} are not allowed.")
        if not statuses:
            raise StateError(f"{field} must contain at least one status.")

    @staticmethod
    def _validate_timing_options(
        timing_options: dict[str, list[int]] | None,
        node_names: dict[str, str],
    ) -> dict[str, list[int]]:
        if timing_options is None:
            return {}
        unknown = sorted(set(timing_options) - set(node_names))
        if unknown:
            raise StateError(f"timing_options references unknown node(s): {unknown}")

        normalized: dict[str, list[int]] = {}
        for name, values in timing_options.items():
            if not values:
                raise StateError(f"timing_options[{name!r}] must not be empty.")
            if any(
                not isinstance(value, int) or isinstance(value, bool)
                for value in values
            ):
                raise StateError(
                    f"timing_options[{name!r}] must contain integers only."
                )
            if len(set(values)) != len(values):
                raise StateError(
                    f"Duplicate values in timing_options[{name!r}] are not allowed."
                )
            normalized[name] = list(values)
        return normalized

    @staticmethod
    def _optional_node_cids(
        optional_nodes: list[str] | None,
        node_names: dict[str, str],
        required_node_cids: set[str],
    ) -> set[str] | None:
        if optional_nodes is None:
            return None
        unknown = sorted(set(optional_nodes) - set(node_names))
        if unknown:
            raise StateError(f"optional_nodes references unknown node(s): {unknown}")
        if len(set(optional_nodes)) != len(optional_nodes):
            raise StateError("Duplicate values in optional_nodes are not allowed.")
        # Focal and fixed-edge endpoints are structural requirements, even if
        # a caller also places them in optional_nodes.
        return {node_names[name] for name in optional_nodes} - required_node_cids

    @staticmethod
    def _node_subsets(
        node_names: dict[str, str],
        required_node_cids: set[str],
        node_policy: str,
        optional_node_cids: set[str] | None,
    ) -> list[set[str]]:
        node_cids = set(node_names.values())
        if optional_node_cids is not None:
            fixed_nodes = node_cids - optional_node_cids
            return [
                fixed_nodes | set(choice)
                for size in range(len(optional_node_cids) + 1)
                for choice in itertools.combinations(sorted(optional_node_cids), size)
            ]
        if node_policy == "all-present":
            return [node_cids]
        return [
            set(choice)
            for size in range(1, len(node_cids) + 1)
            for choice in itertools.combinations(sorted(node_cids), size)
            if required_node_cids.issubset(choice)
        ]

    @staticmethod
    def _timing_assignments(
        subset: set[str],
        node_names_by_cid: dict[str, str],
        node_timing: dict[str, int],
        timing_options: dict[str, list[int]],
    ):
        ordered_cids = sorted(subset)
        choices = [
            timing_options.get(
                node_names_by_cid[cid],
                [node_timing.get(node_names_by_cid[cid])],
            )
            for cid in ordered_cids
        ]
        for values in itertools.product(*choices):
            yield dict(zip(ordered_cids, values))

    @staticmethod
    def _build_spaces(
        *,
        node_subsets: list[set[str]],
        node_names: dict[str, str],
        node_names_by_cid: dict[str, str],
        node_timing: dict[str, int],
        timing_options: dict[str, list[int]],
        strict_timing: bool,
        edge_ids: list[str],
        edge_sources: dict[str, str],
        edge_targets: dict[str, str],
        edge_directions: dict[str, str],
        edge_statuses: list[str],
        bidirected_statuses: list[str],
        fixed_causal_edge_ids: set[str],
        report: dict,
    ) -> list[dict]:
        spaces: list[dict] = []
        for subset in node_subsets:
            for timing in ModelStateExpander._timing_assignments(
                subset,
                node_names_by_cid,
                node_timing,
                timing_options,
            ):
                report["timing_assignments_considered"] += 1
                fixed_edges: list[str] = []
                choices: list[tuple[str, list[str]]] = []
                valid = True

                for cid in edge_ids:
                    source_cid = node_names[edge_sources[cid]]
                    target_cid = node_names[edge_targets[cid]]
                    if source_cid not in subset or target_cid not in subset:
                        continue

                    directed = edge_directions[cid] == "->"
                    temporally_eligible = not directed or ModelStateExpander._eligible(
                        timing[source_cid],
                        timing[target_cid],
                    )
                    if cid in fixed_causal_edge_ids:
                        if not temporally_eligible:
                            report["timing_assignments_pruned"] += 1
                            report["required_edge_assignments_pruned"] += 1
                            valid = False
                            break
                        fixed_edges.append(cid)
                        continue

                    statuses = (
                        list(edge_statuses) if directed else list(bidirected_statuses)
                    )
                    if directed and not temporally_eligible:
                        before = len(statuses)
                        statuses = [status for status in statuses if status != "causal"]
                        report["temporal_edge_statuses_pruned"] += before - len(
                            statuses
                        )
                    if not statuses:
                        # No requested status can represent this mutable edge
                        # under the timing assignment, so the whole timing
                        # assignment is explicitly discarded.  When any
                        # non-causal status remains, the edge component stays
                        # in the model with the reduced status dimension.
                        report["temporal_edge_assignments_pruned"] += 1
                        valid = False
                        break
                    choices.append((cid, statuses))

                if not valid:
                    continue

                combination_count = 1
                for _, statuses in choices:
                    combination_count *= len(statuses)
                spaces.append(
                    {
                        "subset": subset,
                        "timing": timing,
                        "fixed_edges": fixed_edges,
                        "choices": choices,
                        "combination_count": combination_count,
                        "edge_sources": edge_sources,
                        "edge_targets": edge_targets,
                    }
                )
        return spaces

    @staticmethod
    def _eligible(source_timing: int | None, target_timing: int | None) -> bool:
        return (
            source_timing is None
            or target_timing is None
            or source_timing < target_timing
        )

    @staticmethod
    def _record_model(
        records: list[dict],
        model_id: str,
        subset: set[str],
        timing: dict[str, int | None],
        edge_statuses: dict[str, str],
        fixed_edges: list[str],
    ) -> None:
        for cid in sorted(subset):
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": cid,
                    "status": "present",
                    "timing": timing[cid],
                }
            )
        for cid in sorted(edge_statuses):
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": cid,
                    "status": edge_statuses[cid],
                    "timing": None,
                }
            )
        for cid in sorted(fixed_edges):
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": cid,
                    "status": "causal",
                    "timing": None,
                }
            )

    @staticmethod
    def _choice_statuses(space: dict, selection: tuple[str, ...]) -> dict[str, str]:
        return {cid: status for (cid, _), status in zip(space["choices"], selection)}

    @staticmethod
    def _is_acyclic(
        space: dict,
        edge_statuses: dict[str, str],
        edge_directions: dict[str, str],
    ) -> bool:
        edge_sources = space["edge_sources"]
        edge_targets = space["edge_targets"]
        directed_edges = {
            (edge_sources[cid], edge_targets[cid])
            for cid in space["fixed_edges"]
            if edge_directions[cid] == "->"
        }
        directed_edges.update(
            (edge_sources[cid], edge_targets[cid])
            for cid, status in edge_statuses.items()
            if edge_directions[cid] == "->" and status == "causal"
        )
        return not _has_cycle(directed_edges)

    @staticmethod
    def _expand_exhaustive(
        spaces: list[dict],
        edge_directions: dict[str, str],
        report: dict,
        projected: int,
    ) -> list[dict]:
        records: list[dict] = []
        model_counter = 0
        width = max(4, len(str(projected)))
        for space in spaces:
            selections = itertools.product(
                *(statuses for _, statuses in space["choices"])
            )
            for selection in selections:
                statuses = ModelStateExpander._choice_statuses(space, selection)
                if not ModelStateExpander._is_acyclic(
                    space,
                    statuses,
                    edge_directions,
                ):
                    report["cycle_models_pruned"] += 1
                    continue
                model_counter += 1
                ModelStateExpander._record_model(
                    records,
                    f"M{model_counter:0{width}d}",
                    space["subset"],
                    space["timing"],
                    statuses,
                    space["fixed_edges"],
                )
        return records

    @staticmethod
    def _expand_sampled(
        spaces: list[dict],
        edge_directions: dict[str, str],
        report: dict,
        n_models: int,
        seed: int | None,
    ) -> list[dict]:
        records: list[dict] = []
        if not spaces:
            return records
        rng = random.Random(seed)
        width = max(4, len(str(n_models)))
        seen_keys: set[tuple] = set()
        attempts = 0
        model_counter = 0
        max_attempts = n_models * 50

        while model_counter < n_models and attempts < max_attempts:
            attempts += 1
            space = rng.choice(spaces)
            selection = tuple(rng.choice(statuses) for _, statuses in space["choices"])
            key = (
                tuple(sorted(space["subset"])),
                tuple(sorted(space["timing"].items())),
                selection,
            )
            if key in seen_keys:
                continue
            seen_keys.add(key)
            statuses = ModelStateExpander._choice_statuses(space, selection)
            if not ModelStateExpander._is_acyclic(space, statuses, edge_directions):
                report["cycle_models_pruned"] += 1
                continue

            model_counter += 1
            ModelStateExpander._record_model(
                records,
                f"M{model_counter:0{width}d}",
                space["subset"],
                space["timing"],
                statuses,
                space["fixed_edges"],
            )

        if model_counter < n_models:
            message = (
                f"Only generated {model_counter} models; requested {n_models}. "
                "Not enough valid model combinations exist."
            )
            warnings.warn(message)
            report["warnings"].append(message)
        return records

    # ------------------------------------------------------------------
    # Seed integration
    # ------------------------------------------------------------------

    @staticmethod
    def _integrate_seeds(
        *,
        generated_records: list[dict],
        seed_claims: list[dict],
        registry: ComponentRegistry,
        node_timing: dict[str, int],
        timing_options: dict[str, list[int]],
        required_node_cids: set[str],
        fixed_causal_edge_ids: set[str],
    ) -> list[dict]:
        """Find or append seeded models using statuses and claimed timings."""
        component_types = registry.data.set_index("comp_id")["type"].to_dict()
        valid_comp_ids = set(component_types)
        node_map = dict(
            zip(
                registry.data.loc[registry.data["type"] == "node", "source"],
                registry.data.loc[registry.data["type"] == "node", "comp_id"],
            )
        )
        node_names_by_cid = {cid: name for name, cid in node_map.items()}
        node_comp_ids = set(node_map.values())
        edge_rows = registry.data[registry.data["type"] == "edge"].set_index("comp_id")
        edge_endpoints = {
            cid: (node_map.get(row["source"]), node_map.get(row["target"]))
            for cid, row in edge_rows.iterrows()
        }
        edge_endpoint_names = {
            cid: (row["source"], row["target"]) for cid, row in edge_rows.iterrows()
        }
        edge_directions = edge_rows["direction"].to_dict()

        for claim in seed_claims:
            cid = claim.get("comp_id")
            if cid not in valid_comp_ids:
                raise StateError(f"Unknown component ID in seed claim: {cid!r}")
            status = claim.get("status")
            if component_types[cid] == "node":
                if status not in {"present", "absent", *VALID_EDGE_STATUSES}:
                    raise StateError(
                        f"Invalid node status in seed claim for {cid}: {status!r}"
                    )
            else:
                valid_statuses = (
                    VALID_BIDIRECTED_STATUSES
                    if edge_directions[cid] == "<->"
                    else VALID_EDGE_STATUSES
                )
                if status not in valid_statuses:
                    raise StateError(
                        f"Invalid edge status in seed claim for {cid}: {status!r}"
                    )
            timing = claim.get("timing")
            if timing is not None and cid in node_names_by_cid:
                name = node_names_by_cid[cid]
                if name in timing_options and timing not in timing_options[name]:
                    raise StateError(
                        f"Seed timing for node {name!r} is not in its timing_options."
                    )

        for cid in fixed_causal_edge_ids:
            expected_status = "present" if edge_directions[cid] == "<->" else "causal"
            for claim in seed_claims:
                if (
                    claim.get("comp_id") == cid
                    and claim.get("status") != expected_status
                ):
                    raise StateError(
                        f"Seed claim sets fixed edge {cid} to {claim.get('status')!r}, "
                        f"but this edge must be {expected_status!r} in the registry"
                    )

        seed_models: dict[str, dict[str, str]] = {}
        seed_timing: dict[str, dict[str, int]] = {}
        seed_order: list[str] = []
        for claim in seed_claims:
            model_id = claim["model_id"]
            if model_id not in seed_models:
                seed_models[model_id] = {}
                seed_timing[model_id] = {}
                seed_order.append(model_id)
            seed_models[model_id][claim["comp_id"]] = claim["status"]
            if claim.get("timing") is not None:
                seed_timing[model_id][claim["comp_id"]] = claim["timing"]

        def node_present(status: str) -> bool:
            return status in ("present", "causal")

        def default_edge_status(cid: str) -> str:
            return "absent" if edge_directions[cid] == "<->" else "unknown"

        def normalize(model_id: str) -> dict:
            claims = seed_models[model_id]
            present_nodes: set[str] = set()
            absent_nodes: set[str] = set()
            statuses: dict[str, str] = {}
            for cid, status in claims.items():
                if component_types[cid] == "node":
                    if node_present(status):
                        if cid in absent_nodes:
                            raise StateError(
                                f"Seed model {model_id} marks node {cid} both present and absent"
                            )
                        present_nodes.add(cid)
                    else:
                        if cid in present_nodes:
                            raise StateError(
                                f"Seed model {model_id} marks node {cid} both present and absent"
                            )
                        absent_nodes.add(cid)
                else:
                    statuses[cid] = status

            for cid in statuses:
                source_cid, target_cid = edge_endpoints[cid]
                source_name, target_name = edge_endpoint_names[cid]
                if source_cid is None or target_cid is None:
                    raise StateError(
                        f"Seed edge {cid} references unknown endpoint "
                        f"{source_name!r} or {target_name!r}"
                    )
                for node_cid in (source_cid, target_cid):
                    if node_cid in absent_nodes:
                        raise StateError(
                            f"Seed model {model_id} claims edge {cid}, but endpoint "
                            f"node {node_cid} is explicitly absent"
                        )
                    present_nodes.add(node_cid)

            for cid in fixed_causal_edge_ids:
                # State tensors encode active bidirected edges as causal.
                statuses[cid] = "causal"
                source_cid, target_cid = edge_endpoints[cid]
                if source_cid in absent_nodes or target_cid in absent_nodes:
                    raise StateError(
                        f"Seed model {model_id} omits endpoint(s) of fixed edge {cid}. "
                        "Fixed edge endpoints cannot be absent."
                    )
                present_nodes.update((source_cid, target_cid))

            return {
                "present_nodes": present_nodes,
                "edge_statuses": statuses,
                "timing": seed_timing[model_id],
            }

        def semantic_vector(present_nodes: set[str], statuses: dict[str, str]) -> tuple:
            items: list[tuple[str, str]] = [
                (cid, "present") for cid in sorted(present_nodes)
            ]
            for cid, (source_cid, target_cid) in sorted(edge_endpoints.items()):
                if source_cid in present_nodes and target_cid in present_nodes:
                    items.append((cid, statuses.get(cid, default_edge_status(cid))))
            return tuple(items)

        normalized_seeds = {model_id: normalize(model_id) for model_id in seed_order}
        for model_id, normalized in normalized_seeds.items():
            missing = required_node_cids - normalized["present_nodes"]
            if missing:
                raise StateError(
                    f"Seed model {model_id} omits required exposure/outcome node(s): "
                    + ", ".join(sorted(missing))
                )

        seed_vectors = {
            model_id: semantic_vector(
                normalized["present_nodes"], normalized["edge_statuses"]
            )
            for model_id, normalized in normalized_seeds.items()
        }
        generated_statuses: dict[str, dict[str, str]] = {}
        generated_timing: dict[str, dict[str, int]] = {}
        for record in generated_records:
            generated_statuses.setdefault(record["model_id"], {})[record["comp_id"]] = (
                record["status"]
            )
            if record["comp_id"] in node_comp_ids and record.get("timing") is not None:
                generated_timing.setdefault(record["model_id"], {})[
                    record["comp_id"]
                ] = record["timing"]

        generated_vectors = {}
        for model_id, statuses in generated_statuses.items():
            present_nodes = {
                cid
                for cid, status in statuses.items()
                if cid in node_comp_ids and node_present(status)
            }
            generated_vectors[model_id] = semantic_vector(present_nodes, statuses)

        matched_generated: set[str] = set()
        seed_to_generated: dict[str, str] = {}
        for seed_id, vector in seed_vectors.items():
            timing_claims = normalized_seeds[seed_id]["timing"]
            for generated_id, generated_vector in generated_vectors.items():
                if generated_id in matched_generated or vector != generated_vector:
                    continue
                if all(
                    generated_timing.get(generated_id, {}).get(cid) == value
                    for cid, value in timing_claims.items()
                    if cid in node_comp_ids
                ):
                    seed_to_generated[seed_id] = generated_id
                    matched_generated.add(generated_id)
                    break

        result: list[dict] = []
        for seed_id in seed_order:
            if seed_id in seed_to_generated:
                generated_id = seed_to_generated[seed_id]
                result.extend(
                    {
                        "model_id": seed_id,
                        "comp_id": record["comp_id"],
                        "status": record["status"],
                        "timing": record.get("timing"),
                        "seeded": True,
                    }
                    for record in generated_records
                    if record["model_id"] == generated_id
                )
                continue

            normalized = normalized_seeds[seed_id]
            for cid in sorted(normalized["present_nodes"]):
                name = node_names_by_cid[cid]
                timing = normalized["timing"].get(cid)
                if timing is None:
                    timing = node_timing.get(name)
                if timing is None and name in timing_options:
                    timing = timing_options[name][0]
                result.append(
                    {
                        "model_id": seed_id,
                        "comp_id": cid,
                        "status": "present",
                        "timing": timing,
                        "seeded": True,
                    }
                )
            for cid, (source_cid, target_cid) in sorted(edge_endpoints.items()):
                if (
                    source_cid in normalized["present_nodes"]
                    and target_cid in normalized["present_nodes"]
                ):
                    result.append(
                        {
                            "model_id": seed_id,
                            "comp_id": cid,
                            "status": normalized["edge_statuses"].get(
                                cid,
                                default_edge_status(cid),
                            ),
                            "timing": None,
                            "seeded": True,
                        }
                    )

        remaining_ids = [
            model_id
            for model_id in generated_statuses
            if model_id not in matched_generated
        ]
        width = max(4, len(str(len(seed_order) + len(remaining_ids))))
        used_ids = set(seed_order)
        remapped_ids: dict[str, str] = {}
        next_number = len(seed_order) + 1
        for generated_id in remaining_ids:
            while f"M{next_number:0{width}d}" in used_ids:
                next_number += 1
            remapped = f"M{next_number:0{width}d}"
            used_ids.add(remapped)
            remapped_ids[generated_id] = remapped
            next_number += 1
        result.extend(
            {
                "model_id": remapped_ids[record["model_id"]],
                "comp_id": record["comp_id"],
                "status": record["status"],
                "timing": record.get("timing"),
                "seeded": False,
            }
            for record in generated_records
            if record["model_id"] in remapped_ids
        )
        return result

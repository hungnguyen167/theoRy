from __future__ import annotations

import itertools
import random
import warnings

from registry.schema import ComponentRegistry
from state.tensor import StateError

VALID_EDGE_STATUSES = ("causal", "unknown", "non-causal")


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
        max_models: int = 10_000,
        n_models: int | None = None,
        seed: int | None = None,
        edge_statuses: list[str] | None = None,
        node_policy: str = "all-present",
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Expand registry components into model-state records.

        Parameters
        ----------
        registry:
            The component registry defining the universe of components.
        mode:
            ``sampled`` or ``exhaustive``.
        seed_claims:
            Optional list of ``{model_id, comp_id, status, timing}`` dicts.
            When provided, the engine searches for each seeded model in the
            generated multiverse.  Found models are promoted to the top and
            flagged ``seeded=True``.  Models not found in the multiverse are
            appended at the top with ``seeded=True``.
        node_timing:
            Optional mapping of ``node_name -> timing_int`` used for temporal
            validation in exhaustive / sampled modes.
        max_models:
            Safety cap for exhaustive mode.  Expansion fails fast if the
            projected count exceeds this value.
        n_models:
            Number of models to sample in ``sampled`` mode (default 100).
        seed:
            Random seed for reproducible ``sampled`` expansion.
        edge_statuses:
            Statuses to enumerate/sample over for edge components in
            exhaustive and sampled modes.  Defaults to
            ``["causal", "unknown", "non-causal"]``.  Pass
            ``["causal", "unknown"]`` for binary expansion.
        node_policy:
            Controls how node subsets are generated:
            - ``"all-present"`` (default): all registry nodes are present
              in every model (backward-compatible).
            - ``"vary"``: enumerate/sample over non-empty node subsets.
        exposure, outcome:
            Optional causal target nodes. When supplied, every generated model
            must contain both nodes, including when ``node_policy="vary"``.
        """
        if edge_statuses is None:
            edge_statuses = list(VALID_EDGE_STATUSES)

        invalid = [s for s in edge_statuses if s not in VALID_EDGE_STATUSES]
        if invalid:
            raise StateError(
                f"Invalid edge status(es): {invalid}. "
                f"Choose from {list(VALID_EDGE_STATUSES)}."
            )

        if len(set(edge_statuses)) != len(edge_statuses):
            raise StateError("Duplicate values in edge_statuses are not allowed.")

        if not edge_statuses:
            raise StateError("edge_statuses must contain at least one status.")

        if node_policy not in ("all-present", "vary"):
            raise StateError(
                f"Unknown node_policy: {node_policy!r}. "
                f"Use 'all-present' or 'vary'."
            )

        df = registry.data
        node_comps = df[df["type"] == "node"]
        edge_comps = df[df["type"] == "edge"]

        node_names: dict[str, str] = {}
        for _, row in node_comps.iterrows():
            node_names[row["source"]] = row["comp_id"]

        fixed_causal_edge_ids: set[str] = set()
        if "fixed_status" in df.columns:
            fixed_rows = df[
                (df["type"] == "edge") & (df["fixed_status"] == "causal")
            ]
            fixed_causal_edge_ids = set(fixed_rows["comp_id"].tolist())

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
            src = edge_comps.set_index("comp_id").loc[cid, "source"]
            tgt = edge_comps.set_index("comp_id").loc[cid, "target"]
            if src in node_names:
                required_node_cids.add(node_names[src])
            if tgt in node_names:
                required_node_cids.add(node_names[tgt])

        edge_ids = edge_comps["comp_id"].tolist()
        edge_sources = edge_comps.set_index("comp_id")["source"].to_dict()
        edge_targets = edge_comps.set_index("comp_id")["target"].to_dict()
        edge_directions = edge_comps.set_index("comp_id")["direction"].to_dict()

        if mode == "exhaustive":
            records = ModelStateExpander._expand_exhaustive(
                registry=registry,
                node_names=node_names,
                node_timing=node_timing or {},
                edge_ids=edge_ids,
                edge_sources=edge_sources,
                edge_targets=edge_targets,
                edge_directions=edge_directions,
                max_models=max_models,
                edge_statuses=edge_statuses,
                node_policy=node_policy,
                required_node_cids=required_node_cids,
                fixed_causal_edge_ids=fixed_causal_edge_ids,
            )
        elif mode == "sampled":
            records = ModelStateExpander._expand_sampled(
                registry=registry,
                node_names=node_names,
                node_timing=node_timing or {},
                edge_ids=edge_ids,
                edge_sources=edge_sources,
                edge_targets=edge_targets,
                edge_directions=edge_directions,
                n_models=n_models or 100,
                seed=seed,
                max_models=max_models,
                edge_statuses=edge_statuses,
                node_policy=node_policy,
                required_node_cids=required_node_cids,
                fixed_causal_edge_ids=fixed_causal_edge_ids,
            )
        else:
            raise StateError(
                f"Unknown expansion mode: {mode!r}. "
                f"Use 'sampled' or 'exhaustive'. "
                f"To inject user theories, pass seed_claims alongside "
                f"either mode."
            )

        if seed_claims:
            records = ModelStateExpander._integrate_seeds(
                generated_records=records,
                seed_claims=seed_claims,
                registry=registry,
                node_timing=node_timing or {},
                required_node_cids=required_node_cids,
                fixed_causal_edge_ids=fixed_causal_edge_ids,
            )
        else:
            for r in records:
                r["seeded"] = False

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
        required_node_cids: set[str] | None = None,
        fixed_causal_edge_ids: set[str] | None = None,
    ) -> list[dict]:
        """Find or append seeded models, add 'seeded' flag, reorder to top.

        Uses sparse semantic vectors: present node set + applicable edge statuses.
        """
        if required_node_cids is None:
            required_node_cids = set()
        if fixed_causal_edge_ids is None:
            fixed_causal_edge_ids = set()

        all_comp_ids = sorted(registry.data["comp_id"].tolist())
        valid_comp_ids = set(all_comp_ids)
        valid_node_statuses = {"present", "absent", "causal", "unknown", "non-causal"}
        valid_edge_statuses = set(VALID_EDGE_STATUSES)

        component_types = registry.data.set_index("comp_id")["type"].to_dict()

        node_map: dict[str, str] = {}
        node_names_by_cid: dict[str, str] = {}
        for _, row in registry.data[registry.data["type"] == "node"].iterrows():
            node_map[row["source"]] = row["comp_id"]
            node_names_by_cid[row["comp_id"]] = row["source"]
        node_comp_ids = set(node_map.values())

        edge_endpoints: dict[str, tuple[str | None, str | None]] = {}
        edge_endpoint_names: dict[str, tuple[str, str]] = {}
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows():
            edge_endpoints[row["comp_id"]] = (
                node_map.get(row["source"]),
                node_map.get(row["target"]),
            )
            edge_endpoint_names[row["comp_id"]] = (row["source"], row["target"])

        for c in seed_claims:
            if c.get("comp_id") not in valid_comp_ids:
                raise StateError(
                    f"Unknown component ID in seed claim: {c.get('comp_id')!r}"
                )
            cid = c.get("comp_id")
            status = c.get("status")
            comp_type = component_types[cid]
            if comp_type == "node" and status not in valid_node_statuses:
                raise StateError(
                    f"Invalid node status in seed claim for {cid}: {status!r}"
                )
            if comp_type == "edge" and status not in valid_edge_statuses:
                raise StateError(
                    f"Invalid edge status in seed claim for {cid}: {status!r}"
                )

        for cid in fixed_causal_edge_ids:
            for c in seed_claims:
                if c.get("comp_id") == cid and c.get("status") in (
                    "unknown",
                    "non-causal",
                ):
                    raise StateError(
                        f"Seed claim sets fixed edge {cid} to {c.get('status')!r}, "
                        f"but this edge is fixed as causal in the registry"
                    )

        seed_models: dict[str, dict[str, str]] = {}
        seed_order: list[str] = []
        seed_timing: dict[str, dict[str, int | None]] = {}
        for c in seed_claims:
            mid = c["model_id"]
            if mid not in seed_models:
                seed_models[mid] = {}
                seed_timing[mid] = {}
                seed_order.append(mid)
            seed_models[mid][c["comp_id"]] = c["status"]
            if c.get("timing") is not None:
                seed_timing[mid][c["comp_id"]] = c["timing"]

        def _node_status_is_present(status: str) -> bool:
            # Legacy dense inputs used causal for node presence. Treat unknown
            # and non-causal as absent to keep old state-data ingestion explicit.
            return status in ("present", "causal")

        def _normalize_seed_model(mid: str) -> dict:
            claims = seed_models[mid]
            present_nodes: set[str] = set()
            explicit_absent_nodes: set[str] = set()
            edge_statuses: dict[str, str] = {}

            for cid, status in claims.items():
                comp_type = component_types[cid]
                if comp_type == "node":
                    if _node_status_is_present(status):
                        if cid in explicit_absent_nodes:
                            raise StateError(
                                f"Seed model {mid} marks node {cid} both present and absent"
                            )
                        present_nodes.add(cid)
                    else:
                        if cid in present_nodes:
                            raise StateError(
                                f"Seed model {mid} marks node {cid} both present and absent"
                            )
                        explicit_absent_nodes.add(cid)
                elif comp_type == "edge":
                    if cid in fixed_causal_edge_ids and status != "causal":
                        raise StateError(
                            f"Seed model {mid} sets fixed edge {cid} to {status!r}, "
                            f"but it is fixed as causal"
                        )
                    edge_statuses[cid] = status

            for edge_cid in edge_statuses:
                src_cid, tgt_cid = edge_endpoints[edge_cid]
                src_name, tgt_name = edge_endpoint_names[edge_cid]
                if src_cid is None or tgt_cid is None:
                    raise StateError(
                        f"Seed edge {edge_cid} references unknown endpoint "
                        f"{src_name!r} or {tgt_name!r}"
                    )
                for node_cid in (src_cid, tgt_cid):
                    if node_cid in explicit_absent_nodes:
                        raise StateError(
                            f"Seed model {mid} claims edge {edge_cid}, but endpoint "
                            f"node {node_cid} is explicitly absent"
                        )
                    present_nodes.add(node_cid)

            for fixed_cid in fixed_causal_edge_ids:
                if fixed_cid not in edge_statuses:
                    edge_statuses[fixed_cid] = "causal"
                fixed_src, fixed_tgt = edge_endpoints[fixed_cid]
                if fixed_src in explicit_absent_nodes or fixed_tgt in explicit_absent_nodes:
                    raise StateError(
                        f"Seed model {mid} omits endpoint(s) of fixed edge {fixed_cid}. "
                        f"Fixed edge endpoints cannot be absent."
                    )
                if fixed_src is not None:
                    present_nodes.add(fixed_src)
                if fixed_tgt is not None:
                    present_nodes.add(fixed_tgt)

            return {
                "present_nodes": present_nodes,
                "edge_statuses": edge_statuses,
                "timing": seed_timing[mid],
            }

        def _semantic_vector(
            present_nodes: set[str],
            statuses: dict[str, str],
        ) -> tuple:
            items: list[tuple[str, str]] = [
                (cid, "present") for cid in sorted(present_nodes)
            ]
            for edge_cid in sorted(edge_endpoints):
                src_cid, tgt_cid = edge_endpoints[edge_cid]
                if src_cid in present_nodes and tgt_cid in present_nodes:
                    items.append((edge_cid, statuses.get(edge_cid, "unknown")))
            return tuple(items)

        normalized_seeds: dict[str, dict] = {}
        seed_vectors: dict[str, tuple] = {}
        for mid in seed_order:
            normalized = _normalize_seed_model(mid)
            missing_required = required_node_cids - normalized["present_nodes"]
            if missing_required:
                missing = ", ".join(sorted(missing_required))
                raise StateError(
                    f"Seed model {mid} omits required exposure/outcome node(s): "
                    f"{missing}"
                )
            normalized_seeds[mid] = normalized
            seed_vectors[mid] = _semantic_vector(
                normalized["present_nodes"],
                normalized["edge_statuses"],
            )

        gen_by_model: dict[str, dict[str, str]] = {}
        for r in generated_records:
            mid = r["model_id"]
            gen_by_model.setdefault(mid, {})[r["comp_id"]] = r["status"]

        gen_vectors: dict[str, tuple] = {}
        for mid, statuses in gen_by_model.items():
            present_nodes = {
                cid
                for cid, status in statuses.items()
                if cid in node_comp_ids and _node_status_is_present(status)
            }
            gen_vectors[mid] = _semantic_vector(present_nodes, statuses)

        matched_gen_ids: set[str] = set()
        seed_to_gen: dict[str, str] = {}
        for seed_id, seed_vec in seed_vectors.items():
            for gen_id, gen_vec in gen_vectors.items():
                if gen_id in matched_gen_ids:
                    continue
                if seed_vec == gen_vec:
                    seed_to_gen[seed_id] = gen_id
                    matched_gen_ids.add(gen_id)
                    break

        result: list[dict] = []

        for seed_id in seed_order:
            if seed_id in seed_to_gen:
                gen_id = seed_to_gen[seed_id]
                for r in generated_records:
                    if r["model_id"] == gen_id:
                        result.append(
                            {
                                "model_id": seed_id,
                                "comp_id": r["comp_id"],
                                "status": r["status"],
                                "timing": r.get("timing"),
                                "seeded": True,
                            }
                        )
            else:
                normalized = normalized_seeds[seed_id]
                present_nodes = normalized["present_nodes"]
                edge_statuses = normalized["edge_statuses"]
                timing_claims = normalized["timing"]

                for cid in sorted(present_nodes):
                    timing_val = timing_claims.get(cid)
                    if timing_val is None:
                        node_name = node_names_by_cid.get(cid)
                        timing_val = node_timing.get(node_name) if node_name else None
                    result.append(
                        {
                            "model_id": seed_id,
                            "comp_id": cid,
                            "status": "present",
                            "timing": timing_val,
                            "seeded": True,
                        }
                    )

                for cid in sorted(edge_endpoints):
                    src_cid, tgt_cid = edge_endpoints[cid]
                    if src_cid in present_nodes and tgt_cid in present_nodes:
                        result.append(
                            {
                                "model_id": seed_id,
                                "comp_id": cid,
                                "status": edge_statuses.get(cid, "unknown"),
                                "timing": None,
                                "seeded": True,
                            }
                        )

        non_seeded_gen_ids: list[str] = []
        seen_non_seeded_gen_ids: set[str] = set()
        for r in generated_records:
            gen_id = r["model_id"]
            if gen_id in matched_gen_ids or gen_id in seen_non_seeded_gen_ids:
                continue
            seen_non_seeded_gen_ids.add(gen_id)
            non_seeded_gen_ids.append(gen_id)

        if non_seeded_gen_ids:
            width = max(4, len(str(len(seed_order) + len(non_seeded_gen_ids))))
            used_model_ids = set(seed_order)
            gen_id_to_new_id: dict[str, str] = {}
            next_model_num = len(seed_order) + 1

            for gen_id in non_seeded_gen_ids:
                while True:
                    new_mid = f"M{next_model_num:0{width}d}"
                    next_model_num += 1
                    if new_mid not in used_model_ids:
                        break
                used_model_ids.add(new_mid)
                gen_id_to_new_id[gen_id] = new_mid

            for r in generated_records:
                gen_id = r["model_id"]
                if gen_id in matched_gen_ids:
                    continue
                result.append(
                    {
                        "model_id": gen_id_to_new_id[gen_id],
                        "comp_id": r["comp_id"],
                        "status": r["status"],
                        "timing": r.get("timing"),
                        "seeded": False,
                    }
                )

        return result

    # ------------------------------------------------------------------
    # Exhaustive expansion
    # ------------------------------------------------------------------

    @staticmethod
    def _expand_exhaustive(
        *,
        registry: ComponentRegistry,
        node_names: dict[str, str],
        node_timing: dict[str, int],
        edge_ids: list[str],
        edge_sources: dict[str, str],
        edge_targets: dict[str, str],
        edge_directions: dict[str, str] | None = None,
        max_models: int,
        edge_statuses: list[str],
        node_policy: str = "all-present",
        required_node_cids: set[str] | None = None,
        fixed_causal_edge_ids: set[str] | None = None,
    ) -> list[dict]:
        if required_node_cids is None:
            required_node_cids = set()
        if fixed_causal_edge_ids is None:
            fixed_causal_edge_ids = set()
        if edge_directions is None:
            edge_directions = {}

        node_cids = list(node_names.values())

        temporally_valid: set[str] = set()
        for cid in edge_ids:
            if edge_directions.get(cid) == "<->":
                temporally_valid.add(cid)
                continue
            src = edge_sources[cid]
            tgt = edge_targets[cid]
            s_t = node_timing.get(src)
            t_t = node_timing.get(tgt)
            if s_t is None or t_t is None or s_t < t_t:
                temporally_valid.add(cid)

        if node_policy == "all-present":
            node_subsets = [set(node_cids)]
        else:
            all_subsets: list[set[str]] = []
            for r in range(1, len(node_cids) + 1):
                for combo in itertools.combinations(node_cids, r):
                    subset = set(combo)
                    if required_node_cids.issubset(subset):
                        all_subsets.append(subset)
            node_subsets = all_subsets

        E = len(edge_ids)
        mutable_E = E - len(fixed_causal_edge_ids)
        S = len(edge_statuses)

        if node_policy == "all-present":
            projected = S**mutable_E
        else:
            projected = 0
            for subset in node_subsets:
                applicable_count = 0
                for cid in edge_ids:
                    if cid in fixed_causal_edge_ids:
                        continue
                    src_name = edge_sources[cid]
                    tgt_name = edge_targets[cid]
                    src_cid = node_names.get(src_name)
                    tgt_cid = node_names.get(tgt_name)
                    if src_cid in subset and tgt_cid in subset:
                        applicable_count += 1
                projected += S**applicable_count if applicable_count > 0 else 1

        if projected > max_models:
            raise StateError(
                f"Exhaustive expansion projected {projected} models, "
                f"exceeding max_models={max_models}. "
                f"Use mode='sampled' or raise max_models."
            )

        model_id_fmt = f"M{{:0{max(4, len(str(projected)))}d}}"

        records: list[dict] = []
        model_counter = 0

        for subset in node_subsets:
            applicable_edges = []
            fixed_applicable_edges = []
            for cid in edge_ids:
                src_name = edge_sources[cid]
                tgt_name = edge_targets[cid]
                src_cid = node_names.get(src_name)
                tgt_cid = node_names.get(tgt_name)
                if src_cid in subset and tgt_cid in subset:
                    if cid in fixed_causal_edge_ids:
                        fixed_applicable_edges.append(cid)
                    else:
                        applicable_edges.append(cid)

            applicable_count = len(applicable_edges)
            combos = S**applicable_count if applicable_count > 0 else 1

            for combo_num in range(combos):
                edge_status: dict[str, str] = {}
                directed_edges: set[tuple[str, str]] = set()
                valid = True

                for cid in fixed_applicable_edges:
                    edge_status[cid] = "causal"
                    if edge_directions.get(cid) == "->":
                        src_name = edge_sources[cid]
                        tgt_name = edge_targets[cid]
                        directed_edges.add((src_name, tgt_name))

                for i, cid in enumerate(applicable_edges):
                    digit = (combo_num // (S**i)) % S
                    status = edge_statuses[digit]
                    if status == "causal":
                        if edge_directions.get(cid) == "->":
                            if cid not in temporally_valid:
                                valid = False
                                break
                            directed_edges.add(
                                (edge_sources[cid], edge_targets[cid])
                            )
                        edge_status[cid] = "causal"
                    else:
                        edge_status[cid] = status

                if not valid:
                    continue

                if _has_cycle(directed_edges):
                    continue

                model_counter += 1
                mid = model_id_fmt.format(model_counter)

                for cid in subset:
                    timing_val = None
                    comp_row = registry.data[registry.data["comp_id"] == cid]
                    if not comp_row.empty:
                        node_name = comp_row.iloc[0]["source"]
                        timing_val = node_timing.get(node_name) if node_timing else None

                    records.append(
                        {
                            "model_id": mid,
                            "comp_id": cid,
                            "status": "present",
                            "timing": timing_val,
                        }
                    )

                for cid in applicable_edges:
                    status = edge_status.get(cid, "unknown")
                    records.append(
                        {
                            "model_id": mid,
                            "comp_id": cid,
                            "status": status,
                            "timing": None,
                        }
                    )

                for cid in fixed_applicable_edges:
                    records.append(
                        {
                            "model_id": mid,
                            "comp_id": cid,
                            "status": "causal",
                            "timing": None,
                        }
                    )

        return records

    # ------------------------------------------------------------------
    # Sampled expansion
    # ------------------------------------------------------------------

    @staticmethod
    def _expand_sampled(
        *,
        registry: ComponentRegistry,
        node_names: dict[str, str],
        node_timing: dict[str, int],
        edge_ids: list[str],
        edge_sources: dict[str, str],
        edge_targets: dict[str, str],
        edge_directions: dict[str, str] | None = None,
        n_models: int,
        seed: int | None,
        max_models: int,
        edge_statuses: list[str],
        node_policy: str = "all-present",
        required_node_cids: set[str] | None = None,
        fixed_causal_edge_ids: set[str] | None = None,
    ) -> list[dict]:
        if required_node_cids is None:
            required_node_cids = set()
        if fixed_causal_edge_ids is None:
            fixed_causal_edge_ids = set()
        if edge_directions is None:
            edge_directions = {}

        node_cids = list(node_names.values())

        temporally_valid: set[str] = set()
        for cid in edge_ids:
            if edge_directions.get(cid) == "<->":
                temporally_valid.add(cid)
                continue
            src = edge_sources[cid]
            tgt = edge_targets[cid]
            s_t = node_timing.get(src)
            t_t = node_timing.get(tgt)
            if s_t is None or t_t is None or s_t < t_t:
                temporally_valid.add(cid)

        if node_policy == "all-present":
            node_subsets = [set(node_cids)]
        else:
            all_subsets: list[set[str]] = []
            for r in range(1, len(node_cids) + 1):
                for combo in itertools.combinations(node_cids, r):
                    subset = set(combo)
                    if required_node_cids.issubset(subset):
                        all_subsets.append(subset)
            node_subsets = all_subsets

        S = len(edge_statuses)

        rng = random.Random(seed)
        model_id_fmt = f"M{{:0{max(4, len(str(n_models)))}d}}"

        records: list[dict] = []
        model_counter = 0
        seen_keys: set[tuple] = set()
        attempts = 0
        max_attempts = n_models * 50

        while model_counter < n_models and attempts < max_attempts:
            attempts += 1

            subset = rng.choice(node_subsets)

            applicable_edges = []
            fixed_applicable_edges = []
            for cid in edge_ids:
                src_name = edge_sources[cid]
                tgt_name = edge_targets[cid]
                src_cid = node_names.get(src_name)
                tgt_cid = node_names.get(tgt_name)
                if src_cid in subset and tgt_cid in subset:
                    if cid in fixed_causal_edge_ids:
                        fixed_applicable_edges.append(cid)
                    else:
                        applicable_edges.append(cid)

            applicable_count = len(applicable_edges)
            combos = S**applicable_count if applicable_count > 0 else 1
            if combos == 0:
                continue

            combo = rng.randint(0, combos - 1)
            sort_key = (frozenset(subset), combo)
            if sort_key in seen_keys:
                continue
            seen_keys.add(sort_key)

            edge_status: dict[str, str] = {}
            directed_edges: set[tuple[str, str]] = set()
            valid = True

            for cid in fixed_applicable_edges:
                edge_status[cid] = "causal"
                if edge_directions.get(cid) == "->":
                    src_name = edge_sources[cid]
                    tgt_name = edge_targets[cid]
                    directed_edges.add((src_name, tgt_name))

            for i, cid in enumerate(applicable_edges):
                digit = (combo // (S**i)) % S
                status = edge_statuses[digit]
                if status == "causal":
                    if edge_directions.get(cid) == "->":
                        if cid not in temporally_valid:
                            valid = False
                            break
                        directed_edges.add(
                            (edge_sources[cid], edge_targets[cid])
                        )
                    edge_status[cid] = "causal"
                else:
                    edge_status[cid] = status

            if not valid:
                continue

            if _has_cycle(directed_edges):
                continue

            model_counter += 1
            mid = model_id_fmt.format(model_counter)

            for cid in subset:
                timing_val = None
                comp_row = registry.data[registry.data["comp_id"] == cid]
                if not comp_row.empty:
                    node_name = comp_row.iloc[0]["source"]
                    timing_val = node_timing.get(node_name) if node_timing else None

                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": "present",
                        "timing": timing_val,
                    }
                )

            for cid in applicable_edges:
                status = edge_status.get(cid, "unknown")
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": status,
                        "timing": None,
                    }
                )

            for cid in fixed_applicable_edges:
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": "causal",
                        "timing": None,
                    }
                )

        if model_counter < n_models:
            warnings.warn(
                f"Only generated {model_counter} models; "
                f"requested {n_models}. Not enough valid model combinations exist."
            )

        return records

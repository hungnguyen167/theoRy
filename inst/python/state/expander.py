from __future__ import annotations

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
        mode: str = "seeded",
        seed_claims: list[dict] | None = None,
        node_timing: dict[str, int] | None = None,
        max_models: int = 10_000,
        n_models: int | None = None,
        seed: int | None = None,
        edge_statuses: list[str] | None = None,
    ) -> list[dict]:
        """Expand registry components into model-state records.

        Parameters
        ----------
        registry:
            The component registry defining the universe of components.
        mode:
            ``seeded``, ``exhaustive``, or ``sampled``.
        seed_claims:
            Required for ``seeded`` mode.  List of ``{model_id, comp_id,
            status, timing}`` dicts.
        node_timing:
            Optional mapping of ``node_name -> timing_int`` used for temporal
            validation in exhaustive / sampled modes.
        max_models:
            Safety cap for exhaustive mode.  Expansion fails fast if the
            projected count exceeds this value.
        n_models:
            Number of models to sample in ``sampled`` mode (required).
        seed:
            Random seed for reproducible ``sampled`` expansion.
        edge_statuses:
            Statuses to enumerate/sample over for edge components in
            exhaustive and sampled modes.  Defaults to
            ``["causal", "unknown", "non-causal"]``.  Pass
            ``["causal", "unknown"]`` for binary expansion.
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

        df = registry.data
        node_comps = df[df["type"] == "node"]
        edge_comps = df[df["type"] == "edge"]

        node_names: dict[str, str] = {}  # source_name -> comp_id
        for _, row in node_comps.iterrows():
            node_names[row["source"]] = row["comp_id"]

        edge_ids = edge_comps["comp_id"].tolist()
        edge_sources = edge_comps.set_index("comp_id")["source"].to_dict()
        edge_targets = edge_comps.set_index("comp_id")["target"].to_dict()

        if mode == "seeded":
            return ModelStateExpander._expand_seeded(
                registry=registry,
                seed_claims=seed_claims,
                node_names=node_names,
                edge_ids=edge_ids,
            )

        if mode == "exhaustive":
            return ModelStateExpander._expand_exhaustive(
                registry=registry,
                node_names=node_names,
                node_timing=node_timing or {},
                edge_ids=edge_ids,
                edge_sources=edge_sources,
                edge_targets=edge_targets,
                max_models=max_models,
                edge_statuses=edge_statuses,
            )

        if mode == "sampled":
            return ModelStateExpander._expand_sampled(
                registry=registry,
                node_names=node_names,
                node_timing=node_timing or {},
                edge_ids=edge_ids,
                edge_sources=edge_sources,
                edge_targets=edge_targets,
                n_models=n_models or 100,
                seed=seed,
                max_models=max_models,
                edge_statuses=edge_statuses,
            )

        raise StateError(f"Unknown expansion mode: {mode!r}")

    # ------------------------------------------------------------------

    @staticmethod
    def _expand_seeded(
        *,
        registry: ComponentRegistry,
        seed_claims: list[dict] | None,
        node_names: dict[str, str],
        edge_ids: list[str],
    ) -> list[dict]:
        if not seed_claims:
            raise StateError("seed_claims is required for seeded mode")

        all_comp_ids = sorted(registry.data["comp_id"].tolist())
        valid_comp_ids = set(all_comp_ids)
        valid_statuses = {"causal", "unknown", "non-causal"}

        for c in seed_claims:
            if c.get("comp_id") not in valid_comp_ids:
                raise StateError(
                    f"Unknown component ID in seed claim: {c.get('comp_id')!r}"
                )
            if c.get("status") not in valid_statuses:
                raise StateError(
                    f"Invalid status in seed claim: {c.get('status')!r}"
                )
            t = c.get("timing")
            if t is not None and not isinstance(t, int):
                raise StateError(f"Timing must be integer, got {type(t).__name__}")

        model_ids = sorted({c["model_id"] for c in seed_claims})
        all_comp_ids = sorted(registry.data["comp_id"].tolist())

        records: list[dict] = []
        for mid in model_ids:
            claims = {c["comp_id"]: c for c in seed_claims if c["model_id"] == mid}
            for cid in all_comp_ids:
                if cid in claims:
                    rec = claims[cid]
                    records.append(
                        {
                            "model_id": mid,
                            "comp_id": cid,
                            "status": rec.get("status", "unknown"),
                            "timing": rec.get("timing"),
                        }
                    )
                else:
                    records.append(
                        {
                            "model_id": mid,
                            "comp_id": cid,
                            "status": "unknown",
                            "timing": None,
                        }
                    )

        return records

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
        max_models: int,
        edge_statuses: list[str],
    ) -> list[dict]:
        all_comp_ids = sorted(registry.data["comp_id"].tolist())
        node_cids = list(node_names.values())

        # Determine which edges are temporally valid (can be "causal")
        temporally_valid: set[str] = set()
        for cid in edge_ids:
            src = edge_sources[cid]
            tgt = edge_targets[cid]
            s_t = node_timing.get(src)
            t_t = node_timing.get(tgt)
            if s_t is None or t_t is None or s_t < t_t:
                temporally_valid.add(cid)

        E = len(edge_ids)
        S = len(edge_statuses)
        projected = S**E
        if projected > max_models:
            raise StateError(
                f"Exhaustive expansion projected {projected} models, "
                f"exceeding max_models={max_models}. "
                f"Use mode='sampled' or raise max_models."
            )

        model_id_fmt = f"M{{:0{max(4, len(str(projected)))}d}}"

        records: list[dict] = []
        model_counter = 0

        for combo_num in range(projected):
            edge_status: dict[str, str] = {}
            directed_edges: set[tuple[str, str]] = set()
            valid = True

            for i, cid in enumerate(edge_ids):
                digit = (combo_num // (S**i)) % S
                status = edge_statuses[digit]
                if status == "causal":
                    if cid not in temporally_valid:
                        valid = False
                        break
                    edge_status[cid] = "causal"
                    src_name = edge_sources[cid]
                    tgt_name = edge_targets[cid]
                    directed_edges.add((src_name, tgt_name))
                else:
                    edge_status[cid] = status

            if not valid:
                continue

            if _has_cycle(directed_edges):
                continue

            model_counter += 1
            mid = model_id_fmt.format(model_counter)

            for cid in all_comp_ids:
                if cid in node_cids:
                    status = "causal"
                elif cid in edge_status:
                    status = edge_status[cid]
                else:
                    status = "unknown"

                timing_val = None
                comp_row = registry.data[registry.data["comp_id"] == cid]
                if not comp_row.empty and comp_row.iloc[0]["type"] == "node":
                    node_name = comp_row.iloc[0]["source"]
                    timing_val = node_timing.get(node_name) if node_timing else None

                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": status,
                        "timing": timing_val,
                    }
                )

        return records

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
        n_models: int,
        seed: int | None,
        max_models: int,
        edge_statuses: list[str],
    ) -> list[dict]:
        all_comp_ids = sorted(registry.data["comp_id"].tolist())
        node_cids = list(node_names.values())

        temporally_valid: set[str] = set()
        for cid in edge_ids:
            src = edge_sources[cid]
            tgt = edge_targets[cid]
            s_t = node_timing.get(src)
            t_t = node_timing.get(tgt)
            if s_t is None or t_t is None or s_t < t_t:
                temporally_valid.add(cid)

        E = len(edge_ids)
        S = len(edge_statuses)
        projected = S**E

        rng = random.Random(seed)
        model_id_fmt = f"M{{:0{max(4, len(str(n_models)))}d}}"

        records: list[dict] = []
        model_counter = 0
        seen_combos: set[int] = set()
        attempts = 0
        max_attempts = n_models * 50

        while model_counter < n_models and attempts < max_attempts:
            attempts += 1
            combo = rng.randint(0, projected - 1)
            if combo in seen_combos:
                continue
            seen_combos.add(combo)

            edge_status: dict[str, str] = {}
            directed_edges: set[tuple[str, str]] = set()
            valid = True

            for i, cid in enumerate(edge_ids):
                digit = (combo // (S**i)) % S
                status = edge_statuses[digit]
                if status == "causal":
                    if cid not in temporally_valid:
                        valid = False
                        break
                    edge_status[cid] = "causal"
                    directed_edges.add((edge_sources[cid], edge_targets[cid]))
                else:
                    edge_status[cid] = status

            if not valid:
                continue

            if _has_cycle(directed_edges):
                continue

            model_counter += 1
            mid = model_id_fmt.format(model_counter)

            for cid in all_comp_ids:
                if cid in node_cids:
                    status = "causal"
                elif cid in edge_status:
                    status = edge_status[cid]
                else:
                    status = "unknown"

                timing_val = None
                comp_row = registry.data[registry.data["comp_id"] == cid]
                if not comp_row.empty and comp_row.iloc[0]["type"] == "node":
                    node_name = comp_row.iloc[0]["source"]
                    timing_val = node_timing.get(node_name) if node_timing else None

                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": status,
                        "timing": timing_val,
                    }
                )

        if model_counter < n_models:
            warnings.warn(
                f"Only generated {model_counter} models; "
                f"requested {n_models}. Not enough valid model combinations exist."
            )

        return records

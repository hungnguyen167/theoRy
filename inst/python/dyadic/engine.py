from __future__ import annotations

import logging
from collections import OrderedDict

from dyadic.causal import CausalError
from registry.schema import ComponentRegistry
from state.tensor import StateTensor

logger = logging.getLogger(__name__)


class DyadicError(Exception):
    pass


class DyadicEngine:
    """Compute dyadic comparisons between model states."""

    _VALID_MODES = frozenset({"basic", "full"})

    def __init__(self):
        self._causal_cache: dict[tuple[str, str, str, int], tuple] = {}
        self._temporal_cache: dict[tuple[str, int], set[str]] = {}
        self._acyclic_cache: dict[tuple[str, int], bool] = {}
        self._node_comp_cache: dict[int, dict[str, str]] = {}

    def compare(
        self,
        ego_id: str,
        alter_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> dict:
        """Compare two models as a directed dyad (ego -> alter)."""
        if mode not in self._VALID_MODES:
            raise DyadicError("Mode must be one of: basic, full")

        self._validate_acyclic(ego_id, state, registry)
        self._validate_acyclic(alter_id, state, registry)

        component_ids = state.component_ids
        excluded_edges = self._temporally_invalid_edges(
            ego_id, state, registry
        ) | self._temporally_invalid_edges(alter_id, state, registry)

        shared_known = 0
        union_known = 0
        conflicts: list[str] = []
        repair_cost = 0

        for cid in component_ids:
            if cid in excluded_edges:
                continue

            a_status = state.get_status(ego_id, cid)
            b_status = state.get_status(alter_id, cid)

            a_known = a_status == "causal"
            b_known = b_status == "causal"

            if a_known and b_known:
                shared_known += 1
                union_known += 1
            elif a_known or b_known:
                union_known += 1

            if (a_status == "causal" and b_status == "non-causal") or (
                a_status == "non-causal" and b_status == "causal"
            ):
                conflicts.append(cid)

            if a_status != b_status:
                repair_cost += 1

        if union_known == 0:
            similarity_rate = 1.0
        else:
            similarity_rate = shared_known / union_known

        timing_compatible = self._check_timing_compatibility(
            ego_id, alter_id, state, registry
        )

        dyad_id = f"{ego_id}__{alter_id}"

        result = OrderedDict()
        result["dyad_id"] = dyad_id
        result["ego_id"] = ego_id
        result["alter_id"] = alter_id
        result["similarity_rate"] = round(similarity_rate, 6)
        result["timing_compatible"] = timing_compatible
        result["existence_conflict"] = len(conflicts) > 0
        result["conflicting_components"] = conflicts
        result["repair_cost"] = repair_cost
        result["excluded_components"] = sorted(excluded_edges)

        if mode == "full":
            if causal_wrapper is None:
                raise DyadicError(
                    "causal_wrapper is required for mode='full'"
                )
            causal = self._compute_causal_metrics(
                ego_id, alter_id, state, registry, causal_wrapper,
                exposure=exposure, outcome=outcome,
            )
            result.update(causal)

        return dict(result)

    def compare_pairs(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Return one dyad record per directed non-self model pair."""
        if model_ids is None:
            model_ids = state.model_ids
        model_ids = sorted(model_ids)

        results: list[dict] = []
        for i in range(len(model_ids)):
            for j in range(len(model_ids)):
                if i == j:
                    continue
                results.append(
                    self.compare(
                        model_ids[i],
                        model_ids[j],
                        state,
                        registry,
                        mode=mode,
                        causal_wrapper=causal_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                    )
                )
        return results

    def compare_pairs_subset(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        affected_models: list[str],
        all_models: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Compute dyads only for pairs involving affected_models.
        
        Returns dyads where at least one model is in affected_models.
        This is O(K*M) instead of O(M^2) where K=len(affected_models).
        """
        if all_models is None:
            all_models = state.model_ids
        all_models = sorted(all_models)
        affected_set = set(affected_models)

        results: list[dict] = []
        for ego in all_models:
            for alter in all_models:
                if ego == alter:
                    continue
                if ego not in affected_set and alter not in affected_set:
                    continue
                results.append(
                    self.compare(
                        ego,
                        alter,
                        state,
                        registry,
                        mode=mode,
                        causal_wrapper=causal_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                    )
                )
        return results

    def compare_all(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Return ordered, non-self dyad records (convenience wrapper)."""
        if model_ids is None:
            model_ids = state.model_ids
        results: list[dict] = []
        for a in model_ids:
            for b in model_ids:
                if a == b:
                    continue
                results.append(
                    self.compare(
                        a,
                        b,
                        state,
                        registry,
                        mode=mode,
                        causal_wrapper=causal_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                    )
                )
        return results

    def compare_matrix(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[list[dict]]:
        """Return an M x M dyadic comparison matrix including self-dyads.
        Kept for internal testing; not used by the API endpoint."""
        if model_ids is None:
            model_ids = state.model_ids
        return [
            [
                self.compare(
                    a, b, state, registry, mode=mode, causal_wrapper=causal_wrapper,
                    exposure=exposure, outcome=outcome,
                )
                for b in model_ids
            ]
            for a in model_ids
        ]

    # ------------------------------------------------------------------
    # causal helpers (Story 2.2)
    # ------------------------------------------------------------------

    @staticmethod
    def _default_exposure_outcome(nodes: list[str]) -> tuple[str, str]:
        if len(nodes) < 2:
            raise DyadicError("At least two nodes required for causal metrics")
        return nodes[0], nodes[-1]

    def _dag_spec_for_model(
        self,
        model_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
        *,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> dict:
        df = registry.data
        nodes = df[df["type"] == "node"]
        node_names: list[str] = []
        for _, row in nodes.iterrows():
            node_names.append(row["source"])
        node_names = list(dict.fromkeys(node_names))

        if len(node_names) < 2:
            raise DyadicError(
                f"Model {model_id} has fewer than 2 nodes; "
                f"cannot build DAG spec for causal metrics"
            )

        edges: list[tuple[str, str]] = []
        directed_edges = df[
            (df["type"] == "edge") & (df["direction"] == "->")
        ]
        for _, row in directed_edges.iterrows():
            cid = row["comp_id"]
            if state.get_status(model_id, cid) == "causal":
                edges.append((row["source"], row["target"]))

        if (exposure is None) != (outcome is None):
            raise DyadicError(
                "Both or neither of exposure and outcome must be provided"
            )

        if exposure is not None and outcome is not None:
            if exposure not in node_names:
                raise DyadicError(
                    f"Exposure {exposure!r} is not a valid node in the registry. "
                    f"Valid nodes: {node_names}"
                )
            if outcome not in node_names:
                raise DyadicError(
                    f"Outcome {outcome!r} is not a valid node in the registry. "
                    f"Valid nodes: {node_names}"
                )
            if exposure == outcome:
                raise DyadicError(
                    f"Exposure and outcome must be distinct nodes, "
                    f"got both {exposure!r}"
                )
        else:
            exposure, outcome = self._default_exposure_outcome(node_names)

        return {
            "nodes": node_names,
            "edges": edges,
            "exposure": exposure,
            "outcome": outcome,
        }

    def _compute_causal_metrics(
        self,
        model_a: str,
        model_b: str,
        state: StateTensor,
        registry: ComponentRegistry,
        causal_wrapper,
        *,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> dict:
        def compute_for(model_id: str):
            exp = exposure or ""
            out = outcome or ""
            state_hash = state.hash_model(model_id)
            cache_key = (model_id, exp, out, state_hash)
            if cache_key in self._causal_cache:
                return self._causal_cache[cache_key]

            dag_spec = self._dag_spec_for_model(
                model_id, state, registry,
                exposure=exposure, outcome=outcome,
            )
            try:
                mas = causal_wrapper.compute_adjustment_sets(dag_spec)
                identified = len(mas) > 0
                result = (mas, identified, dag_spec)
                self._causal_cache[cache_key] = result
                return result
            except CausalError as e:
                message = str(e).lower()
                if "cycle" in message or "cyclic" in message:
                    logger.warning(
                        "Model %s has cyclic DAG - causal metrics skipped", model_id
                    )
                    result = (None, None, None)
                    self._causal_cache[cache_key] = result
                    return result
                raise

        mas_a, full_model_a, dag_spec_a = compute_for(model_a)
        mas_b, full_model_b, dag_spec_b = compute_for(model_b)

        if mas_a is not None and mas_b is not None:
            mas_result = causal_wrapper.compare_mas(mas_a, mas_b)
            mas_compatible = mas_result["compatible"]
        else:
            mas_compatible = None

        full_compatible = None
        if full_model_a is not None and full_model_b is not None:
            same_ident = bool(full_model_a) == bool(full_model_b)

            active_a = self._active_nodes(dag_spec_a)
            active_b = self._active_nodes(dag_spec_b)

            if active_a == active_b:
                active_compatible = True
            else:
                diff_a = active_a - active_b
                diff_b = active_b - active_a

                exp_a = dag_spec_a["exposure"]
                out_a = dag_spec_a["outcome"]
                exp_b = dag_spec_b["exposure"]
                out_b = dag_spec_b["outcome"]

                ignorable_a = (
                    all(
                        self._is_ignorable_difference(
                            node, model_a, exp_a, out_a, registry, state,
                        )
                        for node in diff_a
                    )
                    if diff_a
                    else True
                )

                ignorable_b = (
                    all(
                        self._is_ignorable_difference(
                            node, model_b, exp_b, out_b, registry, state,
                        )
                        for node in diff_b
                    )
                    if diff_b
                    else True
                )

                active_compatible = ignorable_a and ignorable_b

            full_compatible = same_ident and active_compatible

        return {
            "mas_ego": mas_a,
            "mas_alter": mas_b,
            "mas_compatible": mas_compatible,
            "full_model_ego": full_model_a,
            "full_model_alter": full_model_b,
            "full_compatible": full_compatible,
        }

    @staticmethod
    def _active_nodes(dag_spec: dict) -> set[str]:
        nodes: set[str] = set()
        for src, tgt in dag_spec.get("edges", []):
            nodes.add(src)
            nodes.add(tgt)
        return nodes

    def _is_ignorable_difference(
        self,
        node_name: str,
        model_id: str,
        exposure_name: str,
        outcome_name: str,
        registry: ComponentRegistry,
        state: StateTensor,
    ) -> bool:
        node_cid = self._node_component_id(node_name, registry)
        exp_cid = self._node_component_id(exposure_name, registry)
        out_cid = self._node_component_id(outcome_name, registry)

        node_t = state.get_timing(model_id, node_cid)
        exp_t = state.get_timing(model_id, exp_cid)
        out_t = state.get_timing(model_id, out_cid)

        if node_t is None or exp_t is None or out_t is None:
            return False
        return exp_t < node_t < out_t

    # ------------------------------------------------------------------
    # internal helpers
    # ------------------------------------------------------------------

    def _node_component_id(
        self,
        node_name: str,
        registry: ComponentRegistry,
    ) -> str:
        registry_id = id(registry)
        if registry_id not in self._node_comp_cache:
            df = registry.data
            node_comps = df[df["type"] == "node"]
            self._node_comp_cache[registry_id] = {
                row["source"]: row["comp_id"]
                for _, row in node_comps.iterrows()
            }

        cache = self._node_comp_cache[registry_id]
        if node_name not in cache:
            raise DyadicError(
                f"Ambiguous mapping: cannot find unique node component for "
                f"source={node_name!r}"
            )
        return cache[node_name]

    def _temporally_invalid_edges(
        self,
        model_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> set[str]:
        state_hash = state.hash_model(model_id, include_timing=True)
        cache_key = (model_id, state_hash)
        if cache_key in self._temporal_cache:
            return self._temporal_cache[cache_key]

        invalid: set[str] = set()
        df = registry.data

        for _, row in df[df["type"] == "edge"].iterrows():
            cid = row["comp_id"]
            if state.get_status(model_id, cid) != "causal":
                continue

            source_cid = self._node_component_id(row["source"], registry)
            target_cid = self._node_component_id(row["target"], registry)
            source_t = state.get_timing(model_id, source_cid)
            target_t = state.get_timing(model_id, target_cid)
            if source_t is not None and target_t is not None and source_t >= target_t:
                invalid.add(cid)

        self._temporal_cache[cache_key] = invalid
        return invalid

    def _validate_acyclic(
        self,
        model_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> None:
        state_hash = state.hash_model(model_id)
        cache_key = (model_id, state_hash)
        if cache_key in self._acyclic_cache:
            if not self._acyclic_cache[cache_key]:
                raise DyadicError(f"Invalid DAG: cycle detected for model {model_id}")
            return

        df = registry.data
        graph: dict[str, list[str]] = {}

        for _, row in df[df["type"] == "node"].iterrows():
            graph.setdefault(row["source"], [])

        directed_edges = df[(df["type"] == "edge") & (df["direction"] == "->")]
        for _, row in directed_edges.iterrows():
            cid = row["comp_id"]
            if state.get_status(model_id, cid) != "causal":
                continue
            graph.setdefault(row["source"], []).append(row["target"])
            graph.setdefault(row["target"], [])

        visiting: set[str] = set()
        visited: set[str] = set()

        def visit(node: str) -> bool:
            if node in visiting:
                return True
            if node in visited:
                return False
            visiting.add(node)
            for nxt in graph.get(node, []):
                if visit(nxt):
                    return True
            visiting.remove(node)
            visited.add(node)
            return False

        if any(visit(node) for node in list(graph)):
            self._acyclic_cache[cache_key] = False
            raise DyadicError(f"Invalid DAG: cycle detected for model {model_id}")

        self._acyclic_cache[cache_key] = True

    def _check_timing_compatibility(
        self,
        model_a: str,
        model_b: str,
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> bool:
        component_ids = state.component_ids
        df = registry.data

        if self._temporally_invalid_edges(
            model_a, state, registry
        ) or self._temporally_invalid_edges(model_b, state, registry):
            return False

        for cid in component_ids:
            comp_row = df[df["comp_id"] == cid]
            if comp_row.empty:
                continue
            comp_type = comp_row["type"].values[0]

            if comp_type == "node":
                a_status = state.get_status(model_a, cid)
                b_status = state.get_status(model_b, cid)
                if a_status == "causal" and b_status == "causal":
                    a_timing = state.get_timing(model_a, cid)
                    b_timing = state.get_timing(model_b, cid)
                    if (
                        a_timing is not None
                        and b_timing is not None
                        and a_timing != b_timing
                    ):
                        return False

            elif comp_type == "edge":
                a_status = state.get_status(model_a, cid)
                b_status = state.get_status(model_b, cid)
                if a_status == "causal" and b_status == "causal":
                    source = comp_row["source"].values[0]
                    target = comp_row["target"].values[0]
                    source_cid = self._node_component_id(source, registry)
                    target_cid = self._node_component_id(target, registry)

                    a_source_t = state.get_timing(model_a, source_cid)
                    a_target_t = state.get_timing(model_a, target_cid)
                    b_source_t = state.get_timing(model_b, source_cid)
                    b_target_t = state.get_timing(model_b, target_cid)

                    if (
                        a_source_t is not None
                        and a_target_t is not None
                        and a_source_t >= a_target_t
                    ) or (
                        b_source_t is not None
                        and b_target_t is not None
                        and b_source_t >= b_target_t
                    ):
                        return False

        return True

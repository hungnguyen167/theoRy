from __future__ import annotations

import logging
from collections import OrderedDict

from dyadic.profiles import CausalProfileBuilder
from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from state.semantics import (
    compare_structural_claims,
    edge_applicable,
    edge_endpoint_components,
    node_component_map,
)

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
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
        _causal_profiles: dict | None = None,
    ) -> dict:
        """Compare two models as a directed dyad (ego -> alter)."""
        if mode not in self._VALID_MODES:
            raise DyadicError("Mode must be one of: basic, full")

        if mode == "full" and exposure is not None and outcome is not None:
            self.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=[ego_id, alter_id],
            )

        self._validate_acyclic(ego_id, state, registry)
        self._validate_acyclic(alter_id, state, registry)

        counts = compare_structural_claims(state, registry, ego_id, alter_id)

        if counts.union_claims == 0:
            similarity_rate = 1.0
        else:
            similarity_rate = counts.shared_claims / counts.union_claims

        timing_compatible = self._check_timing_compatibility(
            ego_id, alter_id, state, registry
        )

        conflicts: list[str] = []
        if counts.edge_conflicts > 0:
            edge_endpoints = edge_endpoint_components(registry)
            for edge_cid in edge_endpoints:
                if not edge_applicable(state, ego_id, edge_cid, registry):
                    continue
                if not edge_applicable(state, alter_id, edge_cid, registry):
                    continue
                ego_status = state.get_status(ego_id, edge_cid)
                alter_status = state.get_status(alter_id, edge_cid)
                if (ego_status == "causal" and alter_status == "non-causal") or (
                    ego_status == "non-causal" and alter_status == "causal"
                ):
                    conflicts.append(edge_cid)

        # Also count node presence conflicts
        node_map = node_component_map(registry)
        for node_name, node_cid in node_map.items():
            ego_present = self._node_is_present(state, ego_id, node_cid)
            alter_present = self._node_is_present(state, alter_id, node_cid)
            if ego_present != alter_present:
                conflicts.append(node_cid)

        dyad_id = f"{ego_id}__{alter_id}"

        result = OrderedDict()
        result["dyad_id"] = dyad_id
        result["ego_id"] = ego_id
        result["alter_id"] = alter_id
        result["similarity_rate"] = round(similarity_rate, 6)
        result["timing_compatible"] = timing_compatible
        result["existence_conflict"] = len(conflicts) > 0
        result["conflicting_components"] = conflicts
        result["repair_cost"] = counts.repair_cost
        result["excluded_components"] = sorted(
            self._get_inapplicable_components(state, ego_id, alter_id, registry)
        )

        if counts.node_conflicts > 0:
            result["node_conflicts"] = counts.node_conflicts
        if counts.edge_conflicts > 0:
            result["edge_conflicts"] = counts.edge_conflicts
        if counts.inapplicable_components > 0:
            result["inapplicable_components"] = counts.inapplicable_components
        result["shared_resolved_claims"] = counts.shared_claims
        result["union_resolved_claims"] = counts.union_claims

        if mode == "full":
            if causal_wrapper is None:
                raise DyadicError("causal_wrapper is required for mode='full'")
            causal = self._compute_causal_metrics(
                ego_id,
                alter_id,
                state,
                registry,
                causal_wrapper,
                exposure=exposure,
                outcome=outcome,
                identification_wrapper=identification_wrapper,
                profiles=_causal_profiles,
            )
            result.update(causal)

        return dict(result)

    def _node_is_present(
        self, state: StateTensor, model_id: str, node_cid: str
    ) -> bool:
        if hasattr(state, "node_present"):
            return state.node_present(model_id, node_cid)
        status = state.get_status(model_id, node_cid)
        return status in ("causal", "present")

    def _get_inapplicable_components(
        self,
        state: StateTensor,
        ego_id: str,
        alter_id: str,
        registry: ComponentRegistry,
    ) -> set[str]:
        inapplicable: set[str] = set()
        edge_endpoints = edge_endpoint_components(registry)
        for edge_cid in edge_endpoints:
            ego_app = edge_applicable(state, ego_id, edge_cid, registry)
            alter_app = edge_applicable(state, alter_id, edge_cid, registry)
            if not ego_app or not alter_app:
                inapplicable.add(edge_cid)
        return inapplicable

    def compare_pairs(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
        *,
        mode: str = "basic",
        causal_wrapper=None,
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Return one dyad record per directed non-self model pair."""
        if model_ids is None:
            model_ids = state.model_ids
        model_ids = sorted(model_ids)

        if mode == "full" and exposure is not None and outcome is not None:
            self.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=model_ids,
            )

        profiles = self._build_causal_profiles(
            state,
            registry,
            model_ids=model_ids,
            mode=mode,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
        )

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
                        identification_wrapper=identification_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                        _causal_profiles=profiles,
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
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Compute dyads only for pairs involving affected_models."""
        if all_models is None:
            all_models = state.model_ids
        all_models = sorted(all_models)
        affected_set = set(affected_models)

        if mode == "full" and exposure is not None and outcome is not None:
            self.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=all_models,
            )

        profiles = self._build_causal_profiles(
            state,
            registry,
            model_ids=all_models,
            mode=mode,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
        )

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
                        identification_wrapper=identification_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                        _causal_profiles=profiles,
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
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[dict]:
        """Return ordered, non-self dyad records (convenience wrapper)."""
        if model_ids is None:
            model_ids = state.model_ids
        if mode == "full" and exposure is not None and outcome is not None:
            self.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=model_ids,
            )
        profiles = self._build_causal_profiles(
            state,
            registry,
            model_ids=model_ids,
            mode=mode,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
        )
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
                        identification_wrapper=identification_wrapper,
                        exposure=exposure,
                        outcome=outcome,
                        _causal_profiles=profiles,
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
        identification_wrapper=None,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> list[list[dict]]:
        """Return an M x M dyadic comparison matrix including self-dyads."""
        if model_ids is None:
            model_ids = state.model_ids
        if mode == "full" and exposure is not None and outcome is not None:
            self.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=model_ids,
            )
        profiles = self._build_causal_profiles(
            state,
            registry,
            model_ids=model_ids,
            mode=mode,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
        )
        return [
            [
                self.compare(
                    a,
                    b,
                    state,
                    registry,
                    mode=mode,
                    causal_wrapper=causal_wrapper,
                    identification_wrapper=identification_wrapper,
                    exposure=exposure,
                    outcome=outcome,
                    _causal_profiles=profiles,
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
        nodes_df = df[df["type"] == "node"]
        node_names: list[str] = []
        observed_by_node: dict[str, bool] = {}
        for _, row in nodes_df.iterrows():
            cid = row["comp_id"]
            if self._node_is_present(state, model_id, cid):
                node_names.append(row["source"])
                observed_by_node[row["source"]] = bool(row.get("observed", True))
        node_names = list(dict.fromkeys(node_names))

        if len(node_names) < 2:
            raise DyadicError(
                f"Model {model_id} has fewer than 2 present nodes; "
                f"cannot build DAG spec for causal metrics"
            )

        edges: list[tuple[str, str]] = []
        bidirected_edges: list[tuple[str, str]] = []

        for _, row in df[df["type"] == "edge"].iterrows():
            cid = row["comp_id"]
            if not edge_applicable(state, model_id, cid, registry):
                continue
            if state.get_status(model_id, cid) != "causal":
                continue
            if row["direction"] == "->":
                edges.append((row["source"], row["target"]))
            elif row["direction"] == "<->":
                bidirected_edges.append((row["source"], row["target"]))

        if (exposure is None) != (outcome is None):
            raise DyadicError(
                "Both or neither of exposure and outcome must be provided"
            )

        if exposure is not None and outcome is not None:
            exp_cid = self._node_component_id(exposure, registry)
            out_cid = self._node_component_id(outcome, registry)

            exp_present = self._node_is_present(state, model_id, exp_cid)
            out_present = self._node_is_present(state, model_id, out_cid)

            if not exp_present or not out_present:
                return {
                    "nodes": node_names,
                    "edges": edges,
                    "bidirected_edges": bidirected_edges,
                    "declared_nodes": list(node_names),
                    "declared_directed_edges": list(edges),
                    "declared_bidirected_edges": list(bidirected_edges),
                    "declared_observed_nodes": [
                        node for node in node_names if observed_by_node.get(node, True)
                    ],
                    "exposure": exposure,
                    "outcome": outcome,
                    "query_nodes_missing": True,
                }

            if exposure not in node_names:
                raise DyadicError(
                    f"Exposure {exposure!r} is not present in model {model_id}. "
                    f"Present nodes: {node_names}"
                )
            if outcome not in node_names:
                raise DyadicError(
                    f"Outcome {outcome!r} is not present in model {model_id}. "
                    f"Present nodes: {node_names}"
                )
            if exposure == outcome:
                raise DyadicError(
                    f"Exposure and outcome must be distinct nodes, "
                    f"got both {exposure!r}"
                )
        else:
            exposure, outcome = self._default_exposure_outcome(node_names)

        declared_nodes = list(node_names)
        declared_directed_edges = list(edges)
        declared_bidirected_edges = list(bidirected_edges)
        declared_observed_nodes = [
            node for node in node_names if observed_by_node.get(node, True)
        ]

        node_names, edges, bidirected_edges = self._project_latent_nodes(
            node_names, edges, bidirected_edges, observed_by_node
        )
        if exposure not in node_names or outcome not in node_names:
            raise DyadicError(
                "Causal query exposure and outcome must both be observed nodes; "
                f"got exposure={exposure!r}, outcome={outcome!r}"
            )

        return {
            "nodes": node_names,
            "edges": edges,
            "bidirected_edges": bidirected_edges,
            "declared_nodes": declared_nodes,
            "declared_directed_edges": declared_directed_edges,
            "declared_bidirected_edges": declared_bidirected_edges,
            "declared_observed_nodes": declared_observed_nodes,
            "exposure": exposure,
            "outcome": outcome,
            "query_nodes_missing": False,
        }

    def validate_causal_query(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        exposure: str,
        outcome: str,
        *,
        model_ids: list[str] | tuple[str, ...] | None = None,
    ) -> str:
        """Validate the fixed direct edge required by causal dyad queries.

        The structural/basic mode intentionally does not call this validator.
        Causal modes, however, must use a registry whose unique direct
        ``exposure -> outcome`` component is marked ``fixed_status='causal'``
        and whose state makes that edge causal and applicable in every queried
        model.  Keeping this check in the dyadic engine gives full dyads,
        Delta-U, and clustering the same contract.
        """
        if exposure is None or outcome is None:
            raise DyadicError(
                "Both exposure and outcome are required for causal queries"
            )
        if exposure == outcome:
            raise DyadicError("Exposure and outcome must be distinct nodes")

        df = registry.data
        node_rows = df[df["type"] == "node"]
        for node_name, role in ((exposure, "Exposure"), (outcome, "Outcome")):
            matches = node_rows[node_rows["source"] == node_name]
            if len(matches) != 1:
                raise DyadicError(
                    f"{role} {node_name!r} must map to exactly one registry node"
                )
            if not bool(matches.iloc[0].get("observed", True)):
                raise DyadicError(
                    f"{role} {node_name!r} must be an observed registry node"
                )

        direct_rows = df[
            (df["type"] == "edge")
            & (df["direction"] == "->")
            & (df["source"] == exposure)
            & (df["target"] == outcome)
        ]
        if len(direct_rows) != 1:
            raise DyadicError(
                "Causal query requires exactly one direct exposure -> outcome "
                f"edge ({exposure} -> {outcome}) in the registry"
            )

        direct_row = direct_rows.iloc[0]
        if direct_row.get("fixed_status") != "causal":
            raise DyadicError(
                "Causal query requires the direct exposure -> outcome edge "
                f"({exposure} -> {outcome}) to have fixed_status='causal'"
            )
        direct_cid = direct_row["comp_id"]

        queried_models = list(state.model_ids if model_ids is None else model_ids)
        if not queried_models:
            raise DyadicError("Causal query requires at least one model")

        invalid_status: list[str] = []
        inapplicable: list[str] = []
        exp_cid = node_rows[node_rows["source"] == exposure].iloc[0]["comp_id"]
        out_cid = node_rows[node_rows["source"] == outcome].iloc[0]["comp_id"]
        for model_id in queried_models:
            if model_id not in state.model_index:
                invalid_status.append(model_id)
                continue
            if not self._node_is_present(
                state, model_id, exp_cid
            ) or not self._node_is_present(state, model_id, out_cid):
                inapplicable.append(model_id)
                continue
            if not edge_applicable(state, model_id, direct_cid, registry):
                inapplicable.append(model_id)
                continue
            if state.get_status(model_id, direct_cid) != "causal":
                invalid_status.append(model_id)

        if invalid_status:
            raise DyadicError(
                "Causal query requires the fixed direct exposure -> outcome edge "
                f"to be causal in every model; invalid model(s): "
                f"{sorted(set(invalid_status))}"
            )
        if inapplicable:
            raise DyadicError(
                "Causal query requires the fixed direct exposure -> outcome edge "
                f"to be applicable in every model; missing endpoint(s) in model(s): "
                f"{sorted(set(inapplicable))}"
            )
        return direct_cid

    @staticmethod
    def _project_latent_nodes(
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        observed_by_node: dict[str, bool],
    ) -> tuple[list[str], list[tuple[str, str]], list[tuple[str, str]]]:
        """Latent-project a directed graph onto its observed nodes."""
        observed = {node for node in nodes if observed_by_node.get(node, True)}
        latent = set(nodes) - observed
        if not latent:
            return nodes, directed_edges, bidirected_edges
        if any(
            source in latent or target in latent for source, target in bidirected_edges
        ):
            raise DyadicError(
                "Latent projection supports directed paths through latent nodes, "
                "not bidirected edges incident to latent nodes"
            )

        children: dict[str, set[str]] = {node: set() for node in nodes}
        for source, target in directed_edges:
            children.setdefault(source, set()).add(target)

        def observed_descendants(source: str) -> set[str]:
            descendants: set[str] = set()
            pending = list(children.get(source, ()))
            seen: set[str] = set()
            while pending:
                node = pending.pop()
                if node in seen:
                    continue
                seen.add(node)
                if node in observed:
                    descendants.add(node)
                elif node in latent:
                    pending.extend(children.get(node, ()))
            return descendants

        projected_directed: set[tuple[str, str]] = set()
        for source in observed:
            for target in observed_descendants(source):
                if source != target:
                    projected_directed.add((source, target))

        projected_bidirected = {
            tuple(sorted((source, target)))
            for source, target in bidirected_edges
            if source in observed and target in observed and source != target
        }
        for common_cause in latent:
            descendants = sorted(observed_descendants(common_cause))
            for index, source in enumerate(descendants):
                for target in descendants[index + 1 :]:
                    projected_bidirected.add((source, target))

        return (
            [node for node in nodes if node in observed],
            sorted(projected_directed),
            sorted(projected_bidirected),
        )

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
        identification_wrapper=None,
        profiles: dict | None = None,
    ) -> dict:
        if exposure is None or outcome is None:
            return {
                "mas_ego": None,
                "mas_alter": None,
                "mas_compatible": None,
                "identified_ego": None,
                "identified_alter": None,
                "identified_compatible": None,
            }

        if profiles is None:
            profiles = self._build_causal_profiles(
                state,
                registry,
                mode="full",
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
            )
        return CausalProfileBuilder.compare(profiles[model_a], profiles[model_b])

    def _build_causal_profiles(
        self,
        state: StateTensor,
        registry: ComponentRegistry,
        *,
        mode: str,
        causal_wrapper,
        identification_wrapper,
        exposure: str | None,
        outcome: str | None,
        model_ids: list[str] | tuple[str, ...] | None = None,
    ) -> dict | None:
        if mode != "full" or exposure is None or outcome is None:
            return None
        if causal_wrapper is None:
            raise DyadicError("causal_wrapper is required for mode='full'")

        queried_models = list(state.model_ids if model_ids is None else model_ids)
        builder = CausalProfileBuilder(
            state=state,
            registry=registry,
            dag_spec_builder=self._dag_spec_for_model,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
        )
        return builder.build_all(queried_models, exposure, outcome)

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
                row["source"]: row["comp_id"] for _, row in node_comps.iterrows()
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

        for _, row in df[(df["type"] == "edge") & (df["direction"] == "->")].iterrows():
            cid = row["comp_id"]
            if not edge_applicable(state, model_id, cid, registry):
                continue
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
            cid = row["comp_id"]
            if self._node_is_present(state, model_id, cid):
                graph.setdefault(row["source"], [])

        directed_edges = df[(df["type"] == "edge") & (df["direction"] == "->")]
        for _, row in directed_edges.iterrows():
            cid = row["comp_id"]
            if not edge_applicable(state, model_id, cid, registry):
                continue
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
                a_present = self._node_is_present(state, model_a, cid)
                b_present = self._node_is_present(state, model_b, cid)
                if a_present and b_present:
                    a_timing = state.get_timing(model_a, cid)
                    b_timing = state.get_timing(model_b, cid)
                    if (
                        a_timing is not None
                        and b_timing is not None
                        and a_timing != b_timing
                    ):
                        return False

            elif comp_type == "edge":
                a_app = edge_applicable(state, model_a, cid, registry)
                b_app = edge_applicable(state, model_b, cid, registry)
                if a_app and b_app:
                    a_status = state.get_status(model_a, cid)
                    b_status = state.get_status(model_b, cid)
                    if a_status == "causal" and b_status == "causal":
                        direction = comp_row["direction"].values[0]
                        if direction == "<->":
                            continue
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

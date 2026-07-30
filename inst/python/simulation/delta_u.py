from __future__ import annotations

import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from itertools import combinations, product

import pandas as pd
import torch

from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from state.semantics import (
    edge_applicable,
)

logger = logging.getLogger(__name__)

_TOLERANCE = 1e-12
_ROUND_DECIMALS = 6


class DeltaUError(Exception):
    pass


def _has_cycle(directed_edges: set[tuple[str, str]]) -> bool:
    graph: dict[str, list[str]] = {}
    for source, target in directed_edges:
        graph.setdefault(source, []).append(target)
        graph.setdefault(target, [])

    visiting: set[str] = set()
    visited: set[str] = set()

    def visit(node: str) -> bool:
        if node in visiting:
            return True
        if node in visited:
            return False
        visiting.add(node)
        for child in graph.get(node, []):
            if visit(child):
                return True
        visiting.remove(node)
        visited.add(node)
        return False

    return any(visit(node) for node in list(graph))


def _would_create_cycle(
    comp_id: str,
    model_id: str,
    registry: ComponentRegistry,
    state: StateTensor,
) -> bool:
    df = registry.data
    row = df[df["comp_id"] == comp_id]
    if row.empty or row["type"].values[0] != "edge":
        return False
    if row["direction"].values[0] != "->":
        return False
    if not edge_applicable(state, model_id, comp_id, registry):
        return True

    directed_edges: set[tuple[str, str]] = set()
    for _, edge_row in df[
        (df["type"] == "edge") & (df["direction"] == "->")
    ].iterrows():
        edge_cid = edge_row["comp_id"]
        if not edge_applicable(state, model_id, edge_cid, registry):
            continue
        if edge_cid == comp_id or state.get_status(model_id, edge_cid) == "causal":
            directed_edges.add((edge_row["source"], edge_row["target"]))

    return _has_cycle(directed_edges)


def _clone_state(state: StateTensor) -> StateTensor:
    return StateTensor(
        tensor=state.tensor.clone(),
        model_index=dict(state.model_index),
        component_index=dict(state.component_index),
        component_ids=list(state.component_ids),
        model_ids=list(state.model_ids),
        timing=dict(state.timing),
        node_present=(
            state.node_present_mask.clone()
            if hasattr(state, "node_present_mask")
            else None
        ),
        edge_applicable=(
            state.edge_applicable_mask.clone()
            if hasattr(state, "edge_applicable_mask")
            else None
        ),
        node_comp_ids=set(state._node_comp_ids),
        edge_comp_ids=set(state._edge_comp_ids),
        edge_to_nodes=(
            dict(state._edge_to_nodes) if hasattr(state, "_edge_to_nodes") else None
        ),
    )


def resolve_unknown_component(
    state: StateTensor,
    registry: ComponentRegistry,
    component_id: str,
    target_status: str,
    model_ids: list[str] | None = None,
) -> tuple[StateTensor, list[str]]:
    """Resolve valid unknown edge instances without changing resolved claims."""
    resolved = _clone_state(state)
    updates = []
    for model_id in model_ids or state.model_ids:
        if state.get_status(model_id, component_id) != "unknown":
            continue
        if not edge_applicable(state, model_id, component_id, registry):
            continue
        if target_status == "causal" and not _edge_can_be_causal(
            component_id, model_id, registry, state
        ):
            continue
        updates.append((model_id, component_id, target_status))
    resolved.set_status_batch(updates)
    return resolved, [model_id for model_id, _, _ in updates]


_NODE_COMP_CACHE: dict[int, dict[str, str]] = {}


def _node_component_id(node_name: str, registry: ComponentRegistry) -> str:
    registry_id = id(registry)
    if registry_id not in _NODE_COMP_CACHE:
        df = registry.data
        node_comps = df[df["type"] == "node"]
        _NODE_COMP_CACHE[registry_id] = {
            row["source"]: row["comp_id"] for _, row in node_comps.iterrows()
        }

    cache = _NODE_COMP_CACHE[registry_id]
    if node_name not in cache:
        raise DeltaUError(
            f"Ambiguous mapping: cannot find unique node component for "
            f"source={node_name!r}"
        )
    return cache[node_name]


def _edge_can_be_causal(
    comp_id: str,
    model_id: str,
    registry: ComponentRegistry,
    state: StateTensor,
) -> bool:
    df = registry.data
    row = df[df["comp_id"] == comp_id]
    if row.empty or row["type"].values[0] != "edge":
        return True

    if row["direction"].values[0] == "<->":
        return edge_applicable(state, model_id, comp_id, registry)

    source_name = row["source"].values[0]
    target_name = row["target"].values[0]

    try:
        source_cid = _node_component_id(source_name, registry)
        target_cid = _node_component_id(target_name, registry)
    except DeltaUError:
        return False

    source_t = state.get_timing(model_id, source_cid)
    target_t = state.get_timing(model_id, target_cid)

    if source_t is not None and target_t is not None and source_t >= target_t:
        return False

    if _would_create_cycle(comp_id, model_id, registry, state):
        return False

    return True


def _compute_dyad_similarity_index(
    dyads: list[dict],
) -> tuple[torch.Tensor, list[str]]:
    ids = [d["dyad_id"] for d in dyads]
    scores = torch.tensor(
        [d["similarity_rate"] for d in dyads],
        dtype=torch.float32,
    )
    return scores, ids


def _compute_delta_stats(
    baseline_scores: torch.Tensor,
    simulated_scores: torch.Tensor,
) -> tuple[int, int, float]:
    delta = simulated_scores - baseline_scores
    improved = int((delta > _TOLERANCE).sum().item())
    worsened = int((delta < -_TOLERANCE).sum().item())
    mean_delta = round(float(delta.mean().item()), _ROUND_DECIMALS)
    return improved, worsened, mean_delta


def _merge_dyads(
    baseline_dyads: list[dict],
    affected_dyads: list[dict],
    affected_models: set[str],
) -> list[dict]:
    """Merge baseline dyads with recomputed affected dyads."""
    affected_new = {d["dyad_id"]: d for d in affected_dyads}
    result = []
    for d in baseline_dyads:
        parts = d["dyad_id"].split("__")
        ego, alter = parts[0], parts[1]
        if ego in affected_models or alter in affected_models:
            if d["dyad_id"] in affected_new:
                result.append(affected_new[d["dyad_id"]])
            else:
                raise DeltaUError(f"Missing recomputed affected dyad: {d['dyad_id']}")
        else:
            result.append(d)
    return result


class DeltaUEngine:
    def __init__(
        self,
        dyadic_engine=None,
        causal_wrapper=None,
        compatibility_metric: str = "similarity_rate",
        device: str = "auto",
        use_tensor_engine: bool = True,
        exposure: str | None = None,
        outcome: str | None = None,
        identification_wrapper=None,
        model_ids: list[str] | None = None,
        resolution_strategy: str = "update_unknowns",
    ):
        from dyadic.engine import DyadicEngine
        from simulation.scoring import CompatibilityScorer
        from dyadic.tensor_engine import resolve_device

        self._dyadic_engine = dyadic_engine or DyadicEngine()
        self._causal_wrapper = causal_wrapper
        self._compatibility_metric = compatibility_metric
        self._device = device
        self._use_tensor_engine = use_tensor_engine
        self._exposure = exposure
        self._outcome = outcome
        self._identification_wrapper = identification_wrapper
        self._model_ids = sorted(model_ids) if model_ids is not None else None
        if resolution_strategy not in ("condition", "update_unknowns"):
            raise DeltaUError(
                "resolution_strategy must be 'condition' or 'update_unknowns'"
            )
        self._resolution_strategy = resolution_strategy
        self._resolved_device = resolve_device(device)
        self.used_tensor_engine = False
        self._context_coverage_cache: dict[tuple[int, str, str], float] = {}

        self._scorer = CompatibilityScorer(
            compatibility_metric=compatibility_metric,
        )

    def _get_causal_wrapper(self):
        if self._causal_wrapper is not None:
            return self._causal_wrapper
        from dyadic.causal import CausalWrapper

        return CausalWrapper()

    # ------------------------------------------------------------------
    # public API
    # ------------------------------------------------------------------

    def compute_delta_u(
        self,
        component_id: str,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> dict:
        if component_id not in state.component_index:
            raise DeltaUError(f"Unknown component ID: {component_id!r}")

        unknown_models = self._models_with_unknown_applicable_edge(
            component_id, state, registry, self._analysis_model_ids(state)
        )
        if not unknown_models:
            result = {
                "component_id": component_id,
                "delta_u_positive": 0.0,
                "delta_u_negative": 0.0,
                "delta_u": 0.0,
                "best_resolution": "none",
                "dyads_improved": 0,
                "dyads_worsened": 0,
                "resolution_strategy": self._resolution_strategy,
            }
            return self._enrich_metadata([result], registry)[0]

        if self._scorer.requires_causal():
            baseline_dyads = self._ensure_causal_dyads(dyads, state, registry)
        else:
            baseline_dyads = dyads

        baseline_scores = self._scorer.score_dyads(baseline_dyads)
        dyad_ids = [d["dyad_id"] for d in baseline_dyads]
        baseline_compatibility = round(
            float(baseline_scores.mean().item()), _ROUND_DECIMALS
        )

        if self._resolution_strategy == "condition":
            pos = self._condition_resolution(
                component_id,
                "causal",
                state,
                registry,
                baseline_dyads,
                baseline_compatibility,
            )
            neg = self._condition_resolution(
                component_id,
                "non-causal",
                state,
                registry,
                baseline_dyads,
                baseline_compatibility,
            )
        else:
            pos = self._simulate_resolution(
                component_id,
                "causal",
                unknown_models,
                state,
                registry,
                baseline_scores,
                dyad_ids,
                baseline_dyads,
            )
            neg = self._simulate_resolution(
                component_id,
                "non-causal",
                unknown_models,
                state,
                registry,
                baseline_scores,
                dyad_ids,
                baseline_dyads,
            )

        delta_u = round(max(pos["delta"], neg["delta"], 0.0), _ROUND_DECIMALS)
        if delta_u <= _TOLERANCE:
            best = "none"
            best_stats = {"improved": 0, "worsened": 0}
        elif pos["delta"] > neg["delta"]:
            best = "positive"
            best_stats = pos
        elif neg["delta"] > pos["delta"]:
            best = "negative"
            best_stats = neg
        else:
            # Both resolutions yield the same positive gain. Choose a stable
            # direction rather than reporting that resolution has no value.
            best = "positive"
            best_stats = pos

        result = {
            "component_id": component_id,
            "delta_u_positive": pos["delta"],
            "delta_u_negative": neg["delta"],
            "delta_u": delta_u,
            "best_resolution": best,
            "dyads_improved": (
                None
                if self._resolution_strategy == "condition"
                else best_stats["improved"]
            ),
            "dyads_worsened": (
                None
                if self._resolution_strategy == "condition"
                else best_stats["worsened"]
            ),
            "baseline_compatibility": baseline_compatibility,
            "post_compatibility_positive": pos.get("post_compatibility"),
            "post_compatibility_negative": neg.get("post_compatibility"),
            "models_retained_positive": pos.get("models_retained"),
            "models_retained_negative": neg.get("models_retained"),
            "dyads_retained_positive": pos.get("dyads_retained"),
            "dyads_retained_negative": neg.get("dyads_retained"),
            "context_coverage_positive": pos.get("context_coverage"),
            "context_coverage_negative": neg.get("context_coverage"),
            "resolution_strategy": self._resolution_strategy,
        }
        return self._enrich_metadata([result], registry)[0]

    def rank_lynchpins(
        self,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
        top_k: int = 10,
        mode: str = "exhaustive",
        heatmap_threshold: float | None = None,
    ) -> list[dict]:
        if top_k <= 0:
            raise DeltaUError("top_k must be positive")

        uncertain = self._uncertain_applicable_edges(
            state, registry, self._analysis_model_ids(state)
        )
        if not uncertain:
            logger.info("No uncertain applicable edges - multiverse is fully resolved")
            return []

        if self._scorer.requires_causal():
            causal_dyads = self._ensure_causal_dyads(dyads, state, registry)
        else:
            causal_dyads = dyads

        if mode == "two-stage":
            threshold = heatmap_threshold if heatmap_threshold is not None else 0.1
            if not 0.0 <= threshold <= 1.0:
                raise DeltaUError("heatmap_threshold must be between 0 and 1")
            if self._scorer.requires_causal():
                structural_engine = DeltaUEngine(
                    dyadic_engine=self._dyadic_engine,
                    compatibility_metric="similarity_rate",
                    device=self._device,
                    use_tensor_engine=self._use_tensor_engine,
                    model_ids=self._analysis_model_ids(state),
                    resolution_strategy=self._resolution_strategy,
                )
                results = structural_engine._stage1_all(
                    uncertain,
                    state,
                    dyads,
                    registry,
                )
            else:
                results = self._stage1_all(
                    uncertain,
                    state,
                    causal_dyads,
                    registry,
                )
            candidates = [r for r in results if r["delta_u"] >= threshold]
            if self._scorer.requires_causal():
                final = self._stage2_rank(
                    candidates,
                    uncertain,
                    state,
                    causal_dyads,
                    registry,
                )
            else:
                final = candidates
        else:
            results = self._stage1_all(uncertain, state, causal_dyads, registry)
            final = results

        final.sort(key=lambda r: (-r["delta_u"], r["component_id"]))
        ranked = final[:top_k]
        for i, entry in enumerate(ranked):
            entry["rank"] = i + 1

        return self._enrich_metadata(ranked, registry)

    def compute_synergistic_sets(
        self,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
        set_size: int = 2,
        top_n: int = 10,
        search_strategy: str = "greedy",
        beam_width: int = 5,
    ) -> list[dict]:
        if set_size < 2:
            raise DeltaUError("set_size must be at least 2")
        if top_n <= 0:
            raise DeltaUError("top_n must be positive")
        if beam_width <= 0:
            raise DeltaUError("beam_width must be positive")
        if search_strategy not in ("greedy", "beam"):
            raise DeltaUError("search_strategy must be 'greedy' or 'beam'")

        uncertain = self._uncertain_applicable_edges(
            state, registry, self._analysis_model_ids(state)
        )
        if len(uncertain) < set_size:
            logger.info("No uncertain applicable edges - no synergistic sets possible")
            return []

        if self._scorer.requires_causal():
            causal_dyads = self._ensure_causal_dyads(dyads, state, registry)
        else:
            causal_dyads = dyads

        individual = {
            cid: self.compute_delta_u(cid, state, causal_dyads, registry)
            for cid in uncertain
        }
        individual_ranking = sorted(uncertain, key=lambda c: -individual[c]["delta_u"])

        if search_strategy == "greedy":
            sets = self._greedy_sets(
                individual_ranking[:top_n],
                set_size,
                top_n,
                individual,
                state,
                causal_dyads,
                registry,
            )
        else:
            sets = self._beam_sets(
                individual_ranking,
                set_size,
                top_n,
                beam_width,
                individual,
                state,
                causal_dyads,
                registry,
            )

        return sets

    # ------------------------------------------------------------------
    # internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _models_with_unknown_applicable_edge(
        edge_id: str,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
    ) -> list[str]:
        """Find models where an edge is applicable and has unknown status."""
        result = []
        for mid in model_ids or state.model_ids:
            if not edge_applicable(state, mid, edge_id, registry):
                continue
            if state.get_status(mid, edge_id) == "unknown":
                result.append(mid)
        return result

    @staticmethod
    def _uncertain_applicable_edges(
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str] | None = None,
    ) -> list[str]:
        """Find edge components that are unknown in at least one applicable model."""
        uncertain: list[str] = []
        for cid in state.component_ids:
            if cid not in state._edge_comp_ids:
                continue
            for mid in model_ids or state.model_ids:
                if not edge_applicable(state, mid, cid, registry):
                    continue
                if state.get_status(mid, cid) == "unknown":
                    uncertain.append(cid)
                    break
        return uncertain

    def _ensure_causal_dyads(
        self,
        dyads: list[dict],
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> list[dict]:
        metric = self._compatibility_metric
        if all(metric in d for d in dyads):
            return dyads

        logger.info("Dyads missing %s; recomputing full dyads", metric)
        return self._dyadic_engine.compare_pairs(
            state,
            registry,
            self._analysis_model_ids(state),
            mode="full",
            causal_wrapper=self._get_causal_wrapper(),
            identification_wrapper=self._identification_wrapper,
            exposure=self._exposure,
            outcome=self._outcome,
        )

    def _compute_sim_dyads(
        self,
        sim_state: StateTensor,
        registry: ComponentRegistry,
    ) -> list[dict]:
        if not self._scorer.requires_causal() and self._use_tensor_engine:
            from dyadic.tensor_engine import structural_dyad_scores

            try:
                tensor_scores, tensor_ids = structural_dyad_scores(
                    sim_state,
                    registry,
                    model_ids=self._analysis_model_ids(sim_state),
                    device=self._resolved_device,
                )
                sim_dyads = []
                for i, did in enumerate(tensor_ids):
                    parts = did.split("__")
                    sim_dyads.append(
                        {
                            "dyad_id": did,
                            "ego_id": parts[0],
                            "alter_id": parts[1],
                            "similarity_rate": round(float(tensor_scores[i]), 6),
                        }
                    )
                self.used_tensor_engine = True
                return sim_dyads
            except Exception as e:
                logger.warning(
                    "Tensor engine failed, falling back to DyadicEngine: %s", e
                )

        if self._scorer.requires_causal():
            return self._dyadic_engine.compare_pairs(
                sim_state,
                registry,
                self._analysis_model_ids(sim_state),
                mode="full",
                causal_wrapper=self._get_causal_wrapper(),
                identification_wrapper=self._identification_wrapper,
                exposure=self._exposure,
                outcome=self._outcome,
            )

        return self._dyadic_engine.compare_pairs(
            sim_state,
            registry,
            self._analysis_model_ids(sim_state),
            mode="basic",
        )

    def _compute_sim_dyads_incremental(
        self,
        sim_state: StateTensor,
        registry: ComponentRegistry,
        baseline_dyads: list[dict],
        mutated_models: list[str],
    ) -> list[dict]:
        """Compute dyads incrementally: only recompute pairs involving mutated models."""
        mutated_set = set(mutated_models)
        all_models = self._analysis_model_ids(sim_state)

        if self._scorer.requires_causal():
            affected_dyads = self._dyadic_engine.compare_pairs_subset(
                sim_state,
                registry,
                mutated_models,
                all_models,
                mode="full",
                causal_wrapper=self._get_causal_wrapper(),
                identification_wrapper=self._identification_wrapper,
                exposure=self._exposure,
                outcome=self._outcome,
            )
        elif self._use_tensor_engine:
            try:
                from dyadic.tensor_engine import structural_similarity_matrix

                matrix, ordered_ids = structural_similarity_matrix(
                    sim_state,
                    registry,
                    model_ids=all_models,
                    device=self._resolved_device,
                    exclude_temporally_invalid=True,
                )
                affected_dyads = []
                for i, ego in enumerate(ordered_ids):
                    for j, alter in enumerate(ordered_ids):
                        if ego == alter:
                            continue
                        if ego not in mutated_set and alter not in mutated_set:
                            continue
                        affected_dyads.append(
                            {
                                "dyad_id": f"{ego}__{alter}",
                                "ego_id": ego,
                                "alter_id": alter,
                                "similarity_rate": round(float(matrix[i, j]), 6),
                            }
                        )
                self.used_tensor_engine = True
            except Exception as e:
                logger.warning(
                    "Tensor engine failed, falling back to DyadicEngine: %s", e
                )
                affected_dyads = self._dyadic_engine.compare_pairs_subset(
                    sim_state,
                    registry,
                    mutated_models,
                    all_models,
                    mode="basic",
                )
        else:
            affected_dyads = self._dyadic_engine.compare_pairs_subset(
                sim_state,
                registry,
                mutated_models,
                all_models,
                mode="basic",
            )

        return _merge_dyads(baseline_dyads, affected_dyads, mutated_set)

    def _condition_resolution(
        self,
        component_id: str,
        target_status: str,
        state: StateTensor,
        registry: ComponentRegistry,
        baseline_dyads: list[dict],
        baseline_compatibility: float,
    ) -> dict:
        """Score the existing submultiverse consistent with a resolution."""
        model_ids = self._analysis_model_ids(state)
        retained = {
            model_id
            for model_id in model_ids
            if edge_applicable(state, model_id, component_id, registry)
            and state.get_status(model_id, component_id) == target_status
        }
        conditioned_dyads = [
            dyad
            for dyad in baseline_dyads
            if dyad["ego_id"] in retained and dyad["alter_id"] in retained
        ]

        context_coverage = self._resolution_context_coverage(
            component_id,
            target_status,
            state,
            registry,
            model_ids,
        )
        if len(retained) < 2 or not conditioned_dyads:
            return {
                "improved": 0,
                "worsened": 0,
                "delta": 0.0,
                "post_compatibility": None,
                "models_retained": len(retained),
                "dyads_retained": len(conditioned_dyads),
                "context_coverage": context_coverage,
            }

        scores = self._scorer.score_dyads(conditioned_dyads)
        post_compatibility = round(float(scores.mean().item()), _ROUND_DECIMALS)
        return {
            "improved": 0,
            "worsened": 0,
            "delta": round(
                post_compatibility - baseline_compatibility, _ROUND_DECIMALS
            ),
            "post_compatibility": post_compatibility,
            "models_retained": len(retained),
            "dyads_retained": len(conditioned_dyads),
            "context_coverage": context_coverage,
        }

    def _resolution_context_coverage(
        self,
        component_id: str,
        target_status: str,
        state: StateTensor,
        registry: ComponentRegistry,
        model_ids: list[str],
    ) -> float:
        """Return the share of represented contexts containing a target state."""
        cache_key = (id(state), component_id, target_status)
        if cache_key in self._context_coverage_cache:
            return self._context_coverage_cache[cache_key]

        all_contexts = set()
        target_contexts = set()
        for model_id in model_ids:
            if not edge_applicable(state, model_id, component_id, registry):
                continue
            context = (
                tuple(
                    (other_id, state.get_status(model_id, other_id))
                    for other_id in state.component_ids
                    if other_id != component_id
                ),
                tuple(
                    (node_id, state.node_present(model_id, node_id))
                    for node_id in sorted(state._node_comp_ids)
                ),
                tuple(
                    (node_id, state.get_timing(model_id, node_id))
                    for node_id in sorted(state._node_comp_ids)
                ),
            )
            all_contexts.add(context)
            if state.get_status(model_id, component_id) == target_status:
                target_contexts.add(context)
        if not all_contexts:
            return 0.0
        coverage = round(len(target_contexts) / len(all_contexts), _ROUND_DECIMALS)
        self._context_coverage_cache[cache_key] = coverage
        return coverage

    def _simulate_resolution(
        self,
        component_id: str,
        target_status: str,
        unknown_models: list[str],
        state: StateTensor,
        registry: ComponentRegistry,
        baseline_scores: torch.Tensor,
        dyad_ids: list[str],
        baseline_dyads: list[dict],
    ) -> dict:
        sim_state, mutated_models = resolve_unknown_component(
            state,
            registry,
            component_id,
            target_status,
            unknown_models,
        )
        if not mutated_models:
            baseline_compatibility = round(
                float(baseline_scores.mean().item()), _ROUND_DECIMALS
            )
            return {
                "improved": 0,
                "worsened": 0,
                "delta": 0.0,
                "post_compatibility": baseline_compatibility,
                "models_retained": len(self._analysis_model_ids(state)),
                "dyads_retained": len(baseline_dyads),
                "context_coverage": 1.0,
            }

        sim_dyads = self._compute_sim_dyads_incremental(
            sim_state,
            registry,
            baseline_dyads,
            mutated_models,
        )

        sim_scores = self._scorer.score_dyads(sim_dyads)
        sim_ids = [d["dyad_id"] for d in sim_dyads]

        if sim_ids != dyad_ids:
            id_to_idx = {did: i for i, did in enumerate(dyad_ids)}
            aligned_baseline = torch.tensor(
                [
                    baseline_scores[id_to_idx[did]].item() if did in id_to_idx else 0.0
                    for did in sim_ids
                ],
                dtype=torch.float32,
            )
        else:
            aligned_baseline = baseline_scores

        improved, worsened, delta = _compute_delta_stats(
            aligned_baseline,
            sim_scores,
        )
        return {
            "improved": improved,
            "worsened": worsened,
            "delta": delta,
            "post_compatibility": round(
                float(sim_scores.mean().item()), _ROUND_DECIMALS
            ),
            "models_retained": len(self._analysis_model_ids(state)),
            "dyads_retained": len(sim_dyads),
            "context_coverage": 1.0,
        }

    def _stage1_all(
        self,
        uncertain: list[str],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
        *,
        max_workers: int | None = None,
    ) -> list[dict]:
        if len(uncertain) <= 1:
            return [
                self.compute_delta_u(cid, state, dyads, registry) for cid in uncertain
            ]

        # rpy2 calls used by causal metrics must remain on the calling thread.
        if self._scorer.requires_causal():
            return [
                self.compute_delta_u(cid, state, dyads, registry) for cid in uncertain
            ]

        results: dict[str, dict] = {}
        workers = max_workers or min(8, len(uncertain))

        with ThreadPoolExecutor(max_workers=workers) as executor:
            futures = {
                executor.submit(self.compute_delta_u, cid, state, dyads, registry): cid
                for cid in uncertain
            }
            for future in as_completed(futures):
                cid = futures[future]
                try:
                    results[cid] = future.result()
                except Exception as e:
                    logger.warning("Delta-U computation failed for %s: %s", cid, e)
                    results[cid] = {
                        "component_id": cid,
                        "delta_u_positive": 0.0,
                        "delta_u_negative": 0.0,
                        "delta_u": 0.0,
                        "best_resolution": "none",
                        "dyads_improved": 0,
                        "dyads_worsened": 0,
                        "error": str(e),
                    }

        return [results[cid] for cid in uncertain]

    def _stage2_rank(
        self,
        candidates: list[dict],
        _uncertain: list[str],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        candidate_ids = [c["component_id"] for c in candidates]
        reranked = [
            self.compute_delta_u(cid, state, dyads, registry) for cid in candidate_ids
        ]
        return reranked

    def _enrich_metadata(
        self,
        entries: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        df = registry.data.set_index("comp_id")
        for entry in entries:
            cid = entry["component_id"]
            if cid in df.index:
                row = df.loc[cid]
                if isinstance(row, pd.DataFrame):
                    row = row.iloc[0]
                entry["type"] = row["type"]
                entry["source"] = row["source"]
                entry["target"] = row["target"] if row["target"] is not None else None
            else:
                entry["type"] = None
                entry["source"] = None
                entry["target"] = None
        return entries

    def _greedy_sets(
        self,
        candidates: list[str],
        set_size: int,
        top_n: int,
        individual: dict[str, dict],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        all_sets = self._enumerate_combinations(
            candidates,
            set_size,
            individual,
            state,
            dyads,
            registry,
        )
        all_sets.sort(key=lambda r: (-r["delta_u_combined"], r["components"]))
        return self._with_set_ranks(all_sets[:top_n])

    def _beam_sets(
        self,
        all_candidates: list[str],
        set_size: int,
        top_n: int,
        beam_width: int,
        individual: dict[str, dict],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        if len(all_candidates) < set_size:
            return []

        best_singles = sorted(
            all_candidates,
            key=lambda c: -individual[c]["delta_u"],
        )[:beam_width]

        beam: list[tuple[str, ...]] = [(cid,) for cid in best_singles]

        for depth in range(1, set_size):
            next_beam: list[tuple[str, ...]] = []
            for partial in beam:
                for cid in all_candidates:
                    if cid in partial:
                        continue
                    candidate = tuple(sorted((*partial, cid)))
                    if candidate in next_beam:
                        continue
                    next_beam.append(candidate)
            if not next_beam:
                break

            scored: list[dict] = []
            for combo in next_beam:
                score = self._evaluate_combination(
                    list(combo),
                    individual,
                    state,
                    dyads,
                    registry,
                )
                scored.append(score)
            scored.sort(key=lambda r: (-r["delta_u_combined"], r["components"]))
            beam = [tuple(s["components"]) for s in scored[:beam_width]]

        final_sets = [
            self._evaluate_combination(
                list(combo),
                individual,
                state,
                dyads,
                registry,
            )
            for combo in beam
            if len(combo) == set_size
        ]
        final_sets.sort(key=lambda r: (-r["delta_u_combined"], r["components"]))
        return self._with_set_ranks(final_sets[:top_n])

    @staticmethod
    def _with_set_ranks(entries: list[dict]) -> list[dict]:
        for i, entry in enumerate(entries):
            entry["rank"] = i + 1
        return entries

    def _enumerate_combinations(
        self,
        candidates: list[str],
        set_size: int,
        individual: dict[str, dict],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        results: list[dict] = []
        for combo in combinations(candidates, set_size):
            combo_list = sorted(combo)
            score = self._evaluate_combination(
                combo_list,
                individual,
                state,
                dyads,
                registry,
            )
            results.append(score)
        return results

    def _evaluate_combination(
        self,
        component_ids: list[str],
        individual: dict[str, dict],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> dict:
        if self._resolution_strategy == "condition":
            return self._evaluate_conditioned_combination(
                component_ids,
                individual,
                state,
                dyads,
                registry,
            )

        sim_state = _clone_state(state)

        for cid in component_ids:
            best_res = individual[cid]["best_resolution"]
            if best_res == "positive":
                target = "causal"
            elif best_res == "negative":
                target = "non-causal"
            else:
                continue

            unknown_models = self._models_with_unknown_applicable_edge(
                cid, state, registry, self._analysis_model_ids(state)
            )
            for mid in unknown_models:
                if target == "causal" and not _edge_can_be_causal(
                    cid,
                    mid,
                    registry,
                    state,
                ):
                    continue
                sim_state.set_status(mid, cid, target)

        sim_dyads = self._compute_sim_dyads(sim_state, registry)

        baseline_scores = self._scorer.score_dyads(dyads)
        sim_scores = self._scorer.score_dyads(sim_dyads)
        baseline_ids = [d["dyad_id"] for d in dyads]
        sim_ids = [d["dyad_id"] for d in sim_dyads]

        if sim_ids != baseline_ids:
            id_to_idx = {did: i for i, did in enumerate(baseline_ids)}
            baseline_scores = torch.tensor(
                [
                    baseline_scores[id_to_idx[did]].item() if did in id_to_idx else 0.0
                    for did in sim_ids
                ],
                dtype=torch.float32,
            )

        _, _, combined_delta = _compute_delta_stats(
            baseline_scores,
            sim_scores,
        )

        individual_sum = sum(
            round(individual[cid]["delta_u"], _ROUND_DECIMALS) for cid in component_ids
        )
        synergy = round(combined_delta - individual_sum, _ROUND_DECIMALS)
        label = "super-additive" if synergy > _TOLERANCE else "additive"

        return {
            "components": component_ids,
            "delta_u_combined": combined_delta,
            "delta_u_individual_sum": round(individual_sum, _ROUND_DECIMALS),
            "synergy_score": synergy,
            "label": label,
        }

    def _evaluate_conditioned_combination(
        self,
        component_ids: list[str],
        individual: dict[str, dict],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> dict:
        baseline_scores = self._scorer.score_dyads(dyads)
        baseline = float(baseline_scores.mean().item())
        best_delta = float("-inf")
        best_retained: set[str] = set()
        best_conditioned: list[dict] = []
        best_assignment: tuple[str, ...] | None = None

        for assignment in product(("causal", "non-causal"), repeat=len(component_ids)):
            retained = set(self._analysis_model_ids(state))
            for component_id, target_status in zip(component_ids, assignment):
                retained &= {
                    model_id
                    for model_id in retained
                    if edge_applicable(state, model_id, component_id, registry)
                    and state.get_status(model_id, component_id) == target_status
                }
            conditioned = [
                dyad
                for dyad in dyads
                if dyad["ego_id"] in retained and dyad["alter_id"] in retained
            ]
            if not conditioned:
                continue
            post_scores = self._scorer.score_dyads(conditioned)
            delta = float(post_scores.mean().item()) - baseline
            if delta > best_delta:
                best_delta = delta
                best_retained = retained
                best_conditioned = conditioned
                best_assignment = assignment

        combined_delta = (
            round(best_delta, _ROUND_DECIMALS) if best_assignment is not None else 0.0
        )

        individual_sum = sum(
            round(individual[cid]["delta_u"], _ROUND_DECIMALS) for cid in component_ids
        )
        synergy = round(combined_delta - individual_sum, _ROUND_DECIMALS)
        label = "super-additive" if synergy > _TOLERANCE else "additive"
        return {
            "components": component_ids,
            "delta_u_combined": combined_delta,
            "delta_u_individual_sum": round(individual_sum, _ROUND_DECIMALS),
            "synergy_score": synergy,
            "label": label,
            "best_resolutions": (
                dict(zip(component_ids, best_assignment))
                if best_assignment is not None
                else {}
            ),
            "models_retained": len(best_retained),
            "dyads_retained": len(best_conditioned),
            "resolution_strategy": self._resolution_strategy,
        }

    def _analysis_model_ids(self, state: StateTensor) -> list[str]:
        if self._model_ids is None:
            return list(state.model_ids)
        return [
            model_id for model_id in self._model_ids if model_id in state.model_index
        ]

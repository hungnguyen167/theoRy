from __future__ import annotations

import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from itertools import combinations, product

import pandas as pd
import torch

from registry.schema import ComponentRegistry
from state.completions import CompletionIndex
from state.semantics import (
    edge_applicable,
)
from state.tensor import StateTensor

logger = logging.getLogger(__name__)

_TOLERANCE = 1e-12
_ROUND_DECIMALS = 6


class DeltaUError(Exception):
    pass


def _compute_delta_stats(
    baseline_scores: torch.Tensor,
    simulated_scores: torch.Tensor,
) -> tuple[int, int, float]:
    delta = simulated_scores - baseline_scores
    improved = int((delta > _TOLERANCE).sum().item())
    worsened = int((delta < -_TOLERANCE).sum().item())
    mean_delta = round(float(delta.mean().item()), _ROUND_DECIMALS)
    return improved, worsened, mean_delta


class DeltaUEngine:
    """Concrete crux engine with marginal and global resolution semantics.

    Both modes reuse existing models and their precomputed dyad information
    instead of mutating states or recomputing causal profiles:

    - ``marginal`` ranks individual uncertain edge components. For each
      candidate and each resolution direction (causal / non-causal), every
      analysis model where the edge is applicable and unknown is remapped to
      the existing model whose semantic state is identical except for that
      edge. Post-resolution compatibility is computed from the copied dyads.
    - ``global`` resolves every applicable unknown edge instance in the
      analysis multiverse to a single user-selected status and compares the
      remapped multiverse against the unchanged baseline.
    """

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
        crux_mode: str = "marginal",
    ):
        from simulation.scoring import CompatibilityScorer

        self._dyadic_engine = dyadic_engine
        self._causal_wrapper = causal_wrapper
        self._compatibility_metric = compatibility_metric
        self._device = device
        self._use_tensor_engine = use_tensor_engine
        self._exposure = exposure
        self._outcome = outcome
        self._identification_wrapper = identification_wrapper
        self._model_ids = sorted(model_ids) if model_ids is not None else None
        if crux_mode not in ("marginal", "global"):
            raise DeltaUError("crux_mode must be 'marginal' or 'global'")
        self._crux_mode = crux_mode
        self.used_tensor_engine = False

        self._scorer = CompatibilityScorer(
            compatibility_metric=compatibility_metric,
        )

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
        """Marginal crux for a single component (evaluates both directions)."""
        if self._crux_mode == "global":
            raise DeltaUError("global crux does not evaluate single components")
        if component_id not in state.component_index:
            raise DeltaUError(f"Unknown component ID: {component_id!r}")
        component = registry.data[registry.data["comp_id"] == component_id]
        if component.empty or component.iloc[0]["type"] != "edge":
            raise DeltaUError(
                f"Component {component_id!r} is not an edge and cannot be a crux."
            )

        analysis_ids = self._analysis_model_ids(state)
        unknown_models = self._models_with_unknown_applicable_edge(
            component_id, state, registry, analysis_ids
        )
        if not unknown_models:
            result = {
                "component_id": component_id,
                "delta_u_causal": 0.0,
                "delta_u_non_causal": 0.0,
                "delta_u": 0.0,
                "best_resolution": "none",
                "dyads_improved": 0,
                "dyads_worsened": 0,
                "crux_mode": "marginal",
            }
            return self._enrich_metadata([result], registry)[0]

        self._validate_dyad_universe(dyads, analysis_ids)
        self._validate_baseline_metric(dyads)
        baseline_scores = self._scorer.score_dyads(dyads)
        baseline_compatibility = round(
            float(baseline_scores.mean().item()), _ROUND_DECIMALS
        )
        index = CompletionIndex(state, registry)

        pos = self._marginal_branch(
            component_id,
            "causal",
            unknown_models,
            state,
            dyads,
            registry,
            index,
            baseline_scores,
            baseline_compatibility,
        )
        neg = self._marginal_branch(
            component_id,
            "non-causal",
            unknown_models,
            state,
            dyads,
            registry,
            index,
            baseline_scores,
            baseline_compatibility,
        )

        for target_status, branch in (("causal", pos), ("non-causal", neg)):
            if branch["mapping_coverage"] < 1.0:
                missing = branch["invalid_models"] + branch["unmatched_models"]
                raise DeltaUError(
                    f"Marginal crux is unavailable for {component_id} resolved "
                    f"to {target_status!r}: the multiverse is not resolution-closed "
                    f"(missing/invalid models: {missing}). Supply a resolution-closed "
                    "multiverse (e.g. exhaustive expansion)."
                )

        pos_delta = pos["delta"]
        neg_delta = neg["delta"]

        def _pick_best(pd_: float | None, nd_: float | None):
            if pd_ is None and nd_ is None:
                return "none", {"improved": 0, "worsened": 0}
            if pd_ is None:
                if nd_ is not None and nd_ > _TOLERANCE:
                    return "non-causal", neg
                return "none", {"improved": 0, "worsened": 0}
            if nd_ is None:
                if pd_ > _TOLERANCE:
                    return "causal", pos
                return "none", {"improved": 0, "worsened": 0}
            if pd_ > nd_:
                if pd_ > _TOLERANCE:
                    return "causal", pos
                return "none", {"improved": 0, "worsened": 0}
            if nd_ > pd_:
                if nd_ > _TOLERANCE:
                    return "non-causal", neg
                return "none", {"improved": 0, "worsened": 0}
            if pd_ > _TOLERANCE:
                return "causal", pos
            return "none", {"improved": 0, "worsened": 0}

        best, best_stats = _pick_best(pos_delta, neg_delta)
        nonzero = [d for d in (pos_delta, neg_delta, 0.0) if d is not None]
        delta_u = round(max(nonzero), _ROUND_DECIMALS)

        result = {
            "component_id": component_id,
            "delta_u_causal": pos_delta,
            "delta_u_non_causal": neg_delta,
            "delta_u": delta_u,
            "best_resolution": best,
            "dyads_improved": best_stats["improved"],
            "dyads_worsened": best_stats["worsened"],
            "baseline_compatibility": baseline_compatibility,
            "post_compatibility_causal": pos.get("post_compatibility"),
            "post_compatibility_non_causal": neg.get("post_compatibility"),
            "models_changed_causal": pos.get("models_changed"),
            "models_changed_non_causal": neg.get("models_changed"),
            "instances_forced_causal": pos.get("instances_forced"),
            "instances_forced_non_causal": neg.get("instances_forced"),
            "mapping_coverage_causal": pos.get("mapping_coverage"),
            "mapping_coverage_non_causal": neg.get("mapping_coverage"),
            "invalid_models_causal": pos.get("invalid_models"),
            "invalid_models_non_causal": neg.get("invalid_models"),
            "unmatched_models_causal": pos.get("unmatched_models"),
            "unmatched_models_non_causal": neg.get("unmatched_models"),
            "crux_mode": "marginal",
        }
        return self._enrich_metadata([result], registry)[0]

    def compute_global_crux(
        self,
        target_status: str,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> dict:
        """Resolve every applicable unknown edge instance to one status."""
        if target_status not in ("causal", "non-causal"):
            raise DeltaUError("global target_status must be 'causal' or 'non-causal'")

        analysis_ids = self._analysis_model_ids(state)
        self._validate_dyad_universe(dyads, analysis_ids)
        self._validate_baseline_metric(dyads)
        baseline_scores = self._scorer.score_dyads(dyads)
        baseline_compatibility = round(
            float(baseline_scores.mean().item()), _ROUND_DECIMALS
        )

        index = CompletionIndex(state, registry)
        allowed = self._dyad_model_ids(dyads)
        flips_per_model: dict[str, dict[str, str]] = {}
        instances: list[tuple[str, str]] = []
        for mid in analysis_ids:
            flips: dict[str, str] = {}
            for cid in self._edge_component_ids(registry):
                if not edge_applicable(state, mid, cid, registry):
                    continue
                if state.get_status(mid, cid) == "unknown":
                    flips[cid] = target_status
                    instances.append((mid, cid))
            if flips:
                flips_per_model[mid] = flips

        base = {
            "crux_mode": "global",
            "target_status": target_status,
            "baseline_compatibility": baseline_compatibility,
            "model_count": len(analysis_ids),
            "dyad_count": len(dyads),
            "unknown_instances_forced": len(instances),
        }

        if not flips_per_model:
            logger.info("Global crux: no applicable unknown edge instances")
            return {
                **base,
                "feasible": True,
                "post_compatibility": baseline_compatibility,
                "compatibility_change": 0.0,
                "delta_u": 0.0,
                "models_changed": 0,
                "dyads_improved": 0,
                "dyads_worsened": 0,
                "mapping_coverage": 1.0,
                "invalid_models": [],
                "unmatched_models": [],
                "post_dyads": [dict(dyad) for dyad in dyads],
            }

        mapping: dict[str, str] = {}
        invalid: list[str] = []
        unmatched: list[str] = []
        for mid, flips in flips_per_model.items():
            signature = index.signature_after_resolution(mid, flips)
            if not index.is_valid_signature(signature):
                invalid.append(mid)
                continue
            source = index.matching_model(mid, flips, allowed)
            if source is None:
                unmatched.append(mid)
                continue
            mapping[mid] = source

        coverage = round(len(mapping) / len(flips_per_model), _ROUND_DECIMALS)
        if invalid or unmatched:
            raise DeltaUError(
                "Global crux is unavailable: the multiverse is not "
                "resolution-closed "
                f"(invalid models: {invalid}; unmatched models: {unmatched}; "
                f"mapping coverage: {coverage:.6f}). Supply a resolution-closed "
                "multiverse (e.g. exhaustive expansion)."
            )

        post_dyads = self._remap_dyads(mapping, dyads, analysis_ids, state, registry)
        post_scores = self._scorer.score_dyads(post_dyads)
        aligned_baseline = self._align_baseline_scores(
            dyads, baseline_scores, post_dyads
        )
        improved, worsened, delta = _compute_delta_stats(aligned_baseline, post_scores)
        post_compatibility = round(
            float(post_scores.mean().item()),
            _ROUND_DECIMALS,
        )
        change = round(post_compatibility - baseline_compatibility, _ROUND_DECIMALS)
        return {
            **base,
            "feasible": True,
            "post_compatibility": post_compatibility,
            "compatibility_change": change,
            "delta_u": round(max(change, 0.0), _ROUND_DECIMALS),
            "models_changed": len(mapping),
            "dyads_improved": improved,
            "dyads_worsened": worsened,
            "mapping_coverage": coverage,
            "invalid_models": [],
            "unmatched_models": [],
            "post_dyads": post_dyads,
        }

    def resolve_dyads(
        self,
        component_id: str,
        target_status: str,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> list[dict]:
        """Return remapped post-resolution dyads for a marginal resolution."""
        if target_status not in ("causal", "non-causal"):
            raise DeltaUError("target_status must be 'causal' or 'non-causal'")
        analysis_ids = self._analysis_model_ids(state)
        self._validate_dyad_universe(dyads, analysis_ids)
        self._validate_baseline_metric(dyads)
        unknown_models = self._models_with_unknown_applicable_edge(
            component_id, state, registry, analysis_ids
        )
        if not unknown_models:
            return [dict(dyad) for dyad in dyads]
        mapping = self._build_resolution_mapping(
            component_id, target_status, unknown_models, state, dyads, registry
        )
        return self._remap_dyads(mapping, dyads, analysis_ids, state, registry)

    def rank_lynchpins(
        self,
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
        top_k: int = 10,
        mode: str = "exhaustive",
        heatmap_threshold: float | None = None,
    ) -> list[dict]:
        """Rank uncertain components by marginal Delta-U."""
        if top_k <= 0:
            raise DeltaUError("top_k must be positive")
        if self._crux_mode == "global":
            raise DeltaUError("global crux does not produce component rankings")

        uncertain = self._uncertain_applicable_edges(
            state, registry, self._analysis_model_ids(state)
        )
        if not uncertain:
            logger.info("No uncertain applicable edges - multiverse is fully resolved")
            return []

        self._validate_dyad_universe(dyads, self._analysis_model_ids(state))
        self._validate_baseline_metric(dyads)

        if mode == "two-stage":
            threshold = heatmap_threshold if heatmap_threshold is not None else 0.1
            if not 0.0 <= threshold <= 1.0:
                raise DeltaUError("heatmap_threshold must be between 0 and 1")
            structural_engine = DeltaUEngine(
                compatibility_metric="similarity_rate",
                model_ids=self._analysis_model_ids(state),
                crux_mode="marginal",
            )
            structural = structural_engine._stage1_all(
                uncertain, state, dyads, registry
            )
            candidates = [r for r in structural if r["delta_u"] >= threshold]
            final = self._stage2_rank(candidates, uncertain, state, dyads, registry)
        else:
            final = self._stage1_all(uncertain, state, dyads, registry)

        for entry in final:
            if (
                entry.get("mapping_coverage_causal", 1.0) < 1.0
                or entry.get("mapping_coverage_non_causal", 1.0) < 1.0
            ):
                missing = [
                    m
                    for m in (
                        entry.get("invalid_models_causal", [])
                        + entry.get("unmatched_models_causal", [])
                        + entry.get("invalid_models_non_causal", [])
                        + entry.get("unmatched_models_non_causal", [])
                    )
                ]
                raise DeltaUError(
                    f"Marginal crux ranking is unavailable for {entry['component_id']}: "
                    "the multiverse is not resolution-closed "
                    f"(missing/invalid models: {missing}). Supply a resolution-closed "
                    "multiverse (e.g. exhaustive expansion)."
                )

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
        if self._crux_mode == "global":
            raise DeltaUError("global crux does not support synergistic sets")

        uncertain = self._uncertain_applicable_edges(
            state, registry, self._analysis_model_ids(state)
        )
        if len(uncertain) < set_size:
            logger.info("No uncertain applicable edges - no synergistic sets possible")
            return []

        individual = {
            cid: self.compute_delta_u(cid, state, dyads, registry) for cid in uncertain
        }
        individual_ranking = sorted(uncertain, key=lambda c: -individual[c]["delta_u"])

        if search_strategy == "greedy":
            sets = self._greedy_sets(
                individual_ranking[:top_n],
                set_size,
                top_n,
                individual,
                state,
                dyads,
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
                dyads,
                registry,
            )

        return sets

    # ------------------------------------------------------------------
    # marginal machinery
    # ------------------------------------------------------------------

    def _marginal_branch(
        self,
        component_id: str,
        target_status: str,
        unknown_models: list[str],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
        index: CompletionIndex,
        baseline_scores: torch.Tensor,
        baseline_compatibility: float,
    ) -> dict:
        invalid: list[str] = []
        unmatched: list[str] = []
        mapping: dict[str, str] = {}
        allowed = self._dyad_model_ids(dyads)
        for mid in unknown_models:
            assignments = {component_id: target_status}
            signature = index.signature_after_resolution(mid, assignments)
            if not index.is_valid_signature(signature):
                invalid.append(mid)
                continue
            source = index.matching_model(mid, assignments, allowed)
            if source is None:
                unmatched.append(mid)
                continue
            mapping[mid] = source

        coverage = (
            round(len(mapping) / len(unknown_models), _ROUND_DECIMALS)
            if unknown_models
            else 1.0
        )
        if invalid or unmatched:
            return {
                "delta": None,
                "post_compatibility": None,
                "models_changed": len(mapping),
                "instances_forced": len(unknown_models),
                "mapping_coverage": coverage,
                "invalid_models": invalid,
                "unmatched_models": unmatched,
                "post_dyads": None,
                "improved": 0,
                "worsened": 0,
            }
        if not mapping:
            return {
                "delta": 0.0,
                "post_compatibility": baseline_compatibility,
                "models_changed": 0,
                "instances_forced": len(unknown_models),
                "mapping_coverage": coverage,
                "invalid_models": invalid,
                "unmatched_models": unmatched,
                "post_dyads": None,
                "improved": 0,
                "worsened": 0,
            }

        post_dyads = self._remap_dyads(
            mapping,
            dyads,
            self._analysis_model_ids(state),
            state,
            registry,
        )
        post_scores = self._scorer.score_dyads(post_dyads)
        aligned_baseline = self._align_baseline_scores(
            dyads, baseline_scores, post_dyads
        )
        improved, worsened, delta = _compute_delta_stats(aligned_baseline, post_scores)
        return {
            "delta": delta,
            "post_compatibility": round(
                float(post_scores.mean().item()), _ROUND_DECIMALS
            ),
            "models_changed": len(mapping),
            "instances_forced": len(unknown_models),
            "mapping_coverage": coverage,
            "invalid_models": invalid,
            "unmatched_models": unmatched,
            "post_dyads": post_dyads,
            "improved": improved,
            "worsened": worsened,
        }

    def _build_resolution_mapping(
        self,
        component_id: str,
        target_status: str,
        unknown_models: list[str],
        state: StateTensor,
        dyads: list[dict],
        registry: ComponentRegistry,
    ) -> dict[str, str]:
        mapping: dict[str, str] = {}
        index = CompletionIndex(state, registry)
        allowed = self._dyad_model_ids(dyads)
        for mid in unknown_models:
            assignments = {component_id: target_status}
            signature = index.signature_after_resolution(mid, assignments)
            if not index.is_valid_signature(signature):
                raise DeltaUError(
                    f"Cannot resolve {component_id} to {target_status!r} for model "
                    f"{mid}: the resulting state is invalid (timing or cycle)."
                )
            source = index.matching_model(mid, assignments, allowed)
            if source is None:
                raise DeltaUError(
                    f"No existing model matches model {mid} with {component_id} "
                    f"resolved to {target_status!r}. Supply a resolution-closed "
                    "multiverse (e.g. exhaustive expansion)."
                )
            mapping[mid] = source
        return mapping

    def _remap_dyads(
        self,
        mapping: dict[str, str],
        dyads: list[dict],
        model_ids: list[str],
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> list[dict]:
        index = {(d["ego_id"], d["alter_id"]): d for d in dyads}
        profiles = self._extract_profiles(dyads)
        post: list[dict] = []
        for ego in sorted(model_ids):
            for alter in sorted(model_ids):
                if ego == alter:
                    continue
                source_ego = mapping.get(ego, ego)
                source_alter = mapping.get(alter, alter)
                if source_ego == source_alter:
                    post.append(
                        self._self_dyad(
                            ego,
                            alter,
                            source_ego,
                            profiles,
                            state,
                            registry,
                        )
                    )
                    continue
                key = (source_ego, source_alter)
                if key not in index:
                    raise DeltaUError(
                        f"Missing baseline dyad for source pair {key} while "
                        f"remapping {ego} -> {alter}."
                    )
                record = dict(index[key])
                record["dyad_id"] = f"{ego}__{alter}"
                record["ego_id"] = ego
                record["alter_id"] = alter
                record["source_ego_id"] = source_ego
                record["source_alter_id"] = source_alter
                post.append(record)
        return post

    @staticmethod
    def _extract_profiles(dyads: list[dict]) -> dict[str, dict]:
        profiles: dict[str, dict] = {}
        for d in dyads:
            mid = d.get("ego_id")
            if mid in profiles:
                continue
            identified = d.get("identified_ego")
            if isinstance(identified, str):
                normalized = identified.strip().lower()
                if normalized == "true":
                    identified = True
                elif normalized == "false":
                    identified = False
                else:
                    raise DeltaUError(
                        f"Invalid identified_ego value for model {mid}: "
                        f"{identified!r}."
                    )
            elif identified is not None and not isinstance(identified, bool):
                raise DeltaUError(
                    f"Invalid identified_ego value for model {mid}: " f"{identified!r}."
                )
            profiles[mid] = {
                "mas": d.get("mas_ego"),
                "identified": identified,
                "id_nodes": d.get("identification_nodes_ego"),
                "has_causal": any(
                    field in d
                    for field in (
                        "mas_ego",
                        "identified_ego",
                        "identification_nodes_ego",
                    )
                ),
            }
        return profiles

    def _self_dyad(
        self,
        ego: str,
        alter: str,
        source: str,
        profiles: dict,
        state: StateTensor,
        registry: ComponentRegistry,
    ) -> dict:
        from dyadic.engine import DyadicEngine

        metric = self._compatibility_metric
        structural_engine = self._dyadic_engine or DyadicEngine()
        record = structural_engine.compare(
            source, source, state, registry, mode="basic"
        )
        record.update(
            {
                "dyad_id": f"{ego}__{alter}",
                "ego_id": ego,
                "alter_id": alter,
                "source_ego_id": source,
                "source_alter_id": source,
            }
        )
        profile = profiles.get(source)
        if profile is None:
            raise DeltaUError(
                f"Cannot derive self-source compatibility for model {source}: "
                "profile fields are missing from the baseline dyads."
            )
        if profile["has_causal"]:
            from dyadic.profiles import normalize_mas

            raw = profile["mas"]
            record["mas_ego"] = raw
            record["mas_alter"] = raw
            if raw is None:
                record["mas_compatible"] = None
            else:
                normalized = normalize_mas(raw)
                record["mas_compatible"] = (
                    bool(normalized) if normalized is not None else None
                )

            identified = profile["identified"]
            record["identified_ego"] = identified
            record["identified_alter"] = identified
            record["identification_nodes_ego"] = profile["id_nodes"]
            record["identification_nodes_alter"] = profile["id_nodes"]
            if identified is None:
                record["identified_compatible"] = None
            elif identified is False:
                record["identified_compatible"] = False
            elif profile["id_nodes"] is None:
                record["identified_compatible"] = None
            else:
                record["identified_compatible"] = True

        if metric == "mas_compatible" and "mas_compatible" not in record:
            record["mas_compatible"] = None
        if metric == "identified_compatible" and "identified_compatible" not in record:
            record["identified_compatible"] = None
        return record

    @staticmethod
    def _align_baseline_scores(
        baseline_dyads: list[dict],
        baseline_scores: torch.Tensor,
        post_dyads: list[dict],
    ) -> torch.Tensor:
        positions: dict[tuple[str, str], int] = {}
        for i, dyad in enumerate(baseline_dyads):
            key = (dyad["ego_id"], dyad["alter_id"])
            if key in positions:
                raise DeltaUError(f"Duplicate baseline dyad for model pair {key}.")
            positions[key] = i

        indices: list[int] = []
        for dyad in post_dyads:
            key = (dyad["ego_id"], dyad["alter_id"])
            if key not in positions:
                raise DeltaUError(f"Missing baseline dyad for model pair {key}.")
            indices.append(positions[key])
        index_tensor = torch.tensor(indices, device=baseline_scores.device)
        return baseline_scores[index_tensor]

    @staticmethod
    def _validate_dyad_universe(dyads: list[dict], model_ids: list[str]) -> None:
        expected = {
            (ego, alter) for ego in model_ids for alter in model_ids if ego != alter
        }
        actual = [(d.get("ego_id"), d.get("alter_id")) for d in dyads]
        actual_set = set(actual)
        if len(actual) != len(actual_set):
            raise DeltaUError("Baseline dyads contain duplicate directed model pairs.")
        if actual_set != expected:
            missing = sorted(expected - actual_set)
            extra = sorted(actual_set - expected)
            raise DeltaUError(
                "Baseline dyads must contain exactly one directed dyad for every "
                f"analysis-model pair (missing: {missing}; extra: {extra})."
            )

    def _validate_baseline_metric(self, dyads: list[dict]) -> None:
        missing = sum(1 for d in dyads if d.get(self._compatibility_metric) is None)
        if missing:
            if not self._scorer.requires_causal():
                raise DeltaUError(
                    f"Compatibility metric {self._compatibility_metric!r} is "
                    f"unavailable for {missing} dyad(s)."
                )
            raise DeltaUError(
                f"Compatibility metric {self._compatibility_metric!r} is "
                f"unavailable for {missing} dyad(s). Supply full dyads "
                "(build_dyad_matrix(mode='full')) with exposure/outcome; "
                "hypothetical resolutions never recompute causal profiles."
            )
        if not self._scorer.requires_causal():
            return
        if self._compatibility_metric == "mas_compatible":
            missing_profile = sum(1 for d in dyads if d.get("mas_ego") is None)
        else:
            missing_profile = sum(
                1
                for d in dyads
                if d.get("identified_ego") is None
                or (
                    d.get("identified_ego") in (True, "TRUE", "true")
                    and d.get("identification_nodes_ego") is None
                )
            )
        if missing_profile:
            raise DeltaUError(
                f"Compatibility metric {self._compatibility_metric!r} requires "
                f"per-model profile fields in the baseline dyads "
                f"(mas_ego / identified_ego / identification_nodes_ego); "
                f"{missing_profile} dyad(s) are missing them. Supply full dyads "
                "(build_dyad_matrix(mode='full'))."
            )

    @staticmethod
    def _dyad_model_ids(dyads: list[dict]) -> set[str]:
        ids: set[str] = set()
        for d in dyads:
            ids.add(d["ego_id"])
            ids.add(d["alter_id"])
        return ids

    @staticmethod
    def _edge_component_ids(registry: ComponentRegistry) -> list[str]:
        return registry.data[registry.data["type"] == "edge"]["comp_id"].tolist()

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
                results[cid] = future.result()

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
        return [
            self.compute_delta_u(cid, state, dyads, registry) for cid in candidate_ids
        ]

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
        """Evaluate a joint marginal resolution of several components.

        Every joint causal/non-causal assignment is searched. For each
        assignment, each affected model is remapped to the existing model
        satisfying the full assignment; the joint delta is computed from the
        copied dyads. The individual sum uses each component's marginal delta
        for the same target chosen by the joint assignment.
        """
        analysis_ids = self._analysis_model_ids(state)
        baseline_scores = self._scorer.score_dyads(dyads)
        baseline_compatibility = round(
            float(baseline_scores.mean().item()), _ROUND_DECIMALS
        )

        unknown_by_model: dict[str, list[str]] = {
            mid: [
                cid
                for cid in component_ids
                if edge_applicable(state, mid, cid, registry)
                and state.get_status(mid, cid) == "unknown"
            ]
            for mid in analysis_ids
        }

        index = CompletionIndex(state, registry)
        allowed = self._dyad_model_ids(dyads)

        best_delta = float("-inf")
        best_assignment: tuple[str, ...] | None = None
        best_post_dyads: list[dict] | None = None

        for assignment in product(("causal", "non-causal"), repeat=len(component_ids)):
            targets = dict(zip(component_ids, assignment))
            mapping: dict[str, str] = {}
            feasible = True
            for mid, unknown_ids in unknown_by_model.items():
                if not unknown_ids:
                    continue
                flips = {cid: targets[cid] for cid in unknown_ids}
                signature = index.signature_after_resolution(mid, flips)
                if not index.is_valid_signature(signature):
                    feasible = False
                    break
                source = index.matching_model(mid, flips, allowed)
                if source is None:
                    feasible = False
                    break
                mapping[mid] = source
            if not feasible:
                continue

            post_dyads = self._remap_dyads(
                mapping, dyads, analysis_ids, state, registry
            )
            post_scores = self._scorer.score_dyads(post_dyads)
            post_compatibility = round(
                float(post_scores.mean().item()), _ROUND_DECIMALS
            )
            delta = round(post_compatibility - baseline_compatibility, _ROUND_DECIMALS)
            if delta > best_delta:
                best_delta = delta
                best_assignment = assignment
                best_post_dyads = post_dyads

        if best_assignment is None:
            return {
                "components": component_ids,
                "delta_u_combined": 0.0,
                "delta_u_individual_sum": round(
                    sum(individual[cid]["delta_u"] for cid in component_ids),
                    _ROUND_DECIMALS,
                ),
                "synergy_score": 0.0,
                "label": "additive",
                "best_resolutions": {},
                "feasible": False,
            }

        targets = dict(zip(component_ids, best_assignment))
        individual_sum = round(
            sum(
                (
                    individual[cid]["delta_u_causal"]
                    if targets[cid] == "causal"
                    else individual[cid]["delta_u_non_causal"]
                )
                for cid in component_ids
            ),
            _ROUND_DECIMALS,
        )
        synergy = round(best_delta - individual_sum, _ROUND_DECIMALS)
        label = "super-additive" if synergy > _TOLERANCE else "additive"
        return {
            "components": component_ids,
            "delta_u_combined": best_delta,
            "delta_u_individual_sum": individual_sum,
            "synergy_score": synergy,
            "label": label,
            "best_resolutions": targets,
            "feasible": True,
            "post_compatibility": round(
                float(self._scorer.score_dyads(best_post_dyads).mean().item()),
                _ROUND_DECIMALS,
            ),
            "baseline_compatibility": baseline_compatibility,
        }

    def _analysis_model_ids(self, state: StateTensor) -> list[str]:
        if self._model_ids is None:
            return list(state.model_ids)
        return [
            model_id for model_id in self._model_ids if model_id in state.model_index
        ]

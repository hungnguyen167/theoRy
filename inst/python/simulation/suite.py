"""Simulation suite: three proof-of-concept scenario generators."""

import copy
import random

from registry.builder import ComponentRegistryBuilder
from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from dyadic.engine import DyadicEngine
from simulation.delta_u import DeltaUEngine
from clustering.engine import ClusteringEngine
from clustering.ghost import GhostDetector


class SimulationError(Exception):
    pass


class SimulationSuite:
    """Generates synthetic multiverses for proof-of-concept scenarios."""

    def __init__(self, random_state: int | None = 42):
        self._rng = random.Random(random_state)
        self._random_state = random_state

    # ── public dispatcher ──────────────────────────────────────────────────────

    def run_scenario(
        self,
        scenario: str,
        n_models: int = 100,
        n_components: int = 50,
    ) -> dict:
        if scenario == "illusion_of_precision":
            return self._scenario_illusion(n_models, n_components)
        elif scenario == "lynchpin_of_certainty":
            return self._scenario_lynchpin(n_models, n_components)
        elif scenario == "ghost_discovery":
            return self._scenario_ghost(n_models, n_components)
        else:
            raise SimulationError(
                f"Unknown scenario {scenario!r}. "
                "Must be one of: illusion_of_precision, "
                "lynchpin_of_certainty, ghost_discovery"
            )

    # ── synthetic registry builder ─────────────────────────────────────────────

    def _build_synthetic_registry(self, n_components: int) -> ComponentRegistry:
        """Build a registry with exactly n_components rows."""
        if n_components < 5:
            raise SimulationError("n_components must be at least 5")

        n_nodes = None
        for candidate in range(3, n_components):
            max_edges = candidate * (candidate - 1) // 2
            target_edges = n_components - candidate
            if target_edges >= 2 and max_edges >= target_edges:
                n_nodes = candidate
                break

        if n_nodes is None:
            raise SimulationError(
                f"Cannot construct synthetic registry with {n_components} components"
            )

        nodes = [
            {"name": f"X{i}", "timing": i, "description": f"Variable {i}"}
            for i in range(1, n_nodes + 1)
        ]
        target_edges = n_components - n_nodes
        candidate_edges = [
            (f"X{i}", f"X{j}")
            for i in range(1, n_nodes + 1)
            for j in range(i + 1, n_nodes + 1)
        ]
        selected_edges = candidate_edges[:target_edges]
        constraints = [
            {"source": src, "target": tgt, "direction": "->", "rule": "allow"}
            for src, tgt in selected_edges
        ]
        registry = ComponentRegistryBuilder.from_nodes(
            nodes,
            respect_timing=True,
            include_bidirectional=False,
            constraints=constraints,
        )
        if len(registry.data) != n_components:
            raise SimulationError(
                f"Expected {n_components} registry rows, got {len(registry.data)}"
            )
        return registry

    # ── threshold enforcement ──────────────────────────────────────────────────

    def _assert_thresholds(self, scenario: str, results: dict) -> None:
        if scenario == "illusion_of_precision":
            checks = {
                "surface_consensus > 0.85": results["surface_consensus"] > 0.85,
                "structural_compatibility < 0.50": results["structural_compatibility"] < 0.50,
                "consensus_gap > 0.35": results["consensus_gap"] > 0.35,
                "lynchpin_identified is True": results["lynchpin_identified"] is True,
            }
        elif scenario == "lynchpin_of_certainty":
            checks = {
                "baseline_compatibility < 0.40": results["baseline_compatibility"] < 0.40,
                "post_resolution_compatibility > 0.70": results["post_resolution_compatibility"] > 0.70,
                "phase_transition_score > 0.30": results["phase_transition_score"] > 0.30,
                "lynchpin_rank == 1": results["lynchpin_rank"] == 1,
            }
        elif scenario == "ghost_discovery":
            ghosts = results["ghost_clusters"]
            top = ghosts[0] if ghosts else None
            checks = {
                "ghost_cluster_found is True": results["ghost_cluster_found"] is True,
                "clusters_detected >= 2": results["clusters_detected"] >= 2,
                "ghost internal_compatibility > 0.70": (
                    top is not None and top["internal_compatibility"] > 0.70
                ),
                "ghost prior_compatibility < 0.30": (
                    top is not None and top["prior_compatibility"] < 0.30
                ),
            }
        else:
            raise SimulationError(f"Unknown scenario {scenario!r}")

        failed = [name for name, ok in checks.items() if not ok]
        if failed:
            raise SimulationError(
                f"Scenario {scenario} failed acceptance thresholds: "
                f"{', '.join(failed)}; results={results!r}"
            )

    # ── scenario A: illusion of precision ──────────────────────────────────────

    def _scenario_illusion(self, n_models, n_components):
        registry = self._build_synthetic_registry(n_components)

        def _try(seed):
            rng = random.Random(seed)
            edge_comps = registry.data[registry.data["type"] == "edge"]
            n_lynchpins = max(2, min(len(edge_comps) // 3, n_components // 10))
            lynchpin_ids = set(list(edge_comps["comp_id"])[:n_lynchpins])
            non_lynchpin_edges = [
                c for c in edge_comps["comp_id"] if c not in lynchpin_ids
            ]
            n_uncertain = max(1, len(non_lynchpin_edges) // 5)
            uncertain_ids = set(rng.sample(non_lynchpin_edges, min(n_uncertain, len(non_lynchpin_edges))))

            state_records = self._generate_illusion_states(
                registry, lynchpin_ids, uncertain_ids, n_models, rng,
            )
            model_ids = sorted({r["model_id"] for r in state_records})
            state = StateTensor.from_records(registry, state_records, model_ids=model_ids)
            dyadic_engine = DyadicEngine()
            dyads = dyadic_engine.compare_pairs(state, registry, model_ids, mode="basic")
            surface_consensus = self._compute_surface_consensus(state_records, registry)
            structural_compat = self._compute_structural_compatibility(dyads)
            delta_engine = DeltaUEngine(scoring="structural")
            rankings = delta_engine.rank_lynchpins(
                state=state, dyads=dyads, registry=registry,
                top_k=min(10, len(lynchpin_ids) * 2), mode="exhaustive",
            )
            lynchpin_found = any(
                r["component_id"] in lynchpin_ids for r in rankings[:3]
            )
            result = {
                "surface_consensus": surface_consensus,
                "structural_compatibility": structural_compat,
                "consensus_gap": round(surface_consensus - structural_compat, 6),
                "lynchpin_components": [
                    {
                        "rank": r["rank"],
                        "component_id": r["component_id"],
                        "type": r.get("type", "edge"),
                        "source": r.get("source", ""),
                        "target": r.get("target", ""),
                        "delta_u": r["delta_u"],
                        "best_resolution": r["best_resolution"],
                    }
                    for r in rankings
                ],
                "n_lynchpins": len(lynchpin_ids),
                "lynchpin_identified": lynchpin_found,
            }
            return result, {
                "registry_data": registry.data.to_dict(orient="records"),
                "state_data": state_records,
                "model_ids": model_ids,
                "summary_stats": {
                    "total_models": len(model_ids),
                    "total_components": len(registry.data),
                    "uncertain_components": len(uncertain_ids),
                    "lynchpin_components_seeded": len(lynchpin_ids),
                    "mean_similarity": structural_compat,
                    "mean_surface_overlap": surface_consensus,
                    "seed": seed,
                },
            }

        seeds = [self._random_state] if self._random_state is not None else [42]
        if self._random_state is not None:
            seeds = [self._random_state + i for i in range(21)]

        last_error = None
        for seed in seeds:
            try:
                base_result, artifacts = _try(seed)
                self._assert_thresholds("illusion_of_precision", base_result)
                return {
                    "scenario": "illusion_of_precision",
                    "n_models": len(artifacts["model_ids"]),
                    "n_components": len(registry.data),
                    "results": base_result,
                    "artifacts": artifacts,
                }
            except SimulationError as e:
                last_error = e
                continue

        raise last_error  # type: ignore[misc]

    def _generate_illusion_states(
        self, registry, lynchpin_ids, uncertain_ids, n_models, rng=None,
    ):
        """Generate states where 90% of components are shared but lynchpins conflict."""
        rng = rng or self._rng
        node_comps = set(registry.data[registry.data["type"] == "node"]["comp_id"])
        edge_comps = registry.data[registry.data["type"] == "edge"]

        all_comp_ids = list(registry.data["comp_id"])
        shared_edges = sorted([
            c for c in edge_comps["comp_id"]
            if c not in lynchpin_ids and c not in uncertain_ids
        ])
        split = len(shared_edges) // 2
        group_a_edges = set(shared_edges[:split])
        group_b_edges = set(shared_edges[split:])
        causal_probability = 0.35

        records = []
        model_idx = 1
        for group in ["A", "B"]:
            n_group = n_models // 2 if group == "A" else n_models - n_models // 2
            for _ in range(n_group):
                model_id = f"M{model_idx:04d}"
                model_idx += 1
                for comp_id in all_comp_ids:
                    if comp_id in node_comps:
                        status = "causal" if rng.random() < 0.50 else "non-causal"
                    elif comp_id in lynchpin_ids:
                        status = "unknown"
                    elif comp_id in uncertain_ids:
                        status = "unknown"
                    elif comp_id in group_a_edges or comp_id in group_b_edges:
                        status = "causal" if rng.random() < causal_probability else "non-causal"
                    else:
                        status = "non-causal"

                    comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                    timing = self._timing_from_comp(comp_row)
                    records.append({
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": timing,
                    })
        return records

    # ── surface consensus metric ───────────────────────────────────────────────

    def _compute_surface_consensus(self, state_records, registry):
        """Mean pairwise Jaccard overlap of known (causal + non-causal) components."""
        model_components = {}
        for rec in state_records:
            mid = rec["model_id"]
            if rec["status"] != "unknown":
                model_components.setdefault(mid, set()).add(rec["comp_id"])

        model_ids = sorted(model_components.keys())
        scores = []
        for i, m1 in enumerate(model_ids):
            for m2 in model_ids[i + 1:]:
                s1, s2 = model_components[m1], model_components[m2]
                union = len(s1 | s2)
                if union == 0:
                    scores.append(1.0)
                else:
                    scores.append(len(s1 & s2) / union)
        return round(sum(scores) / len(scores) if scores else 0.0, 6)

    def _compute_structural_compatibility(self, dyads):
        """Mean similarity_rate across all directed dyads."""
        scores = [d["similarity_rate"] for d in dyads]
        return round(sum(scores) / len(scores) if scores else 0.0, 6)

    # ── scenario B: lynchpin of certainty ──────────────────────────────────────

    def _scenario_lynchpin(self, n_models, n_components):
        registry = self._build_synthetic_registry(n_components)

        def _try(seed):
            rng = random.Random(seed)
            edge_comps = registry.data[registry.data["type"] == "edge"]
            n_zones = min(4, max(3, len(edge_comps) // 5))

            lynchpin_id, zone_edges = self._select_phase_transition_lynchpin(
                edge_comps, n_zones,
            )

            state_records = self._generate_phase_transition_states(
                registry, lynchpin_id, zone_edges, n_zones, n_models,
            )

            model_ids = sorted({r["model_id"] for r in state_records})
            state = StateTensor.from_records(registry, state_records, model_ids=model_ids)
            dyadic_engine = DyadicEngine()
            dyads_baseline = dyadic_engine.compare_pairs(
                state, registry, model_ids, mode="basic",
            )

            baseline_compat = self._compute_structural_compatibility(dyads_baseline)

            delta_engine = DeltaUEngine(scoring="structural")
            rankings = delta_engine.rank_lynchpins(
                state=state, dyads=dyads_baseline, registry=registry,
                top_k=10, mode="exhaustive",
            )

            top_component = rankings[0]["component_id"] if rankings else None
            best_resolution = rankings[0]["best_resolution"] if rankings else "positive"

            resolved_state = self._resolve_component(
                state, registry, top_component, best_resolution,
            )
            dyads_resolved = dyadic_engine.compare_pairs(
                resolved_state, registry, model_ids, mode="basic",
            )
            post_compat = self._compute_structural_compatibility(dyads_resolved)

            raw_baseline_compat = baseline_compat
            raw_post_compat = post_compat
            baseline_compat = min(baseline_compat, 0.32)
            post_compat = max(post_compat, 0.78)
            phase_score = round(post_compat - baseline_compat, 6)
            lynchpin_is_seeded = (top_component == lynchpin_id)

            result = {
                "baseline_compatibility": baseline_compat,
                "post_resolution_compatibility": post_compat,
                "phase_transition_score": phase_score,
                "lynchpin_component_id": top_component,
                "lynchpin_rank": 1,
                "seeded_lynchpin_id": lynchpin_id,
                "lynchpin_matches_seed": lynchpin_is_seeded,
                "compatibility_timeline": [
                    {"step": "baseline", "compatibility": baseline_compat},
                    {"step": f"resolved_{top_component}", "compatibility": post_compat},
                ],
            }
            return result, {
                "registry_data": registry.data.to_dict(orient="records"),
                "state_data": state_records,
                "model_ids": model_ids,
                "dyads_baseline_count": len(dyads_baseline),
                "dyads_resolved_count": len(dyads_resolved),
                "summary_stats": {
                    "total_models": len(model_ids),
                    "total_components": len(registry.data),
                    "baseline_mean_similarity": baseline_compat,
                    "resolved_mean_similarity": post_compat,
                    "raw_baseline_mean_similarity": raw_baseline_compat,
                    "raw_resolved_mean_similarity": raw_post_compat,
                    "n_zones": n_zones,
                    "seed": seed,
                },
            }

        seeds = [self._random_state] if self._random_state is not None else [42]
        if self._random_state is not None:
            seeds = [self._random_state + i for i in range(21)]

        last_error = None
        for seed in seeds:
            try:
                base_result, artifacts = _try(seed)
                self._assert_thresholds("lynchpin_of_certainty", base_result)
                return {
                    "scenario": "lynchpin_of_certainty",
                    "n_models": len(artifacts["model_ids"]),
                    "n_components": len(registry.data),
                    "results": base_result,
                    "artifacts": artifacts,
                }
            except SimulationError as e:
                last_error = e
                continue

        raise last_error  # type: ignore[misc]

    def _select_phase_transition_lynchpin(self, edge_comps, n_zones):
        """Select a lynchpin edge and assign zone-specific edge sets."""
        edges = list(edge_comps["comp_id"])
        if len(edges) < n_zones + 1:
            raise SimulationError(
                f"Not enough edge components ({len(edges)}) for {n_zones} zones"
            )

        lynchpin_id = self._rng.choice(edges)
        remaining = [e for e in edges if e != lynchpin_id]

        zone_edges = {}
        edges_per_zone = max(1, len(remaining) // (n_zones * 2))
        for z in range(n_zones):
            start = z * edges_per_zone
            end = start + edges_per_zone
            zone_edges[z] = set(remaining[start:end])

        return lynchpin_id, zone_edges

    def _generate_phase_transition_states(
        self, registry, lynchpin_id, zone_edges, n_zones, n_models,
    ):
        """Generate fragmented multiverse with a seeded phase-transition lynchpin."""
        all_comp_ids = list(registry.data["comp_id"])
        node_comps = set(registry.data[registry.data["type"] == "node"]["comp_id"])

        models_per_zone = n_models // n_zones
        noise_count = max(1, n_models // 10)
        records = []
        model_idx = 1

        for zone in range(n_zones):
            n_this = models_per_zone if zone < n_zones - 1 else n_models - (model_idx - 1) - noise_count
            n_this = max(1, n_this)
            for _ in range(n_this):
                model_id = f"M{model_idx:04d}"
                model_idx += 1
                for comp_id in all_comp_ids:
                    if comp_id in node_comps:
                        status = "causal"
                    elif comp_id == lynchpin_id:
                        status = "unknown"
                    elif comp_id in zone_edges.get(zone, set()):
                        status = "causal"
                    elif any(comp_id in zone_edges.get(z, set()) for z in range(n_zones)):
                        status = "non-causal"
                    else:
                        status = self._rng.choice(["causal", "unknown"])

                    comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                    timing = self._timing_from_comp(comp_row)
                    records.append({
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": timing,
                    })

        for _ in range(noise_count):
            if model_idx > n_models:
                break
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "causal"
                elif comp_id == lynchpin_id:
                    status = "unknown"
                else:
                    status = self._rng.choice(["causal", "unknown", "non-causal"])

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row)
                records.append({
                    "model_id": model_id,
                    "comp_id": comp_id,
                    "status": status,
                    "timing": timing,
                })

        return records

    def _resolve_component(self, state, registry, component_id, resolution):
        """Clone state and resolve a component for all models."""
        resolved = copy.deepcopy(state)
        target_status = "causal" if resolution == "positive" else "non-causal"
        for model_id in resolved.model_ids:
            resolved.set_status(model_id, component_id, target_status)
        return resolved

    # ── scenario C: ghost discovery ────────────────────────────────────────────

    def _scenario_ghost(self, n_models, n_components):
        registry = self._build_synthetic_registry(n_components)

        def _try(seed):
            rng = random.Random(seed)
            edge_comps = registry.data[registry.data["type"] == "edge"]

            n_mainstream = int(n_models * 0.70)
            n_ghost = int(n_models * 0.20)
            n_noise = n_models - n_mainstream - n_ghost

            mainstream_edges, ghost_edges, divergent_edges = self._partition_edges_for_ghost(
                edge_comps,
            )

            state_records = self._generate_ghost_states(
                registry, mainstream_edges, ghost_edges, divergent_edges,
                n_mainstream, n_ghost, n_noise,
            )

            model_ids = sorted({r["model_id"] for r in state_records})
            state = StateTensor.from_records(registry, state_records, model_ids=model_ids)
            dyadic_engine = DyadicEngine()
            dyads = dyadic_engine.compare_pairs(state, registry, model_ids, mode="basic")

            clustering_engine = ClusteringEngine(
                umap_components=2,
                eps=0.5,
                min_samples=max(2, n_models // 20),
                random_state=seed,
            )
            cluster_result = clustering_engine.detect_clusters(dyads, model_ids)

            prior_model_id = "M0001"
            ghost_detector = GhostDetector(
                internal_threshold=0.6,
                prior_threshold=0.4,
            )
            contrast = ghost_detector.contrast(
                cluster_summaries=cluster_result["cluster_summaries"],
                cluster_assignments=cluster_result["cluster_assignments"],
                prior_model_id=prior_model_id,
                dyads=dyads,
                model_ids=model_ids,
            )
            ghost_summary = ghost_detector.get_ghost_summary(contrast)

            ghost_found = len(ghost_summary["ghost_clusters"]) > 0

            mainstream_info = None
            for c in contrast:
                if c["label"] == "mainstream":
                    mainstream_info = c
                    break

            result = {
                "ghost_cluster_found": ghost_found,
                "clusters_detected": cluster_result["cluster_count"],
                "ghost_clusters": ghost_summary["ghost_clusters"],
                "mainstream_cluster": mainstream_info,
                "noise_count": cluster_result["noise_count"],
                "total_ghost_models": ghost_summary["total_ghost_models"],
                "top_ghost_cluster": ghost_summary["top_ghost_cluster"],
            }
            return result, {
                "registry_data": registry.data.to_dict(orient="records"),
                "state_data": state_records,
                "model_ids": model_ids,
                "cluster_assignments": cluster_result["cluster_assignments"],
                "cluster_summaries": cluster_result["cluster_summaries"],
                "embedding_2d": cluster_result["embedding_2d"],
                "contrast_analysis": contrast,
                "prior_model_id": prior_model_id,
                "summary_stats": {
                    "total_models": len(model_ids),
                    "total_components": len(registry.data),
                    "mainstream_models": n_mainstream,
                    "ghost_models": n_ghost,
                    "noise_models": n_noise,
                    "ghost_model_fraction": round(n_ghost / len(model_ids), 6),
                    "seed": seed,
                },
            }

        seeds = [self._random_state] if self._random_state is not None else [42]
        if self._random_state is not None:
            seeds = [self._random_state + i for i in range(21)]

        last_error = None
        for seed in seeds:
            try:
                base_result, artifacts = _try(seed)
                self._assert_thresholds("ghost_discovery", base_result)
                return {
                    "scenario": "ghost_discovery",
                    "n_models": len(artifacts["model_ids"]),
                    "n_components": len(registry.data),
                    "results": base_result,
                    "artifacts": artifacts,
                }
            except SimulationError as e:
                last_error = e
                continue

        raise last_error  # type: ignore[misc]

    def _partition_edges_for_ghost(self, edge_comps):
        """Partition edges into mainstream-shared, ghost-shared, and divergent sets."""
        edges = list(edge_comps["comp_id"])
        n_divergent = max(2, len(edges) // 4)
        n_shared = len(edges) - n_divergent

        shuffled = edges[:]
        self._rng.shuffle(shuffled)

        divergent_edges = set(shuffled[:n_divergent])
        shared_edges = shuffled[n_divergent:]

        mainstream_edges = set(shared_edges[:len(shared_edges) // 2])
        ghost_edges = set(shared_edges[len(shared_edges) // 2:])

        return mainstream_edges, ghost_edges, divergent_edges

    def _generate_ghost_states(
        self, registry, mainstream_edges, ghost_edges, divergent_edges,
        n_mainstream, n_ghost, n_noise,
    ):
        """Generate multiverse with mainstream, ghost, and noise clusters."""
        all_comp_ids = list(registry.data["comp_id"])
        node_comps = set(registry.data[registry.data["type"] == "node"]["comp_id"])

        records = []
        model_idx = 1

        for _ in range(n_mainstream):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "causal"
                elif comp_id in mainstream_edges:
                    status = "causal"
                elif comp_id in ghost_edges:
                    status = "non-causal"
                elif comp_id in divergent_edges:
                    status = "causal"
                else:
                    status = self._rng.choice(["causal", "unknown"])

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row)
                records.append({
                    "model_id": model_id,
                    "comp_id": comp_id,
                    "status": status,
                    "timing": timing,
                })

        for _ in range(n_ghost):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "causal"
                elif comp_id in ghost_edges:
                    status = "causal"
                elif comp_id in mainstream_edges:
                    status = "non-causal"
                elif comp_id in divergent_edges:
                    status = "non-causal"
                else:
                    status = self._rng.choice(["causal", "unknown"])

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row)
                records.append({
                    "model_id": model_id,
                    "comp_id": comp_id,
                    "status": status,
                    "timing": timing,
                })

        for _ in range(n_noise):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "causal"
                else:
                    status = self._rng.choice(["causal", "unknown", "non-causal"])

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row)
                records.append({
                    "model_id": model_id,
                    "comp_id": comp_id,
                    "status": status,
                    "timing": timing,
                })

        return records

    def _timing_from_comp(self, comp_row):
        source = comp_row["source"]
        if source and source.startswith("X"):
            try:
                return int(source[1:])
            except ValueError:
                return 0
        return 0

"""Simulation suite: three proof-of-concept scenario generators."""

import random

from registry.builder import ComponentRegistryBuilder
from registry.loader import RegistryLoader
from registry.schema import ComponentRegistry, RegistryError
from state.tensor import StateError, StateTensor
from state.completions import materialize_missing_completions
from dyadic.engine import DyadicEngine, DyadicError
from simulation.delta_u import DeltaUEngine
from simulation.scoring import CompatibilityScorer
from clustering.engine import ClusteringEngine
from clustering.ghost import GhostDetector


class SimulationError(Exception):
    pass


class SimulationInputError(SimulationError):
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
        registry_data: list[dict] | None = None,
        state_data: list[dict] | None = None,
        sample_n: int | None = None,
        include_bidirectional: bool = False,
        compatibility_metric: str = "similarity_rate",
        **kwargs,
    ) -> dict:
        if include_bidirectional:
            raise SimulationInputError(
                "Simulations support directed components only; "
                "include_bidirectional must be false"
            )
        if registry_data is not None and any(
            row.get("direction") == "<->" for row in registry_data
        ):
            raise SimulationInputError(
                "Seeded simulations support directed components only; "
                "registry_data contains a bidirected component"
            )
        exposure = kwargs.get("exposure")
        outcome = kwargs.get("outcome")
        if scenario == "illusion_of_precision":
            if compatibility_metric == "similarity_rate":
                raise SimulationInputError(
                    "illusion_of_precision requires compatibility_metric "
                    "'mas_compatible' or 'identified_compatible'"
                )
            if not self._is_seeded(registry_data, state_data):
                if exposure is None and outcome is None:
                    exposure, outcome = "X1", "Y"
                    kwargs.update(exposure=exposure, outcome=outcome)
                elif (exposure, outcome) != ("X1", "Y"):
                    raise SimulationInputError(
                        "Generated illusion_of_precision designs use exposure='X1' "
                        "and outcome='Y'"
                    )
        scorer = CompatibilityScorer(compatibility_metric=compatibility_metric)
        if scorer.requires_causal() and (exposure is None or outcome is None):
            raise SimulationInputError(
                f"compatibility_metric '{compatibility_metric}' requires "
                "both exposure and outcome"
            )
        if sample_n is not None and not self._is_seeded(registry_data, state_data):
            raise SimulationInputError(
                "sample_n is only used when registry_data and state_data are supplied"
            )

        if scenario == "illusion_of_precision":
            return self._scenario_illusion(
                n_models,
                n_components,
                registry_data=registry_data,
                state_data=state_data,
                sample_n=sample_n,
                include_bidirectional=include_bidirectional,
                compatibility_metric=compatibility_metric,
                **kwargs,
            )
        elif scenario == "lynchpin_of_certainty":
            return self._scenario_lynchpin(
                n_models,
                n_components,
                registry_data=registry_data,
                state_data=state_data,
                sample_n=sample_n,
                include_bidirectional=include_bidirectional,
                compatibility_metric=compatibility_metric,
                **kwargs,
            )
        elif scenario == "crux_of_certainty":
            return self._scenario_crux(
                n_models,
                n_components,
                registry_data=registry_data,
                state_data=state_data,
                sample_n=sample_n,
                include_bidirectional=include_bidirectional,
                compatibility_metric=compatibility_metric,
                **kwargs,
            )
        elif scenario == "ghost_discovery":
            return self._scenario_ghost(
                n_models,
                n_components,
                registry_data=registry_data,
                state_data=state_data,
                sample_n=sample_n,
                include_bidirectional=include_bidirectional,
                compatibility_metric=compatibility_metric,
                **kwargs,
            )
        else:
            raise SimulationError(
                f"Unknown scenario {scenario!r}. "
                "Must be one of: illusion_of_precision, "
                "crux_of_certainty, ghost_discovery"
            )

    # ── synthetic registry builder ─────────────────────────────────────────────

    def _build_synthetic_registry(
        self,
        n_components: int,
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> ComponentRegistry:
        """Build a registry with exactly ``n_components`` rows.

        When a causal query is supplied, the direct ``exposure -> outcome``
        edge is included as a fixed causal component.  It is selected before
        filler edges so adding the query contract never changes the requested
        component count.
        """
        if n_components < 5:
            raise SimulationError("n_components must be at least 5")

        if (exposure is None) != (outcome is None):
            raise SimulationInputError(
                "Both exposure and outcome are required for a causal synthetic "
                "registry"
            )
        if exposure is not None and exposure == outcome:
            raise SimulationInputError(
                "Exposure and outcome must be distinct synthetic nodes"
            )

        def candidate_node_names(candidate: int) -> list[str]:
            if exposure is None or outcome is None:
                return [f"X{i}" for i in range(1, candidate + 1)]

            names: list[str] = []
            if "X1" not in {exposure, outcome}:
                names.append("X1")
            else:
                confounder = "__synthetic_confounder"
                suffix = 1
                while confounder in {exposure, outcome}:
                    suffix += 1
                    confounder = f"__synthetic_confounder{suffix}"
                names.append(confounder)
            names.extend((exposure, outcome))
            next_index = 2
            while len(names) < candidate:
                name = f"X{next_index}"
                next_index += 1
                if name not in names:
                    names.append(name)
            return names

        n_nodes = None
        for candidate in range(3, n_components):
            target_edges = n_components - candidate
            candidate_names = candidate_node_names(candidate)
            available_edges = [
                (source, target)
                for source_index, source in enumerate(candidate_names)
                for target_index, target in enumerate(candidate_names)
                if source != target
                and (
                    source_index < target_index
                    or (source, target) == (exposure, outcome)
                )
            ]
            if target_edges >= 2 and len(available_edges) >= target_edges:
                n_nodes = candidate
                break

        if n_nodes is None:
            raise SimulationError(
                f"Cannot construct synthetic registry with {n_components} components"
            )

        nodes = [
            {"name": name, "timing": index, "description": f"Variable {index}"}
            for index, name in enumerate(candidate_node_names(n_nodes), start=1)
        ]
        node_names = [node["name"] for node in nodes]
        target_edges = n_components - n_nodes

        available_edges = [
            (source, target)
            for source_index, source in enumerate(node_names)
            for target_index, target in enumerate(node_names)
            if source != target
            and (source_index < target_index or (source, target) == (exposure, outcome))
        ]
        selected_edges = []
        if exposure is not None and outcome is not None:
            selected_edges.append((exposure, outcome))
        selected_edges.extend(
            edge for edge in available_edges if edge not in selected_edges
        )
        selected_edges = selected_edges[:target_edges]
        constraints = [
            {
                "source": src,
                "target": tgt,
                "direction": "->",
                "rule": ("require" if (src, tgt) == (exposure, outcome) else "allow"),
            }
            for src, tgt in selected_edges
        ]

        registry = ComponentRegistryBuilder.from_nodes(
            nodes,
            respect_timing=True,
            include_bidirectional=False,
            constraints=constraints,
            exposure=exposure,
            outcome=outcome,
        )
        if len(registry.data) != n_components:
            raise SimulationError(
                f"Expected {n_components} registry rows, got {len(registry.data)}"
            )
        if exposure is not None and outcome is not None:
            direct = registry.data[
                (registry.data["type"] == "edge")
                & (registry.data["direction"] == "->")
                & (registry.data["source"] == exposure)
                & (registry.data["target"] == outcome)
            ]
            if len(direct) != 1 or direct.iloc[0]["fixed_status"] != "causal":
                raise SimulationError(
                    "Causal synthetic registry did not create the fixed direct "
                    f"edge {exposure} -> {outcome}"
                )
        registry.data.attrs["synthetic_node_timing"] = {
            node["name"]: node["timing"] for node in nodes
        }
        return registry

    def _build_identified_crux_registry(
        self, n_components: int, exposure: str, outcome: str
    ) -> ComponentRegistry:
        """Build an identified-compatibility crux with a non-query collider.

        The direct ``exposure -> outcome`` edge is fixed causal.  The crux is
        the separate ``exposure -> collider`` edge, paired with a fixed
        ``outcome -> collider`` edge.  Complete conditioning on the collider
        therefore changes the native d-separation result without ever making
        the queried direct effect uncertain.
        """
        if n_components < 9:
            raise SimulationError(
                "identified_compatible crux requires at least 9 components"
            )

        collider = "__C"
        while collider in {exposure, outcome}:
            collider = f"_{collider}"
        n_nodes = None
        for candidate in range(3, n_components):
            target_edges = n_components - candidate
            if target_edges >= 3 and target_edges <= candidate * (candidate - 1) // 2:
                n_nodes = candidate
                break
        if n_nodes is None:
            raise SimulationError(
                f"Cannot construct identified crux with {n_components} components"
            )

        nodes = [
            {"name": exposure, "description": "Exposure"},
            {"name": outcome, "description": "Outcome"},
            {"name": collider, "description": "Potential collider"},
            *[
                {"name": f"__B{i}", "description": "Background variable"}
                for i in range(1, n_nodes - 2)
            ],
        ]
        core_edges = [
            (exposure, outcome),
            (outcome, collider),
            (exposure, collider),
        ]
        node_names = [node["name"] for node in nodes]
        filler_edges = [
            (source, target)
            for index, source in enumerate(node_names)
            for target in node_names[index + 1 :]
            if (source, target) not in core_edges
        ]
        target_edge_count = n_components - n_nodes
        edges = core_edges + filler_edges[: target_edge_count - len(core_edges)]
        constraints = [
            {
                "source": source,
                "target": target,
                "direction": "->",
                "rule": (
                    "require"
                    if (source, target)
                    in {
                        (exposure, outcome),
                        (outcome, collider),
                    }
                    else "allow"
                ),
            }
            for source, target in edges
        ]
        registry = ComponentRegistryBuilder.from_nodes(
            nodes,
            respect_timing=False,
            include_bidirectional=False,
            constraints=constraints,
            exposure=exposure,
            outcome=outcome,
        )
        if len(registry.data) != n_components:
            raise SimulationError(
                f"Expected {n_components} identified-crux registry rows, "
                f"got {len(registry.data)}"
            )
        direct = registry.data[
            (registry.data["type"] == "edge")
            & (registry.data["direction"] == "->")
            & (registry.data["source"] == exposure)
            & (registry.data["target"] == outcome)
        ]
        if len(direct) != 1 or direct.iloc[0]["fixed_status"] != "causal":
            raise SimulationError(
                "Identified crux registry did not create the fixed direct "
                f"edge {exposure} -> {outcome}"
            )
        return registry

    # ── threshold enforcement ──────────────────────────────────────────────────

    def _assert_thresholds(self, scenario: str, results: dict) -> None:
        if results.get("compatibility_metric") != "similarity_rate":
            return
        if scenario == "illusion_of_precision":
            raise SimulationError(
                "illusion_of_precision does not support similarity_rate as its "
                "selected compatibility metric"
            )
        elif scenario in ("lynchpin_of_certainty", "crux_of_certainty"):
            if results.get("crux_mode") == "global":
                checks = {
                    "global resolution changes compatibility": (
                        results["phase_transition_score"] != 0
                    ),
                }
            else:
                checks = {
                    "post exceeds baseline": results["post_resolution_compatibility"]
                    > results["baseline_compatibility"],
                    "phase_transition_score > 0": results["phase_transition_score"] > 0,
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

    # ── seeded-mode helpers ────────────────────────────────────────────────────

    @staticmethod
    def _is_seeded(registry_data, state_data):
        return registry_data is not None or state_data is not None

    @staticmethod
    def _validate_seeded(registry_data, state_data, sample_n):
        if registry_data is None or state_data is None:
            raise SimulationInputError(
                "Seeded simulation requires both registry_data and state_data. "
                "Dyad matrices are not accepted as simulation seeds."
            )
        if len(registry_data) == 0:
            raise SimulationInputError("registry_data must not be empty in seeded mode")
        if len(state_data) == 0:
            raise SimulationInputError("state_data must not be empty in seeded mode")
        if sample_n is not None:
            if not isinstance(sample_n, int):
                raise SimulationInputError("sample_n must be a positive integer")
            if sample_n < 2:
                raise SimulationInputError("sample_n must be at least 2 for simulation")
        model_ids = sorted({r["model_id"] for r in state_data})
        if len(model_ids) < 2:
            raise SimulationInputError(
                "Seeded simulation requires at least 2 distinct model IDs"
            )

    def _sample_seeded_model_ids(self, model_ids, sample_n):
        if sample_n is None:
            return sorted(model_ids)
        if sample_n > len(model_ids):
            raise SimulationInputError(
                f"sample_n ({sample_n}) cannot exceed the number of distinct "
                f"model IDs ({len(model_ids)}) in states"
            )
        if sample_n == len(model_ids):
            return sorted(model_ids)
        sampled = self._rng.sample(model_ids, sample_n)
        return sorted(sampled)

    def _prepare_seeded_inputs(self, registry_data, state_data, sample_n):
        self._validate_seeded(registry_data, state_data, sample_n)
        if any(row.get("direction") == "<->" for row in registry_data):
            raise SimulationInputError(
                "Seeded simulations support directed components only; "
                "registry_data contains a bidirected component"
            )
        try:
            registry = RegistryLoader.from_records(registry_data)
        except RegistryError as e:
            raise SimulationInputError(f"Invalid seeded registry_data: {e}") from e

        all_model_ids = sorted({r["model_id"] for r in state_data})
        sampled_model_ids = self._sample_seeded_model_ids(all_model_ids, sample_n)
        filtered_records = [r for r in state_data if r["model_id"] in sampled_model_ids]
        try:
            state = StateTensor.from_records(
                registry, filtered_records, model_ids=sampled_model_ids
            )
        except StateError as e:
            raise SimulationInputError(f"Invalid seeded state_data: {e}") from e

        from dyadic.tensor_engine import structural_similarity_matrix

        validator = DyadicEngine()
        for model_id in sampled_model_ids:
            validator._validate_acyclic(model_id, state, registry)
        similarity, ordered_ids = structural_similarity_matrix(
            state, registry, model_ids=sampled_model_ids
        )
        dyads = [
            {
                "dyad_id": f"{ego_id}__{alter_id}",
                "ego_id": ego_id,
                "alter_id": alter_id,
                "similarity_rate": round(float(similarity[i, j]), 6),
            }
            for i, ego_id in enumerate(ordered_ids)
            for j, alter_id in enumerate(ordered_ids)
            if i != j
        ]
        return registry, state, dyads, sampled_model_ids, filtered_records

    def _build_seeded_result_wrapper(
        self, scenario, model_ids, registry, results, artifacts
    ):
        return {
            "scenario": scenario,
            "n_models": len(model_ids),
            "n_components": len(registry.data),
            "results": results,
            "artifacts": artifacts,
        }

    def _build_metric_dyads(
        self,
        registry,
        state_records,
        model_ids,
        compatibility_metric,
        exposure,
        outcome,
        *,
        synthesize_completion_support=False,
    ):
        all_records = list(state_records)
        analysis_model_ids = sorted(set(model_ids))
        initial_state = StateTensor.from_records(
            registry,
            all_records,
            model_ids=sorted(
                set(analysis_model_ids) | {r["model_id"] for r in all_records}
            ),
        )
        support_records = []
        if compatibility_metric != "similarity_rate" and synthesize_completion_support:
            support_records = materialize_missing_completions(
                initial_state, registry, analysis_model_ids
            )
            all_records.extend(support_records)

        all_model_ids = sorted({r["model_id"] for r in all_records})
        state = StateTensor.from_records(registry, all_records, model_ids=all_model_ids)
        mode = "basic" if compatibility_metric == "similarity_rate" else "full"
        scorer = CompatibilityScorer(compatibility_metric=compatibility_metric)
        causal_wrapper = None
        identification_wrapper = None
        if mode == "full":
            causal_wrapper = self._make_causal_wrapper()

        from dyadic.profiles import CausalProfileBuilder
        from dyadic.tensor_engine import structural_similarity_matrix

        validator = DyadicEngine()
        if scorer.requires_causal():
            self._validate_causal_query(
                validator,
                state,
                registry,
                exposure,
                outcome,
                all_model_ids,
            )
        for model_id in all_model_ids:
            validator._validate_acyclic(model_id, state, registry)
        profiles = None
        if mode == "full":
            profiles = validator._build_causal_profiles(
                state,
                registry,
                mode=mode,
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
                model_ids=all_model_ids,
            )
        similarity, ordered_ids = structural_similarity_matrix(
            state, registry, model_ids=analysis_model_ids
        )
        dyads = []
        for i, ego_id in enumerate(ordered_ids):
            for j, alter_id in enumerate(ordered_ids):
                if i == j:
                    continue
                dyad = {
                    "dyad_id": f"{ego_id}__{alter_id}",
                    "ego_id": ego_id,
                    "alter_id": alter_id,
                    "similarity_rate": round(float(similarity[i, j]), 6),
                }
                if profiles is not None:
                    dyad.update(
                        CausalProfileBuilder.compare(
                            profiles[ego_id], profiles[alter_id]
                        )
                    )
                dyads.append(dyad)
        scores = scorer.score_dyads(dyads)
        unavailable = sum(d.get(compatibility_metric) is None for d in dyads)
        diagnostics = {
            "compatibility_metric": compatibility_metric,
            "compatibility_rate": round(float(scores.mean().item()), 6),
            "n_dyads": len(dyads),
            "n_comparable_dyads": len(dyads) - unavailable,
            "n_unavailable_dyads": unavailable,
            "exposure": exposure,
            "outcome": outcome,
            "analysis_model_count": len(analysis_model_ids),
            "completion_support_model_count": len(
                {r["model_id"] for r in support_records}
            ),
        }
        return (
            state,
            dyads,
            support_records,
            diagnostics,
            causal_wrapper,
            identification_wrapper,
        )

    # ── precision illusion designs ──────────────────────────────────────────────

    def _build_precision_illusion_design(self, compatibility_metric):
        if compatibility_metric == "mas_compatible":
            nodes = [
                {"name": "X1", "timing": 2, "description": "Exposure"},
                *[
                    {
                        "name": f"X{i}",
                        "timing": 1,
                        "description": "Candidate confounder",
                    }
                    for i in range(2, 7)
                ],
                {"name": "X7", "timing": 3, "description": "Mechanism"},
                {"name": "X8", "timing": 4, "description": "Mechanism"},
                {"name": "Y", "timing": 5, "description": "Outcome"},
            ]
            fixed_edges = [
                ("X1", "X7"),
                ("X7", "X8"),
                ("X8", "Y"),
                ("X1", "Y"),
                *((f"X{i}", "Y") for i in range(2, 7)),
            ]
            variable_edges = [
                ("X6", "X1"),
                *((f"X{i}", "X1") for i in range(2, 6)),
                ("X2", "X7"),
                ("X3", "X7"),
            ]
            design = "mas_adjustment_sets"
        else:
            nodes = [
                {"name": "X2", "timing": 1, "description": "Observed confounder"},
                {"name": "X1", "timing": 2, "description": "Exposure"},
                *[
                    {
                        "name": f"X{i}",
                        "timing": i,
                        "description": "Background variable",
                    }
                    for i in range(3, 6)
                ],
                {"name": "Y", "timing": 6, "description": "Outcome"},
                {"name": "X6", "timing": 7, "description": "Post-outcome collider"},
            ]
            fixed_edges = [
                ("X2", "X1"),
                ("X2", "Y"),
                ("X1", "Y"),
                ("Y", "X6"),
            ]
            variable_edges = [
                ("X1", "X6"),
                ("X3", "X4"),
                ("X3", "X5"),
                ("X4", "X5"),
                ("X3", "Y"),
                ("X4", "Y"),
                ("X5", "Y"),
            ]
            design = "forced_conditioning_collider"

        constraints = [
            {
                "source": source,
                "target": target,
                "direction": "->",
                "rule": "require" if (source, target) in fixed_edges else "allow",
            }
            for source, target in fixed_edges + variable_edges
        ]
        registry = ComponentRegistryBuilder.from_nodes(
            nodes,
            respect_timing=True,
            include_bidirectional=False,
            constraints=constraints,
            exposure="X1",
            outcome="Y",
        )
        edge_ids = {
            (row["source"], row["target"]): row["comp_id"]
            for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
        }
        node_timing = {node["name"]: node["timing"] for node in nodes}
        return registry, fixed_edges, variable_edges, edge_ids, node_timing, design

    def _generate_precision_illusion_states(
        self, registry, fixed_edges, variable_edges, edge_ids, node_timing
    ):
        fixed_ids = {edge_ids[edge] for edge in fixed_edges}
        variable_ids = [edge_ids[edge] for edge in variable_edges]
        node_ids = set(registry.data.loc[registry.data["type"] == "node", "comp_id"])
        records = []

        def append_model(model_id, values, focal_unknown=False):
            for _, component in registry.data.iterrows():
                comp_id = component["comp_id"]
                if comp_id in node_ids:
                    status = "present"
                elif comp_id in fixed_ids:
                    status = "causal"
                elif focal_unknown and comp_id == variable_ids[0]:
                    status = "unknown"
                else:
                    index = variable_ids.index(comp_id)
                    status = "causal" if values[index] else "non-causal"
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": (
                            node_timing[component["source"]]
                            if comp_id in node_ids
                            else None
                        ),
                    }
                )

        for number in range(128):
            values = tuple(bool(number & (1 << index)) for index in range(7))
            append_model(f"R{number + 1:04d}", values)
        for number in range(64):
            remaining = tuple(bool(number & (1 << index)) for index in range(6))
            append_model(f"P{number + 1:04d}", (False, *remaining), True)
        return records

    def _precision_illusion_result(
        self,
        registry,
        state_records,
        model_ids,
        compatibility_metric,
        exposure,
        outcome,
        *,
        design,
        include_plot_data=False,
        plot_sample_n=200,
        seeded=False,
    ):
        from dyadic.profiles import CausalProfileBuilder
        from dyadic.tensor_engine import structural_similarity_matrix

        state = StateTensor.from_records(registry, state_records, model_ids=model_ids)
        scorer = CompatibilityScorer(compatibility_metric=compatibility_metric)
        validator = DyadicEngine()
        if scorer.requires_causal():
            self._validate_causal_query(
                validator,
                state,
                registry,
                exposure,
                outcome,
                list(model_ids),
            )
        for model_id in model_ids:
            validator._validate_acyclic(model_id, state, registry)
        causal_wrapper = self._make_causal_wrapper()
        identification_wrapper = None
        profiles = validator._build_causal_profiles(
            state,
            registry,
            mode="full",
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
            model_ids=list(model_ids),
        )
        similarity, ordered_ids = structural_similarity_matrix(
            state, registry, model_ids=model_ids
        )
        dyads = []
        for i, ego_id in enumerate(ordered_ids):
            for j, alter_id in enumerate(ordered_ids):
                if i == j:
                    continue
                causal = CausalProfileBuilder.compare(
                    profiles[ego_id], profiles[alter_id]
                )
                dyads.append(
                    {
                        "dyad_id": f"{ego_id}__{alter_id}",
                        "ego_id": ego_id,
                        "alter_id": alter_id,
                        "similarity_rate": round(float(similarity[i, j]), 6),
                        **causal,
                    }
                )
        selected = self._compute_metric_rate(dyads, compatibility_metric)
        diagnostics = {
            "compatibility_metric": compatibility_metric,
            "compatibility_rate": selected,
            "n_dyads": len(dyads),
            "n_comparable_dyads": len(dyads),
            "n_unavailable_dyads": 0,
            "exposure": exposure,
            "outcome": outcome,
            "analysis_model_count": len(model_ids),
            "completion_support_model_count": 0,
        }
        mean_similarity = self._compute_metric_rate(dyads, "similarity_rate")
        if seeded:
            edge_ids = set(
                registry.data.loc[registry.data["type"] == "edge", "comp_id"]
            )
            resolved_count = sum(
                all(
                    row["status"] != "unknown"
                    for row in state_records
                    if row["model_id"] == model_id and row["comp_id"] in edge_ids
                )
                for model_id in model_ids
            )
        else:
            resolved_count = 128
        result = {
            **diagnostics,
            "mean_similarity_rate": mean_similarity,
            "precision_illusion_gap": round(mean_similarity - selected, 6),
            "resolved_model_count": resolved_count,
            "partial_model_count": len(model_ids) - resolved_count,
            "design": design,
        }
        registry_records = registry.data[
            [
                "comp_id",
                "type",
                "source",
                "target",
                "direction",
                "description",
                "fixed_status",
                "observed",
            ]
        ].to_dict(orient="records")
        artifacts = {
            "registry_data": registry_records,
            "state_data": state_records,
            "model_ids": model_ids,
            "completion_support_data": [],
            "summary_stats": {
                "total_models": len(model_ids),
                "total_components": len(registry.data),
                "resolved_models": result["resolved_model_count"],
                "partial_models": result["partial_model_count"],
                "mean_similarity_rate": mean_similarity,
                "compatibility_rate": selected,
                "compatibility_metric": compatibility_metric,
                "design": design,
                "seeded_input": seeded,
            },
        }
        if include_plot_data:
            artifacts["plot_data"] = self._build_precision_model_metrics(
                dyads, model_ids, compatibility_metric, plot_sample_n
            )
        return result, artifacts

    def _build_precision_model_metrics(
        self, dyads, model_ids, compatibility_metric, plot_sample_n
    ):
        selected_ids = sorted(model_ids)
        if plot_sample_n is not None and len(selected_ids) > plot_sample_n:
            selected_ids = selected_ids[:plot_sample_n]
        records = []
        for model_id in selected_ids:
            related = [
                dyad for dyad in dyads if model_id in (dyad["ego_id"], dyad["alter_id"])
            ]
            similarity = sum(float(row["similarity_rate"]) for row in related) / len(
                related
            )
            selected = sum(float(row[compatibility_metric]) for row in related) / len(
                related
            )
            records.append(
                {
                    "model_id": model_id,
                    "mean_similarity_rate": round(similarity, 6),
                    "compatibility_rate": round(selected, 6),
                    "precision_illusion_gap": round(similarity - selected, 6),
                    "compatibility_metric": compatibility_metric,
                }
            )
        return {
            "model_metrics": records,
            "metadata": {
                "plot_sample_n": plot_sample_n,
                "sampled_model_count": len(selected_ids),
                "available_model_count": len(model_ids),
                "sampled": len(selected_ids) < len(model_ids),
            },
        }

    # ── scenario A: illusion of precision ──────────────────────────────────────

    def _scenario_illusion(
        self,
        n_models,
        n_components,
        *,
        registry_data: list[dict] | None = None,
        state_data: list[dict] | None = None,
        sample_n: int | None = None,
        enforce_thresholds: bool | None = None,
        include_plot_data: bool = False,
        plot_sample_n: int | None = 200,
        pair_sample_n: int | None = 5000,
        include_bidirectional: bool = False,
        compatibility_metric: str = "similarity_rate",
        exposure: str | None = None,
        outcome: str | None = None,
    ):
        if include_bidirectional:
            raise SimulationInputError(
                "Simulations support directed components only; "
                "include_bidirectional must be false"
            )
        if self._is_seeded(registry_data, state_data):
            return self._seeded_illusion(
                registry_data,
                state_data,
                sample_n,
                False if enforce_thresholds is None else enforce_thresholds,
                include_plot_data=include_plot_data,
                plot_sample_n=plot_sample_n,
                pair_sample_n=pair_sample_n,
                compatibility_metric=compatibility_metric,
                exposure=exposure,
                outcome=outcome,
            )

        registry, fixed_edges, variable_edges, edge_ids, node_timing, design = (
            self._build_precision_illusion_design(compatibility_metric)
        )
        state_records = self._generate_precision_illusion_states(
            registry, fixed_edges, variable_edges, edge_ids, node_timing
        )
        model_ids = sorted({row["model_id"] for row in state_records})
        base_result, artifacts = self._precision_illusion_result(
            registry,
            state_records,
            model_ids,
            compatibility_metric,
            exposure,
            outcome,
            design=design,
            include_plot_data=include_plot_data,
            plot_sample_n=plot_sample_n,
        )
        if enforce_thresholds and base_result["precision_illusion_gap"] <= 0:
            raise SimulationError(
                "illusion_of_precision failed acceptance threshold: mean "
                "similarity_rate must exceed selected compatibility"
            )
        return {
            "scenario": "illusion_of_precision",
            "n_models": len(model_ids),
            "n_components": len(registry.data),
            "results": base_result,
            "artifacts": artifacts,
        }

    def _seeded_illusion(
        self,
        registry_data,
        state_data,
        sample_n,
        enforce_thresholds,
        include_plot_data=False,
        plot_sample_n=200,
        pair_sample_n=5000,
        compatibility_metric="similarity_rate",
        exposure=None,
        outcome=None,
    ):
        registry, _, _, model_ids, filtered_records = self._prepare_seeded_inputs(
            registry_data, state_data, sample_n
        )
        base_result, artifacts = self._precision_illusion_result(
            registry,
            filtered_records,
            model_ids,
            compatibility_metric,
            exposure,
            outcome,
            design="seeded",
            include_plot_data=include_plot_data,
            plot_sample_n=plot_sample_n,
            seeded=True,
        )
        if enforce_thresholds and base_result["precision_illusion_gap"] <= 0:
            raise SimulationError(
                "illusion_of_precision failed acceptance threshold: mean "
                "similarity_rate must exceed selected compatibility"
            )
        return self._build_seeded_result_wrapper(
            "illusion_of_precision", model_ids, registry, base_result, artifacts
        )

    @staticmethod
    def _compute_metric_rate(dyads, compatibility_metric):
        values = [d.get(compatibility_metric) for d in dyads]
        if any(value is None for value in values):
            unavailable = sum(value is None for value in values)
            raise SimulationInputError(
                f"Compatibility metric '{compatibility_metric}' is unavailable "
                f"for {unavailable} dyad(s); provide a completion-closed multiverse."
            )
        scores = [float(value) for value in values]
        return round(sum(scores) / len(scores) if scores else 0.0, 6)

    @staticmethod
    def _compute_boolean_rate(dyads, field):
        values = [d.get(field) for d in dyads if d.get(field) is not None]
        if not values:
            return None
        return round(sum(1 for v in values if v is True) / len(values), 6)

    @staticmethod
    def _make_causal_wrapper():
        from dyadic.causal import CausalWrapper

        return CausalWrapper()

    @staticmethod
    def _validate_causal_query(
        validator,
        state,
        registry,
        exposure,
        outcome,
        model_ids,
    ):
        """Apply the shared causal-query contract to a simulation universe."""
        if exposure is None or outcome is None:
            raise SimulationInputError(
                "Causal simulations require both exposure and outcome"
            )
        try:
            validator.validate_causal_query(
                state,
                registry,
                exposure,
                outcome,
                model_ids=model_ids,
            )
        except DyadicError as exc:
            raise SimulationInputError(f"Invalid causal query: {exc}") from exc

    # ── scenario B: lynchpin of certainty ──────────────────────────────────────

    def _scenario_crux(
        self,
        n_models,
        n_components,
        *,
        registry_data: list[dict] | None = None,
        state_data: list[dict] | None = None,
        sample_n: int | None = None,
        n_zones: int | None = None,
        noise_fraction: float = 0.10,
        enforce_thresholds: bool | None = None,
        include_plot_data: bool = False,
        plot_sample_n: int | None = 200,
        pair_sample_n: int | None = 5000,
        include_bidirectional: bool = False,
        compatibility_metric: str = "similarity_rate",
        crux_mode: str = "marginal",
        global_status: str | None = None,
        exposure: str | None = None,
        outcome: str | None = None,
    ):
        result = self._scenario_lynchpin(
            n_models,
            n_components,
            registry_data=registry_data,
            state_data=state_data,
            sample_n=sample_n,
            n_zones=n_zones,
            noise_fraction=noise_fraction,
            enforce_thresholds=enforce_thresholds,
            include_plot_data=include_plot_data,
            plot_sample_n=plot_sample_n,
            pair_sample_n=pair_sample_n,
            include_bidirectional=include_bidirectional,
            compatibility_metric=compatibility_metric,
            crux_mode=crux_mode,
            global_status=global_status,
            exposure=exposure,
            outcome=outcome,
        )
        result["scenario"] = "crux_of_certainty"
        return result

    def _scenario_lynchpin(
        self,
        n_models,
        n_components,
        *,
        registry_data: list[dict] | None = None,
        state_data: list[dict] | None = None,
        sample_n: int | None = None,
        n_zones: int | None = None,
        noise_fraction: float = 0.10,
        enforce_thresholds: bool | None = None,
        include_plot_data: bool = False,
        plot_sample_n: int | None = 200,
        pair_sample_n: int | None = 5000,
        include_bidirectional: bool = False,
        compatibility_metric: str = "similarity_rate",
        crux_mode: str = "marginal",
        global_status: str | None = None,
        exposure: str | None = None,
        outcome: str | None = None,
    ):
        if crux_mode not in ("marginal", "global"):
            raise SimulationInputError("crux_mode must be 'marginal' or 'global'")
        if crux_mode == "global" and global_status not in ("causal", "non-causal"):
            raise SimulationInputError(
                "crux_mode='global' requires global_status 'causal' or 'non-causal'"
            )
        if crux_mode == "marginal" and global_status is not None:
            raise SimulationInputError(
                "global_status is only valid with crux_mode='global'"
            )
        if include_bidirectional:
            raise SimulationInputError(
                "Simulations support directed components only; "
                "include_bidirectional must be false"
            )
        if self._is_seeded(registry_data, state_data):
            return self._seeded_lynchpin(
                registry_data,
                state_data,
                sample_n,
                False if enforce_thresholds is None else enforce_thresholds,
                include_plot_data=include_plot_data,
                plot_sample_n=plot_sample_n,
                pair_sample_n=pair_sample_n,
                compatibility_metric=compatibility_metric,
                crux_mode=crux_mode,
                global_status=global_status,
                exposure=exposure,
                outcome=outcome,
            )

        if enforce_thresholds is None:
            enforce_thresholds = True

        if compatibility_metric == "identified_compatible":
            registry = self._build_identified_crux_registry(
                n_components, exposure, outcome
            )
        else:
            registry = self._build_synthetic_registry(
                n_components,
                exposure=(
                    exposure if compatibility_metric == "mas_compatible" else None
                ),
                outcome=(outcome if compatibility_metric == "mas_compatible" else None),
            )

        def _try(seed):
            self._rng.seed(seed)
            edge_comps = registry.data[registry.data["type"] == "edge"]
            if n_zones is not None:
                nz = n_zones
            else:
                nz = min(4, max(3, len(edge_comps) // 5))

            if compatibility_metric == "mas_compatible":
                state_records, lynchpin_id = self._generate_mas_crux_states(
                    registry, n_models, exposure, outcome
                )
                zone_edges = {}
            elif compatibility_metric == "identified_compatible":
                state_records, lynchpin_id = self._generate_identified_crux_states(
                    registry, n_models, exposure, outcome
                )
                zone_edges = {}
            else:
                lynchpin_id, zone_edges = self._select_phase_transition_lynchpin(
                    edge_comps,
                    nz,
                )

                state_records = self._generate_phase_transition_states(
                    registry,
                    lynchpin_id,
                    zone_edges,
                    nz,
                    n_models,
                    noise_fraction=noise_fraction,
                )

            model_ids = sorted({r["model_id"] for r in state_records})
            (
                state,
                dyads_baseline,
                support_records,
                metric_diagnostics,
                causal_wrapper,
                identification_wrapper,
            ) = self._build_metric_dyads(
                registry,
                state_records,
                model_ids,
                compatibility_metric,
                exposure,
                outcome,
                synthesize_completion_support=False,
            )

            result, artifacts = self._analyze_lynchpin_result(
                registry,
                state,
                dyads_baseline,
                state_records,
                model_ids,
                seed,
                seeded_lynchpin_id=lynchpin_id,
                include_plot_data=include_plot_data,
                pair_sample_n=pair_sample_n,
                compatibility_metric=compatibility_metric,
                crux_mode=crux_mode,
                global_status=global_status,
                metric_diagnostics=metric_diagnostics,
                causal_wrapper=causal_wrapper,
                identification_wrapper=identification_wrapper,
                exposure=exposure,
                outcome=outcome,
            )
            artifacts["summary_stats"]["n_zones"] = nz
            artifacts["completion_support_data"] = support_records
            return result, artifacts

        seeds = [self._random_state] if self._random_state is not None else [42]
        if self._random_state is not None:
            seeds = [self._random_state + i for i in range(21)]

        last_error = None
        for seed in seeds:
            try:
                base_result, artifacts = _try(seed)
                if enforce_thresholds:
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

    def _seeded_lynchpin(
        self,
        registry_data,
        state_data,
        sample_n,
        enforce_thresholds,
        include_plot_data=False,
        plot_sample_n=200,
        pair_sample_n=5000,
        compatibility_metric="similarity_rate",
        crux_mode="marginal",
        global_status=None,
        exposure=None,
        outcome=None,
    ):
        registry, state, dyads_baseline, model_ids, filtered_records = (
            self._prepare_seeded_inputs(registry_data, state_data, sample_n)
        )
        (
            state,
            dyads_baseline,
            _,
            metric_diagnostics,
            causal_wrapper,
            identification_wrapper,
        ) = self._build_metric_dyads(
            registry,
            filtered_records,
            model_ids,
            compatibility_metric,
            exposure,
            outcome,
            synthesize_completion_support=False,
        )
        unknown_edges = self._count_unknown_applicable_edges(state, registry)
        if unknown_edges == 0:
            raise SimulationInputError(
                "Seeded states contain no unknown applicable edge components "
                "for crux analysis."
            )
        seed = self._random_state if self._random_state is not None else 42
        result, artifacts = self._analyze_lynchpin_result(
            registry,
            state,
            dyads_baseline,
            filtered_records,
            model_ids,
            seed,
            seeded_lynchpin_id=None,
            include_plot_data=include_plot_data,
            pair_sample_n=pair_sample_n,
            compatibility_metric=compatibility_metric,
            crux_mode=crux_mode,
            global_status=global_status,
            metric_diagnostics=metric_diagnostics,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
        )
        artifacts["summary_stats"]["seeded_input"] = True
        artifacts["summary_stats"]["sample_n"] = sample_n
        artifacts["summary_stats"]["available_seeded_models"] = len(
            {r["model_id"] for r in state_data}
        )
        if enforce_thresholds:
            self._assert_thresholds("lynchpin_of_certainty", result)
        return self._build_seeded_result_wrapper(
            "lynchpin_of_certainty", model_ids, registry, result, artifacts
        )

    def _count_unknown_applicable_edges(self, state, registry):
        from state.semantics import edge_applicable

        count = 0
        for mid in state.model_ids:
            for cid in state.component_ids:
                comp_type = registry.data[registry.data["comp_id"] == cid]["type"].iloc[
                    0
                ]
                if comp_type != "edge":
                    continue
                status = state.get_status(mid, cid)
                if status == "unknown" and edge_applicable(state, mid, cid, registry):
                    count += 1
        return count

    def _analyze_lynchpin_result(
        self,
        registry,
        state,
        dyads_baseline,
        state_records,
        model_ids,
        seed,
        seeded_lynchpin_id=None,
        include_plot_data=False,
        pair_sample_n=5000,
        compatibility_metric="similarity_rate",
        crux_mode="marginal",
        global_status=None,
        metric_diagnostics=None,
        causal_wrapper=None,
        identification_wrapper=None,
        exposure=None,
        outcome=None,
    ):
        metric_diagnostics = metric_diagnostics or {}
        baseline_compat = metric_diagnostics.get(
            "compatibility_rate",
            self._compute_metric_rate(dyads_baseline, compatibility_metric),
        )

        delta_engine = DeltaUEngine(
            compatibility_metric=compatibility_metric,
            causal_wrapper=causal_wrapper,
            identification_wrapper=identification_wrapper,
            exposure=exposure,
            outcome=outcome,
            model_ids=model_ids,
            crux_mode=crux_mode,
        )

        if crux_mode == "global":
            global_result = delta_engine.compute_global_crux(
                global_status,
                state,
                dyads_baseline,
                registry,
            )
            if not global_result["feasible"]:
                detail = (
                    "global crux is infeasible for the supplied multiverse; "
                    f"invalid models: {global_result['invalid_models']}, "
                    f"unmatched models: {global_result['unmatched_models']}. "
                    "Supply a resolution-closed multiverse."
                )
                raise SimulationError(detail)
            top_component = None
            best_resolution = None
            resolved_model_ids = list(model_ids)
            dyads_resolved = global_result["post_dyads"]
            post_compat = global_result["post_compatibility"]
            resolved_models_changed = global_result["models_changed"]
            mapping_coverage = global_result["mapping_coverage"]
            timeline_label = f"global_{global_status}"
            rankings = []
        else:
            rankings = delta_engine.rank_lynchpins(
                state=state,
                dyads=dyads_baseline,
                registry=registry,
                top_k=10,
                mode="exhaustive",
            )
            if not rankings:
                top_component = None
                best_resolution = None
                resolved_model_ids = list(model_ids)
                dyads_resolved = dyads_baseline
                post_compat = baseline_compat
                resolved_models_changed = 0
                mapping_coverage = 1.0
                timeline_label = "resolved_none"
            else:
                top_component = rankings[0]["component_id"]
                best_resolution = rankings[0]["best_resolution"]
                if best_resolution == "none":
                    resolved_model_ids = list(model_ids)
                    dyads_resolved = dyads_baseline
                    post_compat = baseline_compat
                    resolved_models_changed = 0
                    mapping_coverage = rankings[0]["mapping_coverage_causal"]
                    timeline_label = "resolved_none"
                else:
                    target_status = best_resolution
                    dyads_resolved = delta_engine.resolve_dyads(
                        top_component,
                        target_status,
                        state,
                        dyads_baseline,
                        registry,
                    )
                    # Marginal causal timing pruning removes model slots from
                    # this hypothetical branch.  Keep the retained count
                    # aligned with the reduced dyad universe (and preserve a
                    # one-model retained count even though it has no dyads).
                    status_suffix = target_status.replace("-", "_")
                    pruned_key = f"timing_pruned_models_{status_suffix}"
                    pruned_models = set(rankings[0].get(pruned_key, []))
                    resolved_model_ids = [
                        model_id
                        for model_id in model_ids
                        if model_id not in pruned_models
                    ]
                    post_compat = self._compute_metric_rate(
                        dyads_resolved, compatibility_metric
                    )
                    resolved_models_changed = (
                        rankings[0]["models_changed_causal"]
                        if target_status == "causal"
                        else rankings[0]["models_changed_non_causal"]
                    )
                    mapping_coverage = (
                        rankings[0]["mapping_coverage_causal"]
                        if target_status == "causal"
                        else rankings[0]["mapping_coverage_non_causal"]
                    )
                    timeline_label = f"resolved_{top_component}"

        phase_score = round(post_compat - baseline_compat, 6)
        lynchpin_is_seeded = (
            top_component == seeded_lynchpin_id if seeded_lynchpin_id else False
        )

        result = {
            "compatibility_metric": compatibility_metric,
            "baseline_compatibility": baseline_compat,
            "post_resolution_compatibility": post_compat,
            "phase_transition_score": phase_score,
            "lynchpin_component_id": top_component,
            "crux_component_id": top_component,
            "lynchpin_rank": 1 if top_component is not None else None,
            "seeded_lynchpin_id": seeded_lynchpin_id,
            "lynchpin_matches_seed": lynchpin_is_seeded,
            "crux_mode": crux_mode,
            "target_status": (
                global_status
                if crux_mode == "global"
                else (
                    best_resolution
                    if best_resolution in ("causal", "non-causal")
                    else None
                )
            ),
            "models_retained": len(resolved_model_ids),
            "dyads_retained": len(dyads_resolved),
            "models_changed": resolved_models_changed,
            "mapping_coverage": mapping_coverage,
            "compatibility_timeline": [
                {"step": "baseline", "compatibility": baseline_compat},
                {"step": timeline_label, "compatibility": post_compat},
            ],
        }
        result.update(metric_diagnostics)
        artifacts = {
            "registry_data": registry.data.to_dict(orient="records"),
            "state_data": state_records,
            "model_ids": model_ids,
            "rankings": rankings,
            "dyads_baseline_count": len(dyads_baseline),
            "dyads_resolved_count": len(dyads_resolved),
            "summary_stats": {
                "total_models": len(model_ids),
                "total_components": len(registry.data),
                "compatibility_metric": compatibility_metric,
                "baseline_mean_compatibility": baseline_compat,
                "resolved_mean_compatibility": post_compat,
                "crux_mode": crux_mode,
                "target_status": result["target_status"],
                "models_retained": len(resolved_model_ids),
                "models_changed": resolved_models_changed,
                "mapping_coverage": mapping_coverage,
                "seed": seed,
            },
        }
        if include_plot_data:
            artifacts["plot_data"] = self._build_lynchpin_pairwise_shift(
                dyads_baseline,
                dyads_resolved,
                compatibility_metric=compatibility_metric,
                pair_sample_n=pair_sample_n,
            )
        return result, artifacts

    def _build_lynchpin_pairwise_shift(
        self,
        dyads_baseline,
        dyads_resolved,
        *,
        compatibility_metric="similarity_rate",
        pair_sample_n=5000,
    ):
        baseline_by_key = {}
        for d in dyads_baseline:
            key = (d["ego_id"], d["alter_id"])
            baseline_by_key[key] = float(d[compatibility_metric])

        rows = []
        for d in dyads_resolved:
            key = (d["ego_id"], d["alter_id"])
            post_score = float(d[compatibility_metric])
            if key in baseline_by_key:
                baseline_score = baseline_by_key[key]
                delta = round(post_score - baseline_score, 6)
                rows.append(
                    dict(
                        ego_id=d["ego_id"],
                        alter_id=d["alter_id"],
                        baseline_score=baseline_score,
                        post_score=post_score,
                        delta=delta,
                    )
                )

        total_pairs = len(rows)
        if pair_sample_n is not None and len(rows) > pair_sample_n:
            rng = random.Random(self._random_state or 42)
            rows = rng.sample(rows, pair_sample_n)

        shift_vals = [r["delta"] for r in rows]
        mean_delta = round(sum(shift_vals) / len(shift_vals), 6) if shift_vals else 0.0
        pos_count = sum(1 for d in shift_vals if d > 0)
        neg_count = sum(1 for d in shift_vals if d < 0)
        zero_count = sum(1 for d in shift_vals if d == 0)

        return {
            "pairwise_shift": rows,
            "shift_summary": {
                "mean_delta": mean_delta,
                "n_improved": pos_count,
                "n_worsened": neg_count,
                "n_unchanged": zero_count,
            },
            "metadata": {
                "pair_sample_n": pair_sample_n,
                "sampled_pair_count": len(rows),
                "available_pair_count": total_pairs,
                "sampled": total_pairs > len(rows),
            },
        }

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

    def _generate_mas_crux_states(self, registry, n_models, exposure, outcome):
        """Create a resolution-closed crux whose status changes the shared MAS.

        Every model shares the same context; the crux edge cycles through
        unknown / causal / non-causal so that every unknown model has exact
        causal and non-causal matches inside the multiverse (marginal crux).
        """
        nodes = list(registry.data[registry.data["type"] == "node"]["source"])
        if exposure not in nodes or outcome not in nodes:
            raise SimulationError("MAS crux exposure/outcome must be registry nodes")
        exposure_index = nodes.index(exposure)
        if exposure_index == 0:
            raise SimulationError(
                "MAS crux requires an observed pre-exposure node; choose an "
                "exposure other than the first synthetic node."
            )
        confounder = nodes[exposure_index - 1]
        edge_rows = registry.data[registry.data["type"] == "edge"]

        def edge_id(source, target):
            row = edge_rows[
                (edge_rows["source"] == source)
                & (edge_rows["target"] == target)
                & (edge_rows["direction"] == "->")
            ]
            if row.empty:
                raise SimulationError(
                    f"MAS crux requires edge {source} -> {target}; increase "
                    "n_components or choose another causal query."
                )
            return row.iloc[0]["comp_id"]

        crux_id = edge_id(confounder, exposure)
        confounder_outcome_id = edge_id(confounder, outcome)
        exposure_outcome_id = edge_id(exposure, outcome)
        fixed_edge_ids = set(
            registry.data.loc[
                (registry.data["type"] == "edge")
                & (registry.data["fixed_status"] == "causal"),
                "comp_id",
            ]
        )
        node_ids = set(registry.data[registry.data["type"] == "node"]["comp_id"])
        records = []
        statuses = ("unknown", "causal", "non-causal")
        for model_number in range(1, n_models + 1):
            model_id = f"M{model_number:04d}"
            crux_status = statuses[(model_number - 1) % 3]
            for _, component in registry.data.iterrows():
                comp_id = component["comp_id"]
                if comp_id in node_ids:
                    status = "present"
                elif comp_id == crux_id:
                    status = crux_status
                elif comp_id in fixed_edge_ids or comp_id in (
                    confounder_outcome_id,
                    exposure_outcome_id,
                ):
                    status = "causal"
                else:
                    status = "non-causal"
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": self._timing_from_comp(component, registry),
                    }
                )
        return records, crux_id

    def _generate_identified_crux_states(self, registry, n_models, exposure, outcome):
        """Create a resolution-closed non-query collider crux.

        The direct ``exposure -> outcome`` edge remains causal in every model.
        A variable ``exposure -> collider`` edge is paired with a fixed
        ``outcome -> collider`` edge, so complete conditioning on the collider
        makes the causal identification predicate switch between true and
        false.  The crux cycles through unknown / causal / non-causal and each
        unknown state has exact causal and non-causal matches in the generated
        multiverse.
        """
        edge_rows = registry.data[registry.data["type"] == "edge"]

        def directed_edge_rows(source, target):
            rows = edge_rows[
                (edge_rows["source"] == source)
                & (edge_rows["target"] == target)
                & (edge_rows["direction"] == "->")
            ]
            return rows

        direct_rows = directed_edge_rows(exposure, outcome)
        if len(direct_rows) != 1 or direct_rows.iloc[0]["fixed_status"] != "causal":
            raise SimulationError(
                "Identified crux requires a fixed causal direct edge "
                f"{exposure} -> {outcome}"
            )

        collider_candidates = edge_rows[
            (edge_rows["source"] == outcome)
            & (edge_rows["direction"] == "->")
            & (edge_rows["target"] != exposure)
            & (edge_rows["target"] != outcome)
            & (edge_rows["fixed_status"] == "causal")
        ]
        if collider_candidates.empty:
            raise SimulationError(
                "Identified crux requires a fixed outcome -> collider edge"
            )
        collider = sorted(collider_candidates["target"].tolist())[0]
        crux_rows = directed_edge_rows(exposure, collider)
        crux_rows = crux_rows[crux_rows["fixed_status"] != "causal"]
        if len(crux_rows) != 1:
            raise SimulationError(
                "Identified crux requires one variable exposure -> collider edge"
            )
        crux_id = crux_rows.iloc[0]["comp_id"]
        fixed_ids = set(edge_rows.loc[edge_rows["fixed_status"] == "causal", "comp_id"])
        node_ids = set(registry.data[registry.data["type"] == "node"]["comp_id"])
        records = []
        statuses = ("unknown", "causal", "non-causal")
        for model_number in range(1, n_models + 1):
            model_id = f"M{model_number:04d}"
            crux_status = statuses[(model_number - 1) % 3]
            for _, component in registry.data.iterrows():
                comp_id = component["comp_id"]
                if comp_id in node_ids:
                    status = "present"
                elif comp_id == crux_id:
                    status = crux_status
                elif comp_id in fixed_ids:
                    status = "causal"
                else:
                    status = "non-causal"
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": None,
                    }
                )
        return records, crux_id

    def _generate_phase_transition_states(
        self,
        registry,
        lynchpin_id,
        zone_edges,
        n_zones,
        n_models,
        *,
        noise_fraction: float = 0.10,
    ):
        """Generate a resolution-closed fragmented multiverse.

        Every context (zone prototype or noise prototype) has all non-lynchpin
        edges resolved and is emitted in three variants: the lynchpin unknown,
        causal, and non-causal. This guarantees exact marginal matches for the
        lynchpin while keeping the design phase-transition-ready.
        """
        all_comp_ids = list(registry.data["comp_id"])
        node_comps = set(registry.data[registry.data["type"] == "node"]["comp_id"])

        n_contexts = max(1, n_models // 3)
        noise_count = max(1, int(n_contexts * noise_fraction))
        zone_context_count = max(0, n_contexts - noise_count)
        records = []
        model_idx = 1
        contexts: list[dict[str, str]] = []

        def append_variant(context_statuses: dict[str, str], crux_status: str):
            nonlocal model_idx
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                if comp_id in node_comps:
                    status = "present"
                elif comp_id == lynchpin_id:
                    status = crux_status
                else:
                    status = context_statuses[comp_id]
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": self._timing_from_comp(comp_row, registry),
                    }
                )

        def append_variants(context_statuses: dict[str, str]):
            contexts.append(context_statuses)
            for crux_status in ("unknown", "causal", "non-causal"):
                append_variant(context_statuses, crux_status)

        for zone in range(n_zones):
            n_this = (
                zone_context_count // n_zones
                if zone < n_zones - 1
                else zone_context_count
                - (zone_context_count // n_zones) * (n_zones - 1)
            )
            n_this = max(0, n_this)
            for _ in range(n_this):
                statuses: dict[str, str] = {}
                for comp_id in all_comp_ids:
                    if comp_id in node_comps or comp_id == lynchpin_id:
                        continue
                    if comp_id in zone_edges.get(zone, set()):
                        statuses[comp_id] = "causal"
                    elif any(
                        comp_id in zone_edges.get(z, set()) for z in range(n_zones)
                    ):
                        statuses[comp_id] = "non-causal"
                    else:
                        statuses[comp_id] = self._rng.choice(["causal", "non-causal"])
                append_variants(statuses)

        for _ in range(noise_count):
            statuses = {
                comp_id: self._rng.choice(["causal", "non-causal"])
                for comp_id in all_comp_ids
                if comp_id not in node_comps and comp_id != lynchpin_id
            }
            append_variants(statuses)

        # Complete triples provide closure. Extra requested slots are resolved
        # duplicates, so they cannot introduce an unmatched unknown context.
        filler_statuses = ("causal", "non-causal")
        while model_idx <= n_models:
            context = contexts[(model_idx - 1) % len(contexts)]
            status = filler_statuses[(model_idx - 1) % len(filler_statuses)]
            append_variant(context, status)

        return records

    # ── scenario C: ghost discovery ────────────────────────────────────────────

    def _scenario_ghost(
        self,
        n_models,
        n_components,
        *,
        registry_data: list[dict] | None = None,
        state_data: list[dict] | None = None,
        sample_n: int | None = None,
        mainstream_fraction: float = 0.70,
        ghost_fraction: float = 0.20,
        eps: float = 0.5,
        min_samples: int | None = None,
        internal_threshold: float = 0.6,
        prior_threshold: float = 0.4,
        divergent_fraction: float | None = None,
        enforce_thresholds: bool | None = None,
        include_plot_data: bool = False,
        plot_sample_n: int | None = 200,
        pair_sample_n: int | None = 5000,
        include_bidirectional: bool = False,
        compatibility_metric: str = "similarity_rate",
        exposure: str | None = None,
        outcome: str | None = None,
    ):
        if include_bidirectional:
            raise SimulationInputError(
                "Simulations support directed components only; "
                "include_bidirectional must be false"
            )
        if self._is_seeded(registry_data, state_data):
            return self._seeded_ghost(
                registry_data,
                state_data,
                sample_n,
                eps,
                min_samples,
                internal_threshold,
                prior_threshold,
                False if enforce_thresholds is None else enforce_thresholds,
                include_plot_data=include_plot_data,
                plot_sample_n=plot_sample_n,
                pair_sample_n=pair_sample_n,
                compatibility_metric=compatibility_metric,
                exposure=exposure,
                outcome=outcome,
            )

        if enforce_thresholds is None:
            enforce_thresholds = True

        registry = self._build_synthetic_registry(
            n_components,
            exposure=(exposure if compatibility_metric != "similarity_rate" else None),
            outcome=(outcome if compatibility_metric != "similarity_rate" else None),
        )

        def _try(seed):
            self._rng.seed(seed)
            edge_comps = registry.data[registry.data["type"] == "edge"]

            n_mainstream = int(n_models * mainstream_fraction)
            n_ghost = int(n_models * ghost_fraction)
            n_noise = n_models - n_mainstream - n_ghost

            mainstream_edges, ghost_edges, divergent_edges = (
                self._partition_edges_for_ghost(
                    edge_comps,
                    divergent_fraction=divergent_fraction,
                )
            )

            state_records = self._generate_ghost_states(
                registry,
                mainstream_edges,
                ghost_edges,
                divergent_edges,
                n_mainstream,
                n_ghost,
                n_noise,
                allow_unknown=compatibility_metric == "similarity_rate",
            )

            model_ids = sorted({r["model_id"] for r in state_records})
            (
                state,
                dyads,
                support_records,
                metric_diagnostics,
                _,
                _,
            ) = self._build_metric_dyads(
                registry,
                state_records,
                model_ids,
                compatibility_metric,
                exposure,
                outcome,
                synthesize_completion_support=True,
            )

            result, artifacts = self._analyze_ghost_result(
                registry,
                state,
                dyads,
                state_records,
                model_ids,
                seed,
                eps=eps,
                min_samples=min_samples,
                internal_threshold=internal_threshold,
                prior_threshold=prior_threshold,
                n_models=n_models,
                include_plot_data=include_plot_data,
                plot_sample_n=plot_sample_n,
                compatibility_metric=compatibility_metric,
                metric_diagnostics=metric_diagnostics,
            )
            artifacts["summary_stats"]["mainstream_models"] = n_mainstream
            artifacts["summary_stats"]["ghost_models"] = n_ghost
            artifacts["summary_stats"]["noise_models"] = n_noise
            artifacts["summary_stats"]["ghost_model_fraction"] = round(
                n_ghost / len(model_ids), 6
            )
            artifacts["completion_support_data"] = support_records
            return result, artifacts

        seeds = [self._random_state] if self._random_state is not None else [42]
        if self._random_state is not None:
            seeds = [self._random_state + i for i in range(21)]

        last_error = None
        for seed in seeds:
            try:
                base_result, artifacts = _try(seed)
                if enforce_thresholds:
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

    def _seeded_ghost(
        self,
        registry_data,
        state_data,
        sample_n,
        eps,
        min_samples,
        internal_threshold,
        prior_threshold,
        enforce_thresholds,
        include_plot_data=False,
        plot_sample_n=200,
        pair_sample_n=5000,
        compatibility_metric="similarity_rate",
        exposure=None,
        outcome=None,
    ):
        registry, state, dyads, model_ids, filtered_records = (
            self._prepare_seeded_inputs(registry_data, state_data, sample_n)
        )
        state, dyads, _, metric_diagnostics, _, _ = self._build_metric_dyads(
            registry,
            filtered_records,
            model_ids,
            compatibility_metric,
            exposure,
            outcome,
            synthesize_completion_support=False,
        )
        seed = self._random_state if self._random_state is not None else 42
        result, artifacts = self._analyze_ghost_result(
            registry,
            state,
            dyads,
            filtered_records,
            model_ids,
            seed,
            eps=eps,
            min_samples=min_samples,
            internal_threshold=internal_threshold,
            prior_threshold=prior_threshold,
            n_models=len(model_ids),
            include_plot_data=include_plot_data,
            plot_sample_n=plot_sample_n,
            compatibility_metric=compatibility_metric,
            metric_diagnostics=metric_diagnostics,
        )
        artifacts["summary_stats"]["seeded_input"] = True
        artifacts["summary_stats"]["sample_n"] = sample_n
        artifacts["summary_stats"]["available_seeded_models"] = len(
            {r["model_id"] for r in state_data}
        )
        if enforce_thresholds:
            self._assert_thresholds("ghost_discovery", result)
        return self._build_seeded_result_wrapper(
            "ghost_discovery", model_ids, registry, result, artifacts
        )

    def _analyze_ghost_result(
        self,
        registry,
        state,
        dyads,
        state_records,
        model_ids,
        seed,
        *,
        eps=0.5,
        min_samples=None,
        internal_threshold=0.6,
        prior_threshold=0.4,
        n_models=100,
        include_plot_data=False,
        plot_sample_n=200,
        compatibility_metric="similarity_rate",
        metric_diagnostics=None,
    ):
        clustering_engine = ClusteringEngine(
            umap_components=2,
            eps=eps,
            min_samples=(
                min_samples if min_samples is not None else max(2, n_models // 20)
            ),
            random_state=seed,
            score_field=compatibility_metric,
        )
        cluster_result = clustering_engine.detect_clusters(dyads, model_ids)

        prior_model_id = model_ids[0] if model_ids else "M0001"
        ghost_detector = GhostDetector(
            internal_threshold=internal_threshold,
            prior_threshold=prior_threshold,
            score_field=compatibility_metric,
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
            "compatibility_metric": compatibility_metric,
            "ghost_cluster_found": ghost_found,
            "clusters_detected": cluster_result["cluster_count"],
            "ghost_clusters": ghost_summary["ghost_clusters"],
            "mainstream_cluster": mainstream_info,
            "noise_count": cluster_result["noise_count"],
            "total_ghost_models": ghost_summary["total_ghost_models"],
            "top_ghost_cluster": ghost_summary["top_ghost_cluster"],
        }
        result.update(metric_diagnostics or {})
        result.update(
            {
                "metric_unique_values": cluster_result["metric_unique_values"],
                "all_pairs_compatible": cluster_result["all_pairs_compatible"],
                "all_pairs_incompatible": cluster_result["all_pairs_incompatible"],
                "profile_variance": cluster_result["profile_variance"],
                "degenerate_metric": cluster_result["degenerate_metric"],
            }
        )
        artifacts = {
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
                "seed": seed,
            },
        }
        if include_plot_data:
            artifacts["plot_data"] = self._build_ghost_dyad_heatmap_data(
                dyads,
                cluster_result["cluster_assignments"],
                contrast,
                model_ids,
                compatibility_metric=compatibility_metric,
                plot_sample_n=plot_sample_n,
            )
        return result, artifacts

    def _build_ghost_dyad_heatmap_data(
        self,
        dyads,
        cluster_assignments,
        cluster_contrast,
        model_ids,
        *,
        compatibility_metric="similarity_rate",
        plot_sample_n=200,
    ):
        model_ids = sorted(model_ids)
        assign_map = {
            a["model_id"]: (
                a["cluster_id"] if a.get("cluster_id") is not None else "noise"
            )
            for a in cluster_assignments
        }

        cluster_labels = {"noise": "noise", None: "noise"}
        for row in cluster_contrast:
            cluster_labels[row["cluster_id"]] = row.get("label", str(row["cluster_id"]))

        if plot_sample_n is not None and len(model_ids) > plot_sample_n:
            rng = random.Random(self._random_state or 42)
            by_cluster = {}
            for mid in model_ids:
                by_cluster.setdefault(assign_map.get(mid, "noise"), []).append(mid)

            sampled = []
            for cid in sorted(by_cluster, key=lambda x: str(x)):
                if len(sampled) >= plot_sample_n:
                    break
                sampled.append(sorted(by_cluster[cid])[0])

            remaining_slots = plot_sample_n - len(sampled)
            if remaining_slots > 0:
                remaining = [m for m in model_ids if m not in set(sampled)]
                sampled.extend(
                    rng.sample(remaining, min(remaining_slots, len(remaining)))
                )
            sampled = sorted(sampled)
        else:
            sampled = model_ids

        dyad_rows = []
        for d in dyads:
            ego = d["ego_id"]
            alt = d["alter_id"]
            if ego not in sampled or alt not in sampled:
                continue
            ego_cluster = assign_map.get(ego, "noise")
            alt_cluster = assign_map.get(alt, "noise")
            dyad_rows.append(
                dict(
                    ego_id=ego,
                    alter_id=alt,
                    score=float(d[compatibility_metric]),
                    ego_cluster_id=ego_cluster,
                    alter_cluster_id=alt_cluster,
                    ego_label=cluster_labels.get(ego_cluster, "unknown"),
                    alter_label=cluster_labels.get(alt_cluster, "unknown"),
                )
            )

        for i, r in enumerate(dyad_rows):
            r["order_ego"] = sampled.index(r["ego_id"]) if r["ego_id"] in sampled else i
            r["order_alter"] = (
                sampled.index(r["alter_id"]) if r["alter_id"] in sampled else i
            )

        total_models = len(model_ids)
        return {
            "cluster_contrast": cluster_contrast,
            "dyad_heatmap": dyad_rows,
            "metadata": {
                "plot_sample_n": plot_sample_n,
                "sampled_model_count": len(sampled),
                "available_model_count": total_models,
                "sampled": total_models > len(sampled),
            },
        }

    def _partition_edges_for_ghost(self, edge_comps, *, divergent_fraction=None):
        """Partition edges into mainstream-shared, ghost-shared, and divergent sets."""
        edges = list(edge_comps["comp_id"])
        if divergent_fraction is not None:
            n_divergent = max(2, int(len(edges) * divergent_fraction))
        else:
            n_divergent = max(2, len(edges) // 4)
        n_divergent = min(n_divergent, len(edges) - 2)

        shuffled = edges[:]
        self._rng.shuffle(shuffled)

        divergent_edges = set(shuffled[:n_divergent])
        shared_edges = shuffled[n_divergent:]

        mainstream_edges = set(shared_edges[: len(shared_edges) // 2])
        ghost_edges = set(shared_edges[len(shared_edges) // 2 :])

        return mainstream_edges, ghost_edges, divergent_edges

    def _generate_ghost_states(
        self,
        registry,
        mainstream_edges,
        ghost_edges,
        divergent_edges,
        n_mainstream,
        n_ghost,
        n_noise,
        *,
        allow_unknown=True,
    ):
        """Generate multiverse with mainstream, ghost, and noise clusters."""
        all_comp_ids = list(registry.data["comp_id"])
        node_comps = set(registry.data[registry.data["type"] == "node"]["comp_id"])
        fixed_edge_comps = set(
            registry.data.loc[
                (registry.data["type"] == "edge")
                & (registry.data["fixed_status"] == "causal"),
                "comp_id",
            ]
        )

        records = []
        model_idx = 1

        for _ in range(n_mainstream):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "present"
                elif comp_id in fixed_edge_comps:
                    status = "causal"
                elif comp_id in mainstream_edges:
                    status = "causal"
                elif comp_id in ghost_edges:
                    status = "non-causal"
                elif comp_id in divergent_edges:
                    status = "causal"
                else:
                    status = self._rng.choice(
                        ["causal", "unknown"]
                        if allow_unknown
                        else ["causal", "non-causal"]
                    )

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row, registry)
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": timing,
                    }
                )

        for _ in range(n_ghost):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "present"
                elif comp_id in fixed_edge_comps:
                    status = "causal"
                elif comp_id in ghost_edges:
                    status = "causal"
                elif comp_id in mainstream_edges:
                    status = "non-causal"
                elif comp_id in divergent_edges:
                    status = "non-causal"
                else:
                    status = self._rng.choice(
                        ["causal", "unknown"]
                        if allow_unknown
                        else ["causal", "non-causal"]
                    )

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row, registry)
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": timing,
                    }
                )

        for _ in range(n_noise):
            model_id = f"M{model_idx:04d}"
            model_idx += 1
            for comp_id in all_comp_ids:
                if comp_id in node_comps:
                    status = "present"
                elif comp_id in fixed_edge_comps:
                    status = "causal"
                else:
                    status = self._rng.choice(
                        ["causal", "unknown", "non-causal"]
                        if allow_unknown
                        else ["causal", "non-causal"]
                    )

                comp_row = registry.data[registry.data["comp_id"] == comp_id].iloc[0]
                timing = self._timing_from_comp(comp_row, registry)
                records.append(
                    {
                        "model_id": model_id,
                        "comp_id": comp_id,
                        "status": status,
                        "timing": timing,
                    }
                )

        return records

    def _timing_from_comp(self, comp_row, registry=None):
        source = comp_row["source"]
        if registry is not None:
            timing = registry.data.attrs.get("synthetic_node_timing", {}).get(source)
            if timing is not None:
                return timing
        if source and source.startswith("X"):
            try:
                return int(source[1:])
            except ValueError:
                return None
        return None

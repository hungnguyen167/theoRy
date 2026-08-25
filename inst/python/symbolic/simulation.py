from __future__ import annotations

from symbolic.backend import BddBackend, SymbolicBackend, temporal_variable_order
from symbolic.classes import (
    WeightedQueryClassResult,
    build_signature_atoms,
    build_query_classes,
)
from symbolic.delta_u import (
    SymbolicDeltaUEngine,
    distribution_entropy,
    compatibility_concentration,
)
from symbolic.formula import And, Formula, Not, Var, TRUE
from symbolic.query_index import build_query_formula_index
from symbolic.universe import SymbolicUniverse, build_symbolic_universe


class SymbolicSimulationEngine:
    def __init__(self, backend: SymbolicBackend | None = None):
        self.backend = backend

    def run_illusion_of_precision(
        self,
        n_shared_edges: int = 6,
        n_critical_unknown: int = 2,
        exposure: str = "X",
        outcome: str = "Y",
        seed: int = 42,
        mode: str = "full",
        n_samples: int = 5000,
        fallback: str = "sampled",
        signature_policy: str = "paper_v1",
        template_size: str = "paper_small",
        max_signature_atoms: int = 8,
    ) -> dict:
        template = _build_paper_template(exposure, outcome, template_size)
        universe = template["universe"]
        max_paths = _template_max_paths(template_size)
        critical_edges = _critical_edges(universe, exposure, n_critical_unknown)
        shared_constraints, shared_terms, shared_edges = _constraints_with_unknowns(
            universe,
            template["fixed_present"],
            critical_edges,
        )

        baseline = build_query_classes(
            universe,
            shared_constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
        )

        critical_resolved = []
        for s, t in critical_edges:
            ev = universe.edge_vars[(s, t)]
            resolved_constraints = And(shared_constraints, Var(ev.name))
            resolved = build_query_classes(
                universe,
                resolved_constraints,
                backend=self.backend,
                mode=mode,
                n_samples=n_samples,
                fallback=fallback,
                signature_policy=signature_policy,
                seed=seed,
                max_signature_atoms=max_signature_atoms,
                max_paths=max_paths,
            )
            critical_resolved.append({"edge": (s, t), "classes": resolved})

        surface_consensus = _surface_structural_consensus(universe, shared_terms)
        query_class_entropy = distribution_entropy(baseline)
        dominant_class_share = max(
            (c.proportion for c in baseline.classes), default=0.0
        )
        causal_compat = compatibility_concentration(baseline)
        consensus_gap = surface_consensus - causal_compat

        classes_data = _classes_to_dict(baseline)
        critical_data = []
        for cr in critical_resolved:
            critical_data.append(
                {
                    "edge": list(cr["edge"]),
                    "classes": _classes_to_dict(cr["classes"]),
                }
            )

        return {
            "scenario": "illusion_of_precision",
            "mode": f"symbolic_{baseline.mode}",
            "exact": baseline.exact,
            "universe_summary": {
                "nodes": list(universe.nodes),
                "edge_count": universe.edge_count,
                "exposure": exposure,
                "outcome": outcome,
                "template_size": template_size,
            },
            "constraints_summary": {
                "shared_edges": [[s, t] for s, t in shared_edges],
                "critical_edges": [[s, t] for s, t in critical_edges],
            },
            "classes": classes_data,
            "metrics": {
                "surface_structural_consensus": round(surface_consensus, 4),
                "query_class_entropy": round(query_class_entropy, 4),
                "dominant_class_share": round(dominant_class_share, 4),
                "causal_compatibility": round(causal_compat, 4),
                "consensus_gap": round(consensus_gap, 4),
                "classes_count": len(baseline.classes),
            },
            "artifacts": {
                "shared_edges": [[s, t] for s, t in shared_edges],
                "critical_edges": [[s, t] for s, t in critical_edges],
                "critical_resolved": critical_data,
            },
            "warnings": baseline.warnings,
            "shared_edges": [[s, t] for s, t in shared_edges],
            "critical_edges": [[s, t] for s, t in critical_edges],
        }

    def run_lynchpin_of_certainty(
        self,
        n_zones: int = 3,
        n_edges_per_zone: int = 3,
        exposure: str = "X",
        outcome: str = "Y",
        seed: int = 42,
        mode: str = "full",
        n_samples: int = 5000,
        fallback: str = "sampled",
        signature_policy: str = "paper_v1",
        template_size: str = "paper_small",
        max_signature_atoms: int = 8,
    ) -> dict:
        template = _build_paper_template(exposure, outcome, template_size)
        universe = template["universe"]
        max_paths = _template_max_paths(template_size)
        lynchpin_edge = ("C", "D")
        lynchpin_ev = universe.edge_var_for(*lynchpin_edge)
        shared_constraints, _, shared_edges = _constraints_with_unknowns(
            universe,
            template["fixed_present"],
            [lynchpin_edge],
        )

        baseline = build_query_classes(
            universe,
            shared_constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
        )
        baseline_entropy = distribution_entropy(baseline)

        delta_engine = SymbolicDeltaUEngine(self.backend)
        rankings = delta_engine.compute_delta_u(
            universe,
            shared_constraints,
            top_k=10,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
            candidate_edges=[lynchpin_edge],
        )

        lynchpin_rank = 0
        for i, r in enumerate(rankings):
            if (r["source"], r["target"]) == lynchpin_edge:
                lynchpin_rank = i + 1
                break

        resolved_constraints = (
            And(shared_constraints, Var(lynchpin_ev.name))
            if lynchpin_ev
            else shared_constraints
        )
        resolved = build_query_classes(
            universe,
            resolved_constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
        )
        resolved_entropy = distribution_entropy(resolved)
        top_delta = (
            rankings[0]["delta_u"] if rankings else baseline_entropy - resolved_entropy
        )
        expected_entropy = baseline_entropy - top_delta
        phase_transition = top_delta

        return {
            "scenario": "lynchpin_of_certainty",
            "mode": f"symbolic_{baseline.mode}",
            "exact": baseline.exact,
            "universe_summary": {
                "nodes": list(universe.nodes),
                "edge_count": universe.edge_count,
                "template_size": template_size,
            },
            "constraints_summary": {
                "shared_edges": [[s, t] for s, t in shared_edges],
                "critical_edges": [list(lynchpin_edge)],
            },
            "classes": _classes_to_dict(baseline),
            "metrics": {
                "baseline_entropy": round(baseline_entropy, 4),
                "post_resolution_expected_entropy": round(expected_entropy, 4),
                "phase_transition_score": round(phase_transition, 4),
                "lynchpin_rank": lynchpin_rank,
                "lynchpin_edge": list(lynchpin_edge),
            },
            "artifacts": {
                "lynchpin_edges": [list(lynchpin_edge)],
                "baseline_classes": _classes_to_dict(baseline),
                "resolved_classes": _classes_to_dict(resolved),
                "rankings": rankings,
            },
            "warnings": baseline.warnings,
            "lynchpin_edges": [list(lynchpin_edge)],
            "baseline_classes": _classes_to_dict(baseline),
            "resolved_classes": _classes_to_dict(resolved),
        }

    def run_ghost_discovery(
        self,
        n_mainstream: int = 40,
        n_ghost: int = 10,
        n_noise: int = 10,
        seed: int = 42,
        mode: str = "full",
        n_samples: int = 5000,
        fallback: str = "sampled",
        signature_policy: str = "paper_v1",
        template_size: str = "paper_small",
        max_signature_atoms: int = 8,
    ) -> dict:
        exposure = "X"
        outcome = "Y"
        template = _build_paper_template(exposure, outcome, template_size)
        universe = template["universe"]
        max_paths = _template_max_paths(template_size)
        ghost_edges = [("C", "D")]
        mainstream_edges = list(template["fixed_present"])
        multiverse_constraints, _, _ = _constraints_with_unknowns(
            universe,
            template["fixed_present"],
            ghost_edges,
        )

        multiverse_classes = build_query_classes(
            universe,
            multiverse_constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
        )

        mainstream_constraints = And(
            multiverse_constraints,
            Not(Var(universe.edge_vars[("C", "D")].name)),
        )
        prior_classes = build_query_classes(
            universe,
            mainstream_constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            fallback=fallback,
            signature_policy=signature_policy,
            seed=seed,
            max_signature_atoms=max_signature_atoms,
            max_paths=max_paths,
        )

        index = build_query_formula_index(universe, max_paths=max_paths)
        atoms = build_signature_atoms(universe, index, policy=signature_policy)
        if len(atoms) > multiverse_classes.signature_atom_count:
            atoms = atoms[: multiverse_classes.signature_atom_count]
        ghost_records = []
        for gc in multiverse_classes.classes:
            c_to_d_present = gc.atom_values.get("query_edge_C__D_present")
            if c_to_d_present is None:
                count_backend = BddBackend(
                    variable_order=temporal_variable_order(universe)
                )
                variables = list(universe.variable_names)
                region_formula = _region_formula(atoms, gc.atom_values)
                prior_mass = count_backend.count(
                    region_formula, variables, mainstream_constraints
                )
                overlap = prior_mass / gc.mass if gc.mass else 0.0
            else:
                overlap = 0.0 if c_to_d_present else 1.0
            if overlap <= 0.30:
                ghost_records.append(
                    {
                        "class": gc,
                        "prior_overlap": overlap,
                        "internal_coherence": 1.0,
                    }
                )

        ghost_count = len(ghost_records)
        ghost_total_mass = sum(float(g["class"].mass) for g in ghost_records)
        largest = max(ghost_records, key=lambda g: float(g["class"].mass), default=None)
        largest_ghost_mass = float(largest["class"].mass) if largest else 0.0
        largest_ghost_prior_overlap = largest["prior_overlap"] if largest else 1.0
        ghost_internal_coherence = largest["internal_coherence"] if largest else 0.0

        return {
            "scenario": "ghost_discovery",
            "mode": f"symbolic_{multiverse_classes.mode}",
            "exact": multiverse_classes.exact,
            "universe_summary": {
                "nodes": list(universe.nodes),
                "edge_count": universe.edge_count,
                "template_size": template_size,
            },
            "classes": _classes_to_dict(multiverse_classes),
            "metrics": {
                "classes_detected": len(multiverse_classes.classes),
                "ghost_class_count": ghost_count,
                "ghost_total_mass": ghost_total_mass,
                "largest_ghost_mass": largest_ghost_mass,
                "largest_ghost_prior_overlap": largest_ghost_prior_overlap,
                "ghost_internal_coherence": ghost_internal_coherence,
            },
            "artifacts": {
                "mainstream_edges": [list(e) for e in mainstream_edges],
                "ghost_edges": [list(e) for e in ghost_edges],
                "mainstream_classes": _classes_to_dict(prior_classes),
                "ghost_classes": _classes_to_dicts([g["class"] for g in ghost_records]),
            },
            "warnings": multiverse_classes.warnings,
            "mainstream_edges": [list(e) for e in mainstream_edges],
            "ghost_edges": [list(e) for e in ghost_edges],
            "mainstream_classes": _classes_to_dict(prior_classes),
            "ghost_classes": _classes_to_dicts([g["class"] for g in ghost_records]),
        }


def _surface_structural_consensus(
    universe: SymbolicUniverse,
    shared_terms: list,
) -> float:
    total = universe.edge_count
    if total == 0:
        return 1.0
    shared_count = len(shared_terms)
    return min(1.0, shared_count / total)


def _build_paper_template(exposure: str, outcome: str, template_size: str) -> dict:
    if template_size == "paper_13":
        nodes = [
            "A",
            "S1",
            exposure,
            "S2",
            "B",
            "S3",
            "C",
            "S4",
            "D",
            "S5",
            "S6",
            "S7",
            outcome,
        ]
        timing = {node: i + 1 for i, node in enumerate(nodes)}
    else:
        nodes = ["A", exposure, "B", "C", "D", outcome]
        timing = {"A": 1, exposure: 2, "B": 3, "C": 4, "D": 5, outcome: 6}

    universe = build_symbolic_universe(
        nodes=nodes,
        timing=timing,
        exposure=exposure,
        outcome=outcome,
    )
    fixed_present = {
        ("A", exposure),
        ("A", "C"),
        ("B", "C"),
        ("B", outcome),
        (exposure, outcome),
    }
    fixed_present = {edge for edge in fixed_present if edge in universe.edge_vars}
    return {"universe": universe, "fixed_present": fixed_present}


def _template_max_paths(template_size: str) -> int | None:
    return 256 if template_size == "paper_13" else None


def _critical_edges(
    universe: SymbolicUniverse,
    exposure: str,
    n_critical_unknown: int,
) -> list[tuple[str, str]]:
    preferred = [("C", "D"), (exposure, "C")]
    result = [edge for edge in preferred if edge in universe.edge_vars]
    if len(result) < n_critical_unknown:
        for edge in universe.edge_vars:
            if edge not in result:
                result.append(edge)
                if len(result) >= n_critical_unknown:
                    break
    return result[:n_critical_unknown]


def _constraints_with_unknowns(
    universe: SymbolicUniverse,
    fixed_present: set[tuple[str, str]],
    unknown_edges: list[tuple[str, str]],
) -> tuple[Formula, list[Formula], list[tuple[str, str]]]:
    unknown = set(unknown_edges)
    terms: list[Formula] = []
    shared_edges: list[tuple[str, str]] = []
    for edge, ev in universe.edge_vars.items():
        if edge in unknown:
            continue
        shared_edges.append(edge)
        if edge in fixed_present:
            terms.append(Var(ev.name))
        else:
            terms.append(Not(Var(ev.name)))
    return (And(*terms) if terms else TRUE), terms, shared_edges


def _region_formula(atoms, atom_values: dict[str, bool]) -> Formula:
    terms: list[Formula] = []
    for atom in atoms:
        value = atom_values.get(atom.name, False)
        terms.append(atom.formula if value else Not(atom.formula))
    return And(*terms) if terms else TRUE


def _class_to_dict(c) -> dict:
    return {
        "class_id": c.class_id,
        "mass": _json_mass(c.mass),
        "proportion": round(c.proportion, 6),
        "adjustment_identifiable": c.atom_values.get("adjustment_identifiable", False),
        "empty_adjustment_valid": c.atom_values.get("empty_adjustment_valid", False),
        "signature": c.signature,
        "atom_values": c.atom_values,
    }


def _json_mass(value):
    if isinstance(value, int) and abs(value) > 10**15:
        return str(value)
    return value


def _classes_to_dicts(classes) -> list[dict]:
    return [_class_to_dict(c) for c in classes]


def _classes_to_dict(result: WeightedQueryClassResult) -> list[dict]:
    return _classes_to_dicts(result.classes)

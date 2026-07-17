from __future__ import annotations

import math

from symbolic.backend import SymbolicBackend
from symbolic.classes import (
    WeightedQueryClassResult,
    build_query_classes,
    build_signature_atoms,
)
from symbolic.formula import And, Formula, Not, Var
from symbolic.query_index import build_query_formula_index
from symbolic.universe import SymbolicUniverse


def class_distribution(result: WeightedQueryClassResult) -> dict[str, float]:
    """Return {class_id: proportion} mapping."""
    return {c.class_id: c.proportion for c in result.classes}


def distribution_entropy(result: WeightedQueryClassResult) -> float:
    """Shannon entropy over class proportions (base 2)."""
    total = 0.0
    for c in result.classes:
        if c.proportion > 0:
            total -= c.proportion * math.log2(c.proportion)
    return total


def compatibility_concentration(result: WeightedQueryClassResult) -> float:
    """Sum of squared proportions (Herfindahl index). Higher = more concentrated."""
    return sum(c.proportion * c.proportion for c in result.classes)


class SymbolicDeltaUEngine:
    """Measures how much resolving each uncertain edge shifts the class distribution.

    Uses entropy-based uncertainty reduction:
      delta_u = entropy(baseline) - E[entropy(conditional on edge)]
    """

    def __init__(self, backend: SymbolicBackend | None = None):
        self.backend = backend

    def compute_delta_u(
        self,
        universe: SymbolicUniverse,
        constraints: Formula | None = None,
        top_k: int = 10,
        mode: str = "full",
        n_samples: int = 1000,
        signature_policy: str = "paper_v1",
        fallback: str = "sampled",
        max_signature_atoms: int = 16,
        seed: int | None = None,
        max_path_len: int = 8,
        max_paths: int | None = None,
        candidate_edges: list[tuple[str, str]] | None = None,
    ) -> list[dict]:
        """Rank edges by uncertainty reduction over query classes.

        For each edge variable e:
          1. Compute baseline class distribution
          2. Compute class distribution conditioned on e=true and e=false
          3. delta_u = entropy(baseline) - weighted average of conditional entropies
        """
        baseline = build_query_classes(
            universe,
            constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            signature_policy=signature_policy,
            fallback=fallback,
            max_signature_atoms=max_signature_atoms,
            seed=seed,
            max_path_len=max_path_len,
            max_paths=max_paths,
        )
        baseline_entropy = distribution_entropy(baseline)
        baseline_total = baseline.total_mass
        if baseline_total == 0:
            return []

        index = build_query_formula_index(
            universe,
            max_path_len=max_path_len,
            max_paths=max_paths,
        )
        atoms = build_signature_atoms(universe, index, policy=signature_policy)

        results: list[dict] = []
        edge_items = (
            [
                (edge, universe.edge_vars[edge])
                for edge in candidate_edges
                if edge in universe.edge_vars
            ]
            if candidate_edges is not None
            else list(universe.edge_vars.items())
        )
        for (src, tgt), ev in edge_items:
            pos_constraints = (
                And(constraints, Var(ev.name)) if constraints else Var(ev.name)
            )
            neg_constraints = (
                And(constraints, Not(Var(ev.name)))
                if constraints
                else Not(Var(ev.name))
            )

            pos_result = build_query_classes(
                universe,
                pos_constraints,
                backend=self.backend,
                mode=mode,
                n_samples=n_samples,
                signature_policy=signature_policy,
                fallback=fallback,
                max_signature_atoms=max_signature_atoms,
                seed=seed,
                max_path_len=max_path_len,
                max_paths=max_paths,
                _index=index,
                _atoms=atoms,
            )
            neg_result = build_query_classes(
                universe,
                neg_constraints,
                backend=self.backend,
                mode=mode,
                n_samples=n_samples,
                signature_policy=signature_policy,
                fallback=fallback,
                max_signature_atoms=max_signature_atoms,
                seed=seed,
                max_path_len=max_path_len,
                max_paths=max_paths,
                _index=index,
                _atoms=atoms,
            )

            pos_mass = pos_result.total_mass
            neg_mass = neg_result.total_mass

            if pos_mass + neg_mass == 0:
                continue

            p_pos = pos_mass / baseline_total
            p_neg = neg_mass / baseline_total

            pos_entropy = distribution_entropy(pos_result)
            neg_entropy = distribution_entropy(neg_result)

            expected_entropy = p_pos * pos_entropy + p_neg * neg_entropy
            delta_u = baseline_entropy - expected_entropy

            best = "none"
            if p_pos > 0 and p_neg > 0:
                if pos_entropy < neg_entropy:
                    best = "positive"
                elif neg_entropy < pos_entropy:
                    best = "negative"
            elif p_pos > 0:
                best = "positive"
            elif p_neg > 0:
                best = "negative"

            affected_classes = []
            pos_classes = class_distribution(pos_result)
            neg_classes = class_distribution(neg_result)
            for cid in set(list(pos_classes.keys()) + list(neg_classes.keys())):
                if pos_classes.get(cid, 0) != neg_classes.get(cid, 0):
                    affected_classes.append(cid)

            results.append(
                {
                    "component_id": ev.comp_id or f"e__{src}__{tgt}",
                    "type": "edge",
                    "source": src,
                    "target": tgt,
                    "baseline_entropy": round(baseline_entropy, 6),
                    "entropy_if_present": round(pos_entropy, 6),
                    "entropy_if_absent": round(neg_entropy, 6),
                    "delta_u": round(delta_u, 6),
                    "delta_u_positive": round(baseline_entropy - pos_entropy, 6),
                    "delta_u_negative": round(baseline_entropy - neg_entropy, 6),
                    "best_resolution": best,
                    "branch_mass_present": pos_mass,
                    "branch_mass_absent": neg_mass,
                    "affected_class_ids": affected_classes,
                    "exact": baseline.exact and pos_result.exact and neg_result.exact,
                    "mode": f"symbolic_{baseline.mode}",
                }
            )

        results.sort(key=lambda r: r["delta_u"], reverse=True)
        return results[:top_k]

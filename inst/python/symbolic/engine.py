from __future__ import annotations

from symbolic.backend import BddBackend, SymbolicBackend, temporal_variable_order
from symbolic.classes import WeightedQueryClassResult, build_query_classes
from symbolic.dsep import (
    adjustment_identifiable_formula,
    candidate_adjustment_sets,
    valid_adjustment_formula,
)
from symbolic.formula import Formula
from symbolic.reachability import build_reachability
from symbolic.signature import (
    QueryClassSignature,
    query_signature_from_formula,
)
from symbolic.universe import SymbolicUniverse


class SymbolicCompatibilityEngine:
    def __init__(self, backend: SymbolicBackend | None = None):
        self.backend = backend

    def _get_variables(self, universe: SymbolicUniverse) -> list[str]:
        return list(universe.variable_names)

    def _build_formulas(
        self,
        universe: SymbolicUniverse,
        constraints: Formula | None = None,
    ) -> dict:
        reachability = build_reachability(universe)
        candidate_sets = candidate_adjustment_sets(universe)
        adj_id_formula = adjustment_identifiable_formula(
            universe, candidate_sets, reachability
        )
        empty_Z = frozenset()
        empty_adj_formula = valid_adjustment_formula(universe, reachability, empty_Z)
        return {
            "reachability": reachability,
            "candidate_sets": candidate_sets,
            "adj_id_formula": adj_id_formula,
            "empty_adj_formula": empty_adj_formula,
        }

    def build_classes(
        self,
        universe: SymbolicUniverse,
        constraints: Formula | None = None,
        mode: str = "full",
        n_samples: int = 1000,
    ) -> list[QueryClassSignature]:
        """Build equivalence classes.

        Delegates to the new build_query_classes for rich partitioning,
        then converts to legacy QueryClassSignature format for backward compatibility.
        """
        result = build_query_classes(
            universe,
            constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
        )
        return _to_legacy_class_list(result)

    def build_classes_weighted(
        self,
        universe: SymbolicUniverse,
        constraints: Formula | None = None,
        mode: str = "full",
        n_samples: int = 1000,
        signature_policy: str = "paper_v1",
    ) -> WeightedQueryClassResult:
        """Build weighted query classes (new API)."""
        return build_query_classes(
            universe,
            constraints,
            backend=self.backend,
            mode=mode,
            n_samples=n_samples,
            signature_policy=signature_policy,
        )

    def compare_preferred_to_multiverse(
        self,
        universe: SymbolicUniverse,
        preferred_constraints: Formula,
        multiverse_constraints: Formula | None = None,
    ) -> dict:
        formulas = self._build_formulas(universe, multiverse_constraints)
        self._get_variables(universe)
        adj_id_f = formulas["adj_id_formula"]
        formulas["empty_adj_formula"]

        pref_adj_id = (
            adj_id_f.evaluate({v.name: True for v in universe.edge_vars.values()})
            if adj_id_f.variables()
            else False
        )

        classes = self.build_classes(universe, multiverse_constraints, mode="full")
        return {
            "preferred_adjustment_identifiable": pref_adj_id,
            "classes": classes,
        }

    def compare_theories(
        self,
        universe: SymbolicUniverse,
        a_constraints: Formula,
        b_constraints: Formula,
    ) -> dict:
        formulas = self._build_formulas(universe)
        variables = self._get_variables(universe)
        backend = self.backend or BddBackend(
            variable_order=temporal_variable_order(universe)
        )
        adj_id_f = formulas["adj_id_formula"]
        empty_f = formulas["empty_adj_formula"]

        a_sig = query_signature_from_formula(
            adj_id_f, empty_f, universe, backend, variables, a_constraints
        )
        b_sig = query_signature_from_formula(
            adj_id_f, empty_f, universe, backend, variables, b_constraints
        )

        adjustment_identifiable_compatible = (
            a_sig.adjustment_identifiable == b_sig.adjustment_identifiable
        )
        return {
            "adjustment_identifiable_compatible": adjustment_identifiable_compatible,
            "a_signature": a_sig,
            "b_signature": b_sig,
        }


def _to_legacy_class_list(
    result: WeightedQueryClassResult,
) -> list[QueryClassSignature]:
    """Convert WeightedQueryClassResult to legacy QueryClassSignature list."""
    classes: list[QueryClassSignature] = []
    for wc in result.classes:
        adj_id = wc.atom_values.get("adjustment_identifiable", False)
        empty_valid = wc.atom_values.get("empty_adjustment_valid", False)
        classes.append(
            QueryClassSignature(
                class_id=wc.class_id,
                mass=wc.mass,
                adjustment_identifiable=adj_id,
                possibly_adjustment_identifiable=adj_id,
                necessarily_adjustment_identifiable=None,
                adjustment_identifiable_mass=wc.mass if adj_id else 0,
                valid_adjustment_exists=adj_id,
                empty_adjustment_valid=empty_valid,
            )
        )
    return classes

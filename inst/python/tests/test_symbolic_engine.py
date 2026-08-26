from __future__ import annotations

from symbolic.backend import BruteForceBackend
from symbolic.constraints import constraints_from_dag_spec
from symbolic.engine import SymbolicCompatibilityEngine
from symbolic.formula import Var
from symbolic.universe import build_symbolic_universe


class TestSymbolicCompatibilityEngine:
    def setup_method(self):
        self.engine = SymbolicCompatibilityEngine(BruteForceBackend(max_vars=20))

    def test_build_classes_simple(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        classes = self.engine.build_classes(u)
        assert len(classes) >= 1
        for c in classes:
            assert c.mass >= 0

    def test_build_classes_with_constraints(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        ev = u.edge_var_for("X", "Y")
        assert ev is not None
        constraints = Var(ev.name)
        classes = self.engine.build_classes(u, constraints)
        assert len(classes) >= 1

    def test_compare_theories(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )

        dag_a = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"], ["A", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        dag_b = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }

        a_constraints = constraints_from_dag_spec(
            dag_a, u, unmentioned_edges="non-causal"
        )
        b_constraints = constraints_from_dag_spec(
            dag_b, u, unmentioned_edges="non-causal"
        )

        result = self.engine.compare_theories(u, a_constraints, b_constraints)
        assert "adjustment_identifiable_compatible" in result
        assert "a_signature" in result
        assert "b_signature" in result

    def test_compare_preferred_to_multiverse(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        dag = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        pref = constraints_from_dag_spec(dag, u)
        result = self.engine.compare_preferred_to_multiverse(u, pref)
        assert "preferred_adjustment_identifiable" in result
        assert "classes" in result

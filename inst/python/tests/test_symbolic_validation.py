from __future__ import annotations

from symbolic.backend import BruteForceBackend, RandomSampleBackend
from symbolic.constraints import constraints_from_dag_spec
from symbolic.dsep import (
    _is_collider_on_path,
    _path_open_formula,
    adjustment_identifiable_formula,
    backdoor_path_formulas,
    valid_adjustment_formula,
)
from symbolic.engine import SymbolicCompatibilityEngine
from symbolic.formula import And, Not, TRUE, Var
from symbolic.reachability import build_reachability
from symbolic.universe import build_symbolic_universe


class TestNoConfounding:
    """X -> Y only. Empty adjustment set is valid; effect is identifiable."""

    def test_empty_adjustment_valid(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        f = valid_adjustment_formula(u, reach, frozenset())
        backend = BruteForceBackend(max_vars=20)
        count = backend.count(f, u.variable_names)
        # 2 assignments (X->Y present or absent); both valid
        assert count == 2

    def test_adjustment_identifiable(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        adj = adjustment_identifiable_formula(u)
        backend = BruteForceBackend(max_vars=20)
        assert backend.count(adj, u.variable_names) == 2


class TestSimpleConfounding:
    """A -> X, A -> Y, X -> Y.  Backdoor X <- A -> Y is open without adjustment."""

    def setup_method(self):
        # A before X before Y so A->X edge actually exists
        self.u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        self.reach = build_reachability(self.u)
        self.backend = BruteForceBackend(max_vars=20)
        self.vl = self.u.variable_names
        self.total = 1 << len(self.vl)  # 8

    def test_backdoor_path_detected(self):
        bps = backdoor_path_formulas(self.u, self.reach)
        paths = [p for p, _ in bps]
        assert ["X", "A", "Y"] in paths

    def test_empty_z_invalid_when_backdoor_open(self):
        """Empty Z is invalid when both A->X and A->Y are present (backdoor open)."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset())
        count = self.backend.count(f, self.vl)
        # 2 assignments have both A->X and A->Y True (X->Y varies) => invalid
        assert count == self.total - 2

    def test_z_A_valid_always(self):
        """Z={A} blocks the backdoor (A is a fork, conditioning blocks it)."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset({"A"}))
        count = self.backend.count(f, self.vl)
        assert count == self.total

    def test_adjustment_identifiable_all(self):
        """Since Z={A} always works, all assignments are identifiable."""
        adj = adjustment_identifiable_formula(self.u)
        count = self.backend.count(adj, self.vl)
        assert count == self.total


class TestCollider:
    """X <- A -> C <- B -> Y.  Collider at C blocks path when unadjusted."""

    def setup_method(self):
        # Timing ensures A->X, A->C, B->C, B->Y, X->Y all possible
        self.u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "Y": 5},
            exposure="X",
            outcome="Y",
        )
        self.reach = build_reachability(self.u)
        self.backend = BruteForceBackend(max_vars=20)
        self.vl = self.u.variable_names

        # Constrain to only the 5 intended edges
        self.keep = {("A", "X"), ("A", "C"), ("B", "C"), ("B", "Y"), ("X", "Y")}
        self.block_terms = [
            Not(Var(self.u.edge_vars[(s, t)].name))
            for (s, t) in self.u.edge_vars
            if (s, t) not in self.keep
        ]
        self.constraint = And(*self.block_terms)
        self.total_c = self.backend.count(TRUE, self.vl, self.constraint)

    def test_collider_path_exists(self):
        bps = backdoor_path_formulas(self.u, self.reach)
        paths = [p for p, _ in bps]
        assert ["X", "A", "C", "B", "Y"] in paths

    def test_c_is_collider_on_path(self):
        path = ["X", "A", "C", "B", "Y"]
        assert _is_collider_on_path(self.u, path, 2) is True  # C at index 2
        assert _is_collider_on_path(self.u, path, 1) is False  # A at index 1

    def test_empty_z_valid_collider_blocks(self):
        """Collider at C blocks the backdoor when not adjusted."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset())
        count = self.backend.count(f, self.vl, self.constraint)
        assert count == self.total_c

    def test_z_C_opens_collider(self):
        """Adjusting for C opens the collider path."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset({"C"}))
        count = self.backend.count(f, self.vl, self.constraint)
        assert count < self.total_c

    def test_z_A_blocks_at_fork(self):
        """A is a fork; conditioning blocks the path."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset({"A"}))
        count = self.backend.count(f, self.vl, self.constraint)
        assert count == self.total_c


class TestDescendantOfCollider:
    """Adjusting for a descendant of a collider also opens the path."""

    def setup_method(self):
        self.u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "D", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "D": 5, "Y": 6},
            exposure="X",
            outcome="Y",
        )
        self.reach = build_reachability(self.u)
        self.backend = BruteForceBackend(max_vars=20)
        self.vl = self.u.variable_names

        self.keep = {
            ("A", "X"),
            ("A", "C"),
            ("B", "C"),
            ("B", "Y"),
            ("X", "Y"),
            ("C", "D"),
        }
        self.block_terms = [
            Not(Var(self.u.edge_vars[(s, t)].name))
            for (s, t) in self.u.edge_vars
            if (s, t) not in self.keep
        ]
        self.constraint = And(*self.block_terms)
        self.total_c = self.backend.count(TRUE, self.vl, self.constraint)

    def test_empty_z_valid(self):
        f = valid_adjustment_formula(self.u, self.reach, frozenset())
        count = self.backend.count(f, self.vl, self.constraint)
        assert count == self.total_c

    def test_z_D_descendant_opens_collider(self):
        """D is descendant of C; adjusting for D opens the collider path."""
        f = valid_adjustment_formula(self.u, self.reach, frozenset({"D"}))
        count = self.backend.count(f, self.vl, self.constraint)
        assert count < self.total_c

    def test_path_open_formula_includes_C_D_edge(self):
        """The path-open formula for Z={D} at collider C should reference C->D edge."""
        path = ["X", "A", "C", "B", "Y"]
        po = _path_open_formula(self.u, self.reach, path, frozenset({"D"}))
        formula_str = str(po)
        assert "e__C__D" in formula_str


class TestDagConversion:
    def test_dag_conversion_closed_world(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        dag = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        f = constraints_from_dag_spec(dag, u, unmentioned_edges="non-causal")
        backend = BruteForceBackend(max_vars=20)
        # Exactly 1 satisfying assignment (X->Y=1, others=0)
        assert backend.count(f, u.variable_names) == 1

    def test_dag_conversion_open_world(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        dag = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        f = constraints_from_dag_spec(dag, u, unmentioned_edges="unknown")
        backend = BruteForceBackend(max_vars=20)
        # X->Y=1 fixed; A->X and A->Y free => 4 assignments
        assert backend.count(f, u.variable_names) == 4


class TestSampledConvergence:
    def test_sampled_approximates_full(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        full_engine = SymbolicCompatibilityEngine()
        full_engine.build_classes(u, mode="full")

        sampled_engine = SymbolicCompatibilityEngine(RandomSampleBackend())
        sampled_classes = sampled_engine.build_classes(u, mode="sampled", n_samples=200)
        sampled_total = sum(c.mass for c in sampled_classes)

        assert sampled_total <= 200
        # Both should have at least one class
        assert len(sampled_classes) >= 1

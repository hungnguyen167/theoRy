from __future__ import annotations

from symbolic.backend import BruteForceBackend
from symbolic.dsep import (
    adjustment_identifiable_formula,
    backdoor_path_formulas,
    candidate_adjustment_sets,
    valid_adjustment_formula,
)
from symbolic.reachability import build_reachability
from symbolic.universe import build_symbolic_universe


class TestDsep:
    def setup_method(self):
        self.backend = BruteForceBackend(max_vars=20)

    def test_simple_no_confounding(self):
        """X -> Y only: empty adjustment set valid, all assignments identifiable."""
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        candidates = candidate_adjustment_sets(u)
        adj_id = adjustment_identifiable_formula(u, candidates, reach)
        count = self.backend.count(adj_id, u.variable_names)
        assert count == 2  # both assignments (X->Y present/absent) are identifiable

    def test_simple_confounding_backdoor_detected(self):
        """A -> X, A -> Y: backdoor path X <- A -> Y is detected."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        bps = backdoor_path_formulas(u, reach)
        paths = [p for p, _ in bps]
        assert ["X", "A", "Y"] in paths

    def test_simple_confounding_empty_z_partial(self):
        """Empty Z is invalid when backdoor is open (both A->X and A->Y present)."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        f = valid_adjustment_formula(u, reach, frozenset())
        count = self.backend.count(f, u.variable_names)
        total = 1 << len(u.variable_names)
        assert count == total - 2  # 2 assignments have open backdoor

    def test_simple_confounding_z_A_valid(self):
        """Z={A} blocks the backdoor (fork conditioned) => all valid."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        f = valid_adjustment_formula(u, reach, frozenset({"A"}))
        count = self.backend.count(f, u.variable_names)
        assert count == 1 << len(u.variable_names)

    def test_candidate_adjustment_sets_all(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A", "B"],
            timing={"X": 1, "A": 1, "B": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        candidates = candidate_adjustment_sets(u, "all_non_endpoints")
        assert len(candidates) == 4
        assert frozenset() in candidates
        assert frozenset({"A"}) in candidates
        assert frozenset({"B"}) in candidates
        assert frozenset({"A", "B"}) in candidates

    def test_backdoor_paths_exclude_direct_causal(self):
        """The direct X -> Y edge is NOT a backdoor path."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        reach = build_reachability(u)
        bps = backdoor_path_formulas(u, reach)
        paths = [p for p, _ in bps]
        # X -> Y (direct causal) should NOT be in backdoor paths
        assert ["X", "Y"] not in paths

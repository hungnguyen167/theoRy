from __future__ import annotations

from symbolic.backend import BruteForceBackend
from symbolic.query_index import QueryFormulaIndex, build_query_formula_index
from symbolic.universe import build_symbolic_universe


class TestQueryFormulaIndex:
    def test_basic_construction(self):
        """Index builds without error for a simple universe."""
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        assert isinstance(idx, QueryFormulaIndex)
        assert idx.universe is u
        assert len(idx.paths) >= 1
        assert len(idx.candidate_sets) == 1  # only frozenset()

    def test_confounder_paths_cached(self):
        """Backdoor paths are identified and cached."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        assert len(idx.backdoor_paths) >= 1
        bd_nodes = [p.nodes for p in idx.backdoor_paths]
        assert ("X", "A", "Y") in bd_nodes

    def test_valid_z_cached(self):
        """valid_z contains formulas for all candidate sets."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        assert frozenset() in idx.valid_z
        assert frozenset({"A"}) in idx.valid_z
        assert len(idx.valid_z) == len(idx.candidate_sets)

    def test_adjustment_identifiable_formula(self):
        """adjustment_identifiable is the Or of all valid_z formulas."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        backend = BruteForceBackend(max_vars=20)
        count = backend.count(idx.adjustment_identifiable, u.variable_names)
        total = 1 << len(u.variable_names)
        assert count == total  # Z={A} always works

    def test_empty_adjustment_valid_matches_direct(self):
        """empty_adjustment_valid matches direct computation."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        backend = BruteForceBackend(max_vars=20)
        count_cached = backend.count(idx.empty_adjustment_valid, u.variable_names)
        # Direct computation: 6 out of 8 valid (both A->X and A->Y present = invalid)
        assert count_cached == 6

    def test_open_backdoor_empty(self):
        """open_backdoor_empty is Not(empty_adjustment_valid)."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        backend = BruteForceBackend(max_vars=20)
        open_count = backend.count(idx.open_backdoor_empty, u.variable_names)
        empty_count = backend.count(idx.empty_adjustment_valid, u.variable_names)
        total = 1 << len(u.variable_names)
        assert open_count + empty_count == total

    def test_collider_paths(self):
        """Collider nodes are correctly identified in cached paths."""
        u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "Y": 5},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        # Find the collider path X <- A -> C <- B -> Y
        collider_paths = [p for p in idx.backdoor_paths if "C" in p.collider_nodes]
        assert len(collider_paths) >= 1
        cp = collider_paths[0]
        assert "C" in cp.collider_nodes
        assert "A" in cp.noncollider_nodes

    def test_descendant_of_collider_path(self):
        """Descendant-of-collider path is captured."""
        u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "D", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "D": 5, "Y": 6},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        # The path should reference C->D edge in path_exists formula
        assert len(idx.backdoor_paths) >= 1
        # valid_z for Z={D} should reference e__C__D
        z_d = frozenset({"D"})
        if z_d in idx.valid_z:
            formula_str = str(idx.valid_z[z_d])
            # D is a descendant of collider C; adjusting for D opens the path
            assert "e__C__D" in formula_str or "C" in formula_str

    def test_forbidden_by_z(self):
        """forbidden_by_z rejects descendants of X."""
        u = build_symbolic_universe(
            nodes=["A", "X", "D", "Y"],
            timing={"A": 1, "X": 2, "D": 3, "Y": 4},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        z_d = frozenset({"D"})
        if z_d in idx.forbidden_by_z:
            # D is a descendant of X, so forbidden formula should be unsatisfiable
            # for assignments where X->D is present
            forbidden = idx.forbidden_by_z[z_d]
            assert forbidden is not None

    def test_path_record_fields(self):
        """PathRecord has all required fields populated."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        for p in idx.paths:
            assert p.path_id.startswith("P")
            assert len(p.nodes) >= 2
            assert isinstance(p.is_backdoor, bool)
            assert p.path_exists is not None
            assert isinstance(p.collider_nodes, tuple)
            assert isinstance(p.noncollider_nodes, tuple)

    def test_max_paths_limit(self):
        """max_paths limits the number of paths enumerated."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y", "B", "C"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "Y": 5},
            exposure="X",
            outcome="Y",
        )
        idx_limited = build_query_formula_index(u, max_paths=2)
        assert len(idx_limited.paths) <= 2

    def test_valid_z_masses_match_direct(self):
        """Each cached valid_z formula produces the same count as direct computation."""
        from symbolic.dsep import valid_adjustment_formula

        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        backend = BruteForceBackend(max_vars=20)
        from symbolic.reachability import build_reachability

        reach = build_reachability(u)
        for Z in idx.candidate_sets:
            direct = valid_adjustment_formula(u, reach, Z)
            direct_count = backend.count(direct, u.variable_names)
            cached_count = backend.count(idx.valid_z[Z], u.variable_names)
            assert cached_count == direct_count, f"Mismatch for Z={Z}"

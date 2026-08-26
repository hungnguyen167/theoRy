from __future__ import annotations

from symbolic.classes import build_query_classes
from symbolic.delta_u import (
    SymbolicDeltaUEngine,
    class_distribution,
    compatibility_concentration,
    distribution_entropy,
)
from symbolic.universe import build_symbolic_universe


class TestDistributionMetrics:
    def test_entropy_nonnegative(self):
        """Entropy is always >= 0."""
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        ent = distribution_entropy(result)
        assert ent >= 0.0

    def test_entropy_positive_multiple_classes(self):
        """Entropy > 0 when there are multiple classes with non-trivial proportions."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        if len(result.classes) > 1:
            ent = distribution_entropy(result)
            assert ent > 0

    def test_concentration_between_0_and_1(self):
        """Concentration is between 0 and 1."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        conc = compatibility_concentration(result)
        assert 0 < conc <= 1.0

    def test_class_distribution_keys(self):
        """class_distribution returns class_id -> proportion mapping."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        dist = class_distribution(result)
        assert isinstance(dist, dict)
        for cid, prop in dist.items():
            assert isinstance(cid, str)
            assert 0 <= prop <= 1


class TestSymbolicDeltaUEngine:
    def test_basic_delta_u(self):
        """Delta-U returns results for each edge."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        engine = SymbolicDeltaUEngine()
        results = engine.compute_delta_u(u, top_k=10, mode="full")
        assert len(results) > 0
        for r in results:
            assert "delta_u" in r
            assert "source" in r
            assert "target" in r
            assert "best_resolution" in r

    def test_delta_u_sorted_descending(self):
        """Results are sorted by delta_u descending."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        engine = SymbolicDeltaUEngine()
        results = engine.compute_delta_u(u, top_k=10, mode="full")
        for i in range(len(results) - 1):
            assert results[i]["delta_u"] >= results[i + 1]["delta_u"]

    def test_top_k_limits_results(self):
        """top_k limits the number of returned results."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        engine = SymbolicDeltaUEngine()
        results = engine.compute_delta_u(u, top_k=1, mode="full")
        assert len(results) <= 1

    def test_result_has_metadata(self):
        """Each result has required metadata fields."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        engine = SymbolicDeltaUEngine()
        results = engine.compute_delta_u(u, top_k=5, mode="full")
        for r in results:
            assert "component_id" in r
            assert "type" in r
            assert "exact" in r
            assert "mode" in r
            assert "baseline_entropy" in r
            assert "entropy_if_present" in r
            assert "entropy_if_absent" in r
            assert "branch_mass_present" in r
            assert "branch_mass_absent" in r
            assert r["exact"] is True
            assert r["mode"] == "symbolic_full"

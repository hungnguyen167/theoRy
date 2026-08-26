from __future__ import annotations

from symbolic.simulation import SymbolicSimulationEngine, _build_paper_template


def test_symbolic_templates_use_one_based_timings():
    small = _build_paper_template("X", "Y", "paper_small")
    assert small["universe"].timing == {
        "A": 1,
        "X": 2,
        "B": 3,
        "C": 4,
        "D": 5,
        "Y": 6,
    }

    paper = _build_paper_template("X", "Y", "paper_13")
    assert list(paper["universe"].timing.values()) == list(range(1, 14))


class TestConsensusIllusion:
    def test_returns_scenario_data(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_consensus_illusion(seed=42, mode="sampled", n_samples=200)
        assert result["scenario"] == "consensus_illusion"
        assert "classes" in result
        assert "metrics" in result
        assert "artifacts" in result

    def test_has_metrics(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_consensus_illusion(seed=42, mode="sampled", n_samples=200)
        m = result["metrics"]
        assert "surface_structural_consensus" in m
        assert "query_class_entropy" in m
        assert "dominant_class_share" in m
        assert "causal_compatibility" in m
        assert "consensus_gap" in m
        assert "classes_count" in m

    def test_classes_count_at_least_two(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_consensus_illusion(seed=42, mode="sampled", n_samples=200)
        assert result["metrics"]["classes_count"] >= 2

    def test_exact_flag(self):
        engine = SymbolicSimulationEngine()
        r1 = engine.run_consensus_illusion(seed=42, mode="sampled", n_samples=200)
        assert r1["exact"] is False
        assert r1["mode"] == "symbolic_sampled"

    def test_universe_summary(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_consensus_illusion(seed=42, mode="sampled", n_samples=200)
        us = result["universe_summary"]
        assert "nodes" in us
        assert "edge_count" in us
        assert us["edge_count"] > 0


class TestLynchpinOfCertainty:
    def test_returns_scenario_data(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_lynchpin_of_certainty(
            seed=42, mode="sampled", n_samples=200
        )
        assert result["scenario"] == "lynchpin_of_certainty"
        assert "classes" in result
        assert "metrics" in result

    def test_has_phase_transition_metrics(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_lynchpin_of_certainty(
            seed=42, mode="sampled", n_samples=200
        )
        m = result["metrics"]
        assert "baseline_entropy" in m
        assert "post_resolution_expected_entropy" in m
        assert "phase_transition_score" in m
        assert "lynchpin_rank" in m
        assert "lynchpin_edge" in m

    def test_baseline_entropy_nonnegative(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_lynchpin_of_certainty(
            seed=42, mode="sampled", n_samples=200
        )
        assert result["metrics"]["baseline_entropy"] >= 0


class TestGhostDiscovery:
    def test_returns_scenario_data(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_ghost_discovery(seed=42, mode="sampled", n_samples=200)
        assert result["scenario"] == "ghost_discovery"
        assert "classes" in result
        assert "metrics" in result

    def test_has_ghost_metrics(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_ghost_discovery(seed=42, mode="sampled", n_samples=200)
        m = result["metrics"]
        assert "classes_detected" in m
        assert "ghost_class_count" in m

    def test_classes_detected_positive(self):
        engine = SymbolicSimulationEngine()
        result = engine.run_ghost_discovery(seed=42, mode="sampled", n_samples=200)
        assert result["metrics"]["classes_detected"] >= 1

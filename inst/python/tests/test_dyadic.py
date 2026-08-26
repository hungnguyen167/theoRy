from __future__ import annotations

import pytest

from dyadic.engine import DyadicEngine, DyadicError
from registry.loader import RegistryLoader
from registry.schema import ComponentRegistry
from state.tensor import StateTensor


def _make_registry() -> ComponentRegistry:
    records = [
        {
            "comp_id": "C0001",
            "type": "node",
            "source": "X",
            "target": None,
            "direction": None,
            "description": "Variable X",
        },
        {
            "comp_id": "C0002",
            "type": "node",
            "source": "Y",
            "target": None,
            "direction": None,
            "description": "Variable Y",
        },
        {
            "comp_id": "C0003",
            "type": "edge",
            "source": "X",
            "target": "Y",
            "direction": "->",
            "description": "X->Y",
        },
    ]
    return RegistryLoader.from_records(records)


def _make_state(registry: ComponentRegistry, records: list[dict]) -> StateTensor:
    return StateTensor.from_records(registry, records)


class TestDyadicEngineCompare:
    def test_identical_models(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["dyad_id"] == "M0001__M0002"
        assert result["ego_id"] == "M0001"
        assert result["alter_id"] == "M0002"
        assert result["similarity_rate"] == 1.0
        assert result["timing_compatible"] is True
        assert result["existence_conflict"] is False
        assert result["conflicting_components"] == []
        assert result["repair_cost"] == 0

    def test_completely_different_models(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # Under sparse semantics: M0002 has no present nodes;
        # M0001 has 2 present nodes -> 0 shared, 2 union -> 0.0
        assert result["similarity_rate"] == 0.0
        assert result["existence_conflict"] is True  # node presence conflict

    def test_no_causal_components_returns_one(self):
        reg = _make_registry()
        records = [
            {
                "model_id": "M0001",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["similarity_rate"] == 1.0

    def test_partial_overlap_similarity(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # Nodes: C0001 shared present; C0002 M1 present M2 absent
        # Edge C0003: M1 has both endpoints (applicable), M2 lacks Y (inapplicable)
        # One-sided applicable edge -> union+1, no shared
        # shared=1 (C0001 node), union=2 (both nodes) + 1 (one-sided edge) = 3 -> 1/3
        assert result["similarity_rate"] == pytest.approx(1 / 3)
        assert (
            result["repair_cost"] == 2
        )  # node conflict (C0002) + edge one-sided (C0003)

    def test_causal_non_causal_conflict(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "non-causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["existence_conflict"] is True
        assert "C0001" in result["conflicting_components"]
        assert len(result["conflicting_components"]) == 1

    def test_repair_cost_count(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "non-causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # M2 C0001=non-causal -> node NOT present -> 1 node diff
        # M2 C0002=causal -> present. Edge C0003 inapplicable in M2 (missing X).
        # One-sided applicable edge -> repair+1
        # repair_cost = 2 (node presence difference on C0001 + edge one-sided on C0003)
        assert result["repair_cost"] == 2

    def test_timing_mismatch(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 5},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # C0001 has timing 1 vs 5 -> mismatch -> timing_compatible=False
        assert result["timing_compatible"] is False

    def test_timing_match(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["timing_compatible"] is True

    def test_temporal_impossibility_excludes_edge(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 5},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 3},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # M0002 has source(X)=5 >= target(Y)=3 -> temporal violation
        assert result["timing_compatible"] is False
        # Under sparse: binding isstill applicable, scored structurally
        assert result["similarity_rate"] == 1.0

    def test_temporal_integrity_passes(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 3},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 7},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        # Both: source(X) < target(Y) -> OK
        # But timing mismatch on C0001 (1 vs 3) and C0002 (2 vs 7)
        assert result["timing_compatible"] is False  # timing values differ

    def test_temporal_no_timing_does_not_fail(self):
        reg = _make_registry()
        records = [
            {
                "model_id": "M0001",
                "comp_id": "C0001",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0002",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["timing_compatible"] is True


class TestDyadicEngineCompareAll:
    def test_compare_all_returns_all_pairs(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        results = engine.compare_all(state, reg)
        assert len(results) == 3 * 2  # N*(N-1) = 6
        dyads = [(r["ego_id"], r["alter_id"]) for r in results]
        for a in ["M0001", "M0002", "M0003"]:
            for b in ["M0001", "M0002", "M0003"]:
                if a == b:
                    continue
                assert (a, b) in dyads

    def test_compare_all_with_model_ids_subset(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        results = engine.compare_all(state, reg, model_ids=["M0001", "M0003"])
        assert len(results) == 2  # N*(N-1) = 2
        dyads = {(r["ego_id"], r["alter_id"]) for r in results}
        assert dyads == {("M0001", "M0003"), ("M0003", "M0001")}

    def test_compare_matrix_returns_square_matrix_with_self_dyads(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        matrix = engine.compare_matrix(state, reg)
        assert len(matrix) == 2
        assert all(len(row) == 2 for row in matrix)
        assert matrix[0][0]["dyad_id"] == "M0001__M0001"
        assert matrix[0][0]["ego_id"] == "M0001"
        assert matrix[0][0]["alter_id"] == "M0001"
        assert matrix[0][0]["similarity_rate"] == 1.0

    def test_compare_pairs_returns_directed_non_self_pairs(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        results = engine.compare_pairs(state, reg)
        # 3 models => 3*2 = 6 directed non-self pairs
        assert len(results) == 6
        pairs = {(r["ego_id"], r["alter_id"]) for r in results}
        expected = {
            ("M0001", "M0002"),
            ("M0001", "M0003"),
            ("M0002", "M0001"),
            ("M0002", "M0003"),
            ("M0003", "M0001"),
            ("M0003", "M0002"),
        }
        assert pairs == expected
        for r in results:
            assert r["dyad_id"] == f"{r['ego_id']}__{r['alter_id']}"

    def test_compare_pairs_no_self_dyads(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        results = engine.compare_pairs(state, reg)
        # 2 models => 2*1 = 2 directed non-self pairs, no self
        assert len(results) == 2
        for r in results:
            assert r["ego_id"] != r["alter_id"]

    def test_compare_pairs_includes_both_directions(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        results = engine.compare_pairs(state, reg)
        # 2 models => 2 directed pairs
        assert len(results) == 2
        dyad_ids = {r["dyad_id"] for r in results}
        assert "M0001__M0002" in dyad_ids
        assert "M0002__M0001" in dyad_ids


class TestDyadicEngineErrors:
    def test_edge_with_missing_target_node_handled(self):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X dup",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
                "fixed_status": "causal",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, state_records)
        engine = DyadicEngine()
        # Under sparse semantics, edge C0003 (X->Y) is inapplicable
        # because Y has no node component, so no error is raised
        result = engine.compare("M0001", "M0002", state, reg)
        assert result["similarity_rate"] == 1.0

    def test_cycle_detection_raises_error(self):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
                "fixed_status": "causal",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "Y",
                "target": "X",
                "direction": "->",
                "description": "Y->X",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0004",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0004",
                "status": "unknown",
                "timing": None,
            },
        ]
        state = _make_state(reg, state_records)
        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="Invalid DAG: cycle detected"):
            engine.compare("M0001", "M0002", state, reg)


class TestCausalWrapper:
    """Tests for dyadic/causal.py -- mocked, no real R/dagitty required."""

    @staticmethod
    def _mock_ensure_r(wrapper, monkeypatch):
        def _stub():
            wrapper._r_available = True
            return True

        monkeypatch.setattr(wrapper, "_ensure_r", _stub)

    @staticmethod
    def _install_fake_rpy2(monkeypatch, dagitty_attrs=None, dagitty_module=None):
        import sys
        from unittest.mock import MagicMock

        if dagitty_module is not None:
            fake_dagitty = dagitty_module
        else:
            fake_dagitty = MagicMock()
            if dagitty_attrs:
                for attr, val in dagitty_attrs.items():
                    setattr(fake_dagitty, attr, val)

        fake_importr = MagicMock(return_value=fake_dagitty)
        fake_packages = MagicMock()
        fake_packages.importr = fake_importr
        fake_vectors = MagicMock()
        fake_vectors.StrVector = lambda x: list(x)
        fake_robjects = MagicMock()
        fake_robjects.packages = fake_packages
        fake_robjects.vectors = fake_vectors

        monkeypatch.setitem(sys.modules, "rpy2", MagicMock())
        monkeypatch.setitem(sys.modules, "rpy2.robjects", fake_robjects)
        monkeypatch.setitem(sys.modules, "rpy2.robjects.packages", fake_packages)
        monkeypatch.setitem(sys.modules, "rpy2.robjects.vectors", fake_vectors)
        monkeypatch.setitem(sys.modules, "rpy2.robjects.conversion", MagicMock())

        return fake_dagitty, fake_importr

    def _sample_dag(self):
        return {
            "nodes": ["X", "Y", "Z"],
            "edges": [("X", "Y"), ("Z", "Y")],
            "exposure": "X",
            "outcome": "Y",
        }

    def test_compute_adjustment_sets_mocked(self, monkeypatch):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        self._mock_ensure_r(wrapper, monkeypatch)

        def fake_adjustment_sets(dag_obj, exposure, outcome, effect=None):
            class FakeSets:
                def __iter__(self):
                    return iter([["X", "Z"]])

            return FakeSets()

        self._install_fake_rpy2(
            monkeypatch,
            dagitty_attrs={
                "dagitty": lambda s: s,
                "adjustmentSets": fake_adjustment_sets,
            },
        )

        result = wrapper.compute_adjustment_sets(self._sample_dag())
        assert result == [["X", "Z"]]

    def test_check_identification_mocked(self, monkeypatch):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        self._mock_ensure_r(wrapper, monkeypatch)

        self._install_fake_rpy2(
            monkeypatch,
            dagitty_attrs={
                "dagitty": lambda s: s,
                "adjustmentSets": lambda dag_obj, exposure, outcome: [[]],
            },
        )

        result = wrapper.check_identification(self._sample_dag())
        assert result is True

    def test_check_identification_no_is_identified_fallback(self, monkeypatch):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        self._mock_ensure_r(wrapper, monkeypatch)

        class FakeDagittyModule:
            @staticmethod
            def dagitty(spec):
                return spec

            @staticmethod
            def adjustmentSets(dag_obj, exposure, outcome):
                return [[]]

        self._install_fake_rpy2(monkeypatch, dagitty_module=FakeDagittyModule)

        result = wrapper.check_identification(self._sample_dag())
        assert result is True

    def test_missing_dagitty_raises_causal_error(self):
        from dyadic.causal import CausalWrapper, CausalError

        wrapper = CausalWrapper()
        wrapper._r_available = False

        with pytest.raises(CausalError, match="already checked"):
            wrapper.compute_adjustment_sets(self._sample_dag())

    def test_compare_mas_same(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=[["X", "Z"]],
            mas_b=[["X", "Z"]],
        )
        assert result["compatible"] is True

    def test_compare_mas_different(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=[["X", "Z"]],
            mas_b=[["Y", "W"]],
        )
        assert result["compatible"] is False

    def test_compare_mas_both_empty(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(mas_a=[], mas_b=[])
        assert result["compatible"] is False

    def test_compare_mas_one_empty(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=[["X"]],
            mas_b=[],
        )
        assert result["compatible"] is False

    def test_compare_mas_flat_list_same(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=["X", "Z"],
            mas_b=["X", "Z"],
        )
        assert result["compatible"] is True

    def test_compare_mas_flat_list_different(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=["X", "Z", "W"],
            mas_b=["X", "Z"],
        )
        assert result["compatible"] is False

    def test_compare_mas_multiple_sets_partial_overlap(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=[["X", "Z"], ["X", "W"]],
            mas_b=[["X", "Z"], ["Y"]],
        )
        assert result["compatible"] is True

    def test_compare_mas_multiple_sets_no_overlap(self):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        result = wrapper.compare_mas(
            mas_a=[["X"], ["W"]],
            mas_b=[["Y"], ["Z"]],
        )
        assert result["compatible"] is False

    def test_invalid_dag_raises_causal_error(self, monkeypatch):
        from dyadic.causal import CausalWrapper, CausalError

        wrapper = CausalWrapper()
        self._mock_ensure_r(wrapper, monkeypatch)
        self._install_fake_rpy2(monkeypatch)

        with pytest.raises(CausalError, match="must contain at least one node"):
            wrapper.compute_adjustment_sets({"nodes": [], "edges": []})

    def test_dag_spec_to_dagitty_syntax(self):
        from dyadic.causal import CausalWrapper

        dag_spec = self._sample_dag()
        result = CausalWrapper._dag_spec_to_dagitty(dag_spec)
        assert "dag {" in result
        assert "  X" in result
        assert "  Y" in result
        assert "  Z" in result
        assert "X -> Y" in result
        assert "Z -> Y" in result

    def test_dag_spec_to_dagitty_includes_nodes_without_edges(self):
        from dyadic.causal import CausalWrapper

        result = CausalWrapper._dag_spec_to_dagitty({"nodes": ["X", "Y"], "edges": []})
        assert "  X" in result
        assert "  Y" in result

    def test_invalid_dag_cyclic_handled(self, monkeypatch):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        self._mock_ensure_r(wrapper, monkeypatch)

        def fake_adjustment_sets_raises(dag_obj, exposure, outcome):
            raise RuntimeError("graph contains cycles")

        self._install_fake_rpy2(
            monkeypatch,
            dagitty_attrs={
                "dagitty": lambda s: s,
                "adjustmentSets": fake_adjustment_sets_raises,
            },
        )

        from dyadic.causal import CausalError

        with pytest.raises(CausalError, match="Failed to compute adjustment sets"):
            wrapper.compute_adjustment_sets(self._sample_dag())

    def test_rpy2_mock_fixture(self, rpy2_mock):
        from dyadic.causal import CausalWrapper

        wrapper = CausalWrapper()
        mas = wrapper.compute_adjustment_sets({})
        assert mas == rpy2_mock["adjustment_sets"]
        assert mas == [["X", "Z"]]


class TestExtendedDyadicMetrics:
    """Tests for Story 2.2 -- full-mode causal metrics in DyadicEngine."""

    @staticmethod
    def _make_causal_registry():
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
                "fixed_status": "causal",
            },
        ]
        from registry.loader import RegistryLoader

        return RegistryLoader.from_records(records)

    @staticmethod
    def _make_causal_state(registry, model_a_causal=True, model_b_causal=True):
        records = [
            {
                "model_id": "M0001",
                "comp_id": "C0001",
                "status": "causal",
                "timing": 1,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0002",
                "status": "causal",
                "timing": 2,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal" if model_a_causal else "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "causal",
                "timing": 1,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0002",
                "status": "causal",
                "timing": 2,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "causal" if model_b_causal else "non-causal",
                "timing": None,
            },
        ]
        from state.tensor import StateTensor

        return StateTensor.from_records(registry, records)

    @staticmethod
    def _install_causal_mock(
        monkeypatch,
        mas_a=None,
        mas_b=None,
        identified_a=True,
        identified_b=True,
        alternate=False,
    ):
        from dyadic.causal import CausalWrapper

        def _mas_for(which_mas, which_ident, default):
            if which_mas is not None:
                return which_mas
            return default if which_ident else []

        mas_a = _mas_for(mas_a, identified_a, [["Z"]])
        mas_b = _mas_for(mas_b, identified_b, [["Z"]])

        calls = 0

        def fake_mas(self, dag_spec):
            nonlocal calls
            if alternate:
                result = mas_a if calls == 0 else mas_b
                calls += 1
                return result
            if ("X", "Y") in dag_spec.get("edges", []):
                return mas_a
            return mas_b

        monkeypatch.setattr(
            CausalWrapper,
            "compute_adjustment_sets",
            fake_mas,
        )

    def test_compare_full_includes_causal_fields(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert "mas_ego" in result
        assert "mas_alter" in result
        assert "mas_compatible" in result
        assert "identified_ego" in result
        assert "identified_alter" in result
        assert "identified_compatible" in result

    def test_compare_basic_excludes_causal_fields(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)

        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        result = engine.compare("M0001", "M0002", state, reg, mode="basic")

        assert "mas_ego" not in result
        assert "mas_alter" not in result
        assert "mas_compatible" not in result
        assert "identified_ego" not in result
        assert "identified_alter" not in result
        assert "identified_compatible" not in result
        assert "similarity_rate" in result

    def test_compare_full_mas_compatible_true(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[["Z"]],
            mas_b=[["Z"]],
        )

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert result["mas_compatible"] is True

    def test_compare_full_mas_compatible_false(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        state.set_timing("M0002", "C0002", 3)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[["Z"]],
            mas_b=[["W"]],
            alternate=True,
        )

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert result["mas_compatible"] is False

    def test_compare_full_identification_agreement(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[["Z"]],
            mas_b=[["Z"]],
        )

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert result["mas_compatible"] is True

    def test_compare_full_identification_disagreement(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        state.set_timing("M0002", "C0002", 3)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[["Z"]],
            mas_b=[["W"]],
            alternate=True,
        )

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert result["mas_compatible"] is False

    def test_compare_full_cyclic_dag_skips_causal(self):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "Y",
                "target": "X",
                "direction": "->",
                "description": "Y->X",
            },
        ]
        from registry.loader import RegistryLoader

        reg = RegistryLoader.from_records(records)

        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0004",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0004",
                "status": "unknown",
                "timing": None,
            },
        ]
        from state.tensor import StateTensor

        state = StateTensor.from_records(reg, state_records)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        wrapper = CausalWrapper()

        # M0001 has a cycle, so structural validation raises
        with pytest.raises(DyadicError, match="cycle detected"):
            engine.compare(
                "M0001",
                "M0002",
                state,
                reg,
                mode="full",
                causal_wrapper=wrapper,
            )

    def test_compare_full_propagates_non_cyclic_causal_error(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)

        from dyadic.causal import CausalError, CausalWrapper
        from dyadic.engine import DyadicEngine

        def raise_missing_dagitty(self, dag_spec):
            raise CausalError(
                "Cannot load R package dagitty. "
                "Install in R with: install.packages('dagitty')\n"
                "Underlying error: package not found"
            )

        monkeypatch.setattr(
            CausalWrapper, "compute_adjustment_sets", raise_missing_dagitty
        )

        engine = DyadicEngine()
        wrapper = CausalWrapper()

        with pytest.raises(CausalError, match="Cannot load R package dagitty"):
            engine.compare(
                "M0001",
                "M0002",
                state,
                reg,
                mode="full",
                causal_wrapper=wrapper,
                exposure="X",
                outcome="Y",
            )

    def test_compare_full_requires_causal_wrapper(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)

        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="causal_wrapper is required"):
            engine.compare("M0001", "M0002", state, reg, mode="full")

    def test_compare_invalid_mode_raises(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)

        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="Mode must be one of"):
            engine.compare("M0001", "M0002", state, reg, mode="invalid")

    def test_compare_pairs_full_keeps_ego_alter_ordering(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        results = engine.compare_pairs(
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert len(results) == 2
        r = results[0]
        assert r["ego_id"] == "M0001"
        assert r["alter_id"] == "M0002"
        assert r["dyad_id"] == "M0001__M0002"
        assert "mas_ego" in r
        assert "identified_compatible" in r

    def test_compare_full_causal_wrapper_passthrough(self, monkeypatch):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        results = engine.compare_all(
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )

        assert len(results) == 2
        for r in results:
            assert "mas_ego" in r
            assert "identified_compatible" in r

    def test_default_exposure_outcome_first_last(self):
        from dyadic.engine import DyadicEngine

        exp, out = DyadicEngine._default_exposure_outcome(["X", "Y", "Z"])
        assert exp == "X"
        assert out == "Z"

    def test_default_exposure_outcome_two_nodes(self):
        from dyadic.engine import DyadicEngine

        exp, out = DyadicEngine._default_exposure_outcome(["A", "B"])
        assert exp == "A"
        assert out == "B"

    def test_default_exposure_outcome_requires_two_nodes(self):
        from dyadic.engine import DyadicEngine, DyadicError

        with pytest.raises(DyadicError, match="At least two nodes"):
            DyadicEngine._default_exposure_outcome(["X"])

    # ------------------------------------------------------------------
    # identified_compatible semantics (fixed-direct-edge identification)
    # ------------------------------------------------------------------

    def test_identified_compatible_both_true(self, monkeypatch):
        """identified_compatible is native and ignores the legacy ID wrapper."""
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        class MockIDWrapper:
            def __init__(self):
                self.calls = 0

            def identify_total_effect(self, **kwargs):
                self.calls += 1
                return True, "P(Y|X)"

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        id_wrapper = MockIDWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            identification_wrapper=id_wrapper,
            exposure="X",
            outcome="Y",
        )
        assert result["identified_ego"] is True
        assert result["identified_alter"] is True
        assert result["identified_compatible"] is True
        assert id_wrapper.calls == 0

    def test_identified_compatible_one_false(self, monkeypatch):
        """A non-causal fixed direct edge is rejected before profile building."""
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg, model_b_causal=False)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        with pytest.raises(DyadicError, match="fixed direct"):
            engine.compare(
                "M0001",
                "M0002",
                state,
                reg,
                mode="full",
                causal_wrapper=wrapper,
                exposure="X",
                outcome="Y",
            )

    def test_identified_compatible_no_wrapper_returns_none(self, monkeypatch):
        """The native predicate runs without an identification wrapper."""
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )
        assert result["identified_ego"] is True
        assert result["identified_alter"] is True
        assert result["identified_compatible"] is True

    # ------------------------------------------------------------------
    # End-to-end node-set compatibility (new identification contract)
    # ------------------------------------------------------------------

    @staticmethod
    def _make_latent_registry():
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
                "observed": True,
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
                "observed": True,
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "Z",
                "observed": True,
            },
            {
                "comp_id": "C0004",
                "type": "node",
                "source": "U",
                "target": None,
                "direction": None,
                "description": "U",
                "observed": False,
            },
            {
                "comp_id": "C0005",
                "type": "node",
                "source": "V",
                "target": None,
                "direction": None,
                "description": "V",
                "observed": False,
            },
            {
                "comp_id": "C0006",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
                "fixed_status": "causal",
            },
            {
                "comp_id": "C0007",
                "type": "edge",
                "source": "U",
                "target": "X",
                "direction": "->",
                "description": "U->X",
            },
            {
                "comp_id": "C0008",
                "type": "edge",
                "source": "U",
                "target": "Y",
                "direction": "->",
                "description": "U->Y",
            },
            {
                "comp_id": "C0009",
                "type": "edge",
                "source": "V",
                "target": "X",
                "direction": "->",
                "description": "V->X",
            },
            {
                "comp_id": "C0010",
                "type": "edge",
                "source": "Z",
                "target": "Y",
                "direction": "->",
                "description": "Z->Y",
            },
        ]
        return RegistryLoader.from_records(records)

    def test_dag_spec_preserves_declared_metadata_for_ordinary_model(self, monkeypatch):
        from dyadic.engine import DyadicEngine

        reg = self._make_latent_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 5},
            {"model_id": "M0001", "comp_id": "C0003", "status": "present", "timing": 4},
            {"model_id": "M0001", "comp_id": "C0004", "status": "present", "timing": 1},
            {
                "model_id": "M0001",
                "comp_id": "C0005",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0007",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0008",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0009",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0010",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        spec = engine._dag_spec_for_model(
            "M0001", state, reg, exposure="X", outcome="Y"
        )
        assert spec["query_nodes_missing"] is False
        assert set(spec["declared_nodes"]) == {"X", "Y", "Z", "U"}
        assert ("X", "Y") in spec["declared_directed_edges"]
        assert ("U", "X") in spec["declared_directed_edges"]
        assert ("U", "Y") in spec["declared_directed_edges"]
        assert ("Z", "Y") in spec["declared_directed_edges"]
        assert "V" not in spec["declared_nodes"]

    def test_dag_spec_preserves_declared_metadata_when_query_nodes_missing(
        self, monkeypatch
    ):
        from dyadic.engine import DyadicEngine

        reg = self._make_latent_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 5},
            {
                "model_id": "M0001",
                "comp_id": "C0003",
                "status": "non-causal",
                "timing": None,
            },
            {"model_id": "M0001", "comp_id": "C0004", "status": "present", "timing": 1},
            {
                "model_id": "M0001",
                "comp_id": "C0005",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0007",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0008",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0009",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0010",
                "status": "causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        engine = DyadicEngine()
        # Z is a registered node but absent from this model -> query_nodes_missing.
        spec = engine._dag_spec_for_model(
            "M0001", state, reg, exposure="X", outcome="Z"
        )
        assert spec["query_nodes_missing"] is True
        assert set(spec["declared_nodes"]) == {"X", "Y", "U"}
        assert ("X", "Y") in spec["declared_directed_edges"]

    def test_extra_declared_latent_non_intermediate_makes_incompatible(
        self, monkeypatch
    ):
        reg = self._make_latent_registry()
        # M0001: U present (common cause, non-intermediate). M0002: U absent.
        records = []
        for mid, u_present, others in [
            ("M0001", True, None),
            ("M0002", False, None),
        ]:
            records.append(
                {"model_id": mid, "comp_id": "C0001", "status": "present", "timing": 2}
            )
            records.append(
                {"model_id": mid, "comp_id": "C0002", "status": "present", "timing": 4}
            )
            records.append(
                {
                    "model_id": mid,
                    "comp_id": "C0004" if u_present else "C0004",
                    "status": "present" if u_present else "non-causal",
                    "timing": 1 if u_present else None,
                }
            )
            records.append(
                {
                    "model_id": mid,
                    "comp_id": "C0006",
                    "status": "causal",
                    "timing": None,
                }
            )
            if u_present:
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": "C0007",
                        "status": "causal",
                        "timing": None,
                    }
                )
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": "C0008",
                        "status": "causal",
                        "timing": None,
                    }
                )
            else:
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": "C0007",
                        "status": "non-causal",
                        "timing": None,
                    }
                )
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": "C0008",
                        "status": "non-causal",
                        "timing": None,
                    }
                )
        state = _make_state(reg, records)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        class MockIDWrapper:
            def identify_total_effect(self, **kwargs):
                return True, "P(Y|X)"

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            identification_wrapper=MockIDWrapper(),
            exposure="X",
            outcome="Y",
        )
        assert result["identified_ego"] is True
        assert result["identified_alter"] is True
        # Same observed projection but M0001 has an extra declared latent U.
        assert result["identified_compatible"] is False

    def test_declared_latent_on_directed_path_normalized_compatible(self, monkeypatch):
        # M0001: latent U on a directed X->U->Y path.
        # M0002: no U, direct X->Y. Declared-node presence differs.
        rec_registry = RegistryLoader.from_records(
            [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                    "observed": True,
                },
                {
                    "comp_id": "C0002",
                    "type": "node",
                    "source": "Y",
                    "target": None,
                    "direction": None,
                    "description": "Y",
                    "observed": True,
                },
                {
                    "comp_id": "C0004",
                    "type": "node",
                    "source": "U",
                    "target": None,
                    "direction": None,
                    "description": "U",
                    "observed": False,
                },
                {
                    "comp_id": "C0006",
                    "type": "edge",
                    "source": "X",
                    "target": "Y",
                    "direction": "->",
                    "description": "X->Y",
                    "fixed_status": "causal",
                },
                {
                    "comp_id": "C0007",
                    "type": "edge",
                    "source": "X",
                    "target": "U",
                    "direction": "->",
                    "description": "X->U",
                },
                {
                    "comp_id": "C0008",
                    "type": "edge",
                    "source": "U",
                    "target": "Y",
                    "direction": "->",
                    "description": "U->Y",
                },
            ]
        )
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0004", "status": "present", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0007",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0008",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 3},
            {
                "model_id": "M0002",
                "comp_id": "C0004",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0007",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0008",
                "status": "non-causal",
                "timing": None,
            },
        ]
        state = _make_state(rec_registry, records)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        class MockIDWrapper:
            def identify_total_effect(self, **kwargs):
                return True, "P(Y|X)"

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            rec_registry,
            mode="full",
            causal_wrapper=wrapper,
            identification_wrapper=MockIDWrapper(),
            exposure="X",
            outcome="Y",
        )
        assert result["identified_ego"] is True
        assert result["identified_alter"] is True
        assert result["identified_compatible"] is False

    def test_identified_compatible_false_when_non_intermediate_node_differs(
        self, monkeypatch
    ):
        """Individual ID may be true for both while node sets differ."""
        reg = self._make_latent_registry()
        # M0001 has observed Z present; M0002 does not. Z is a parent of Y and
        # not on the X->Y directed path, so it is a relevant non-intermediate.
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0003", "status": "present", "timing": 2},
            {
                "model_id": "M0001",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0001",
                "comp_id": "C0010",
                "status": "causal",
                "timing": None,
            },
            {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 3},
            {
                "model_id": "M0002",
                "comp_id": "C0003",
                "status": "non-causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0006",
                "status": "causal",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0010",
                "status": "non-causal",
                "timing": None,
            },
        ]
        state = _make_state(reg, records)
        self._install_causal_mock(monkeypatch)

        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        class MockIDWrapper:
            def identify_total_effect(self, **kwargs):
                return True, "P(Y|X)"

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            identification_wrapper=MockIDWrapper(),
            exposure="X",
            outcome="Y",
        )
        assert result["identified_ego"] is True
        assert result["identified_alter"] is True
        assert result["identified_compatible"] is False

    def test_mas_compatible_both_empty_false(self, monkeypatch):
        """Two models both having no valid MAS are NOT compatible."""
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[],
            mas_b=[],
        )
        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )
        assert result["mas_compatible"] is False

    def test_mas_compatible_empty_set_valid(self, monkeypatch):
        """Models sharing the valid empty adjustment set {} are compatible."""
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        self._install_causal_mock(
            monkeypatch,
            mas_a=[[]],
            mas_b=[[]],
        )
        from dyadic.causal import CausalWrapper
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        wrapper = CausalWrapper()
        result = engine.compare(
            "M0001",
            "M0002",
            state,
            reg,
            mode="full",
            causal_wrapper=wrapper,
            exposure="X",
            outcome="Y",
        )
        assert result["mas_compatible"] is True
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        from dyadic.engine import DyadicEngine

        engine = DyadicEngine()
        dag_spec = engine._dag_spec_for_model(
            "M0001",
            state,
            reg,
            exposure="X",
            outcome="Y",
        )
        assert dag_spec["exposure"] == "X"
        assert dag_spec["outcome"] == "Y"

    def test_invalid_exposure_raises_error(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="cannot find unique node component"):
            engine._dag_spec_for_model(
                "M0001",
                state,
                reg,
                exposure="Invalid",
                outcome="Y",
            )

    def test_partial_exposure_outcome_raises_error(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="Both or neither"):
            engine._dag_spec_for_model(
                "M0001",
                state,
                reg,
                exposure="X",
            )

    def test_invalid_outcome_raises_error(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="cannot find unique node component"):
            engine._dag_spec_for_model(
                "M0001",
                state,
                reg,
                exposure="X",
                outcome="Invalid",
            )

    def test_exposure_outcome_same_node_raises_error(self):
        reg = self._make_causal_registry()
        state = self._make_causal_state(reg)
        from dyadic.engine import DyadicEngine, DyadicError

        engine = DyadicEngine()
        with pytest.raises(DyadicError, match="distinct nodes"):
            engine._dag_spec_for_model(
                "M0001",
                state,
                reg,
                exposure="X",
                outcome="X",
            )


class TestBidirectionalCausal:
    """Bidirectional <-> edges in DAG specs, DAGitty serialization, and MAS."""

    @staticmethod
    def _dag_spec_to_dagitty(dag_spec):
        from dyadic.causal import CausalWrapper

        return CausalWrapper._dag_spec_to_dagitty(dag_spec)

    def test_directed_only_no_bidirected_line(self):
        """Directed-only graph produces no <-> line."""
        spec = {
            "nodes": ["X", "Y"],
            "edges": [("X", "Y")],
        }
        text = self._dag_spec_to_dagitty(spec)
        assert "->" in text
        assert "<->" not in text
        assert "~~" not in text

    def test_latent_nodes_are_marked_unobserved_for_dagitty(self):
        spec = {
            "nodes": ["U", "X", "Y"],
            "observed_nodes": ["X", "Y"],
            "edges": [("U", "X"), ("U", "Y")],
        }
        text = self._dag_spec_to_dagitty(spec)
        assert "U [unobserved]" in text
        assert "X [unobserved]" not in text
        assert "Y [unobserved]" not in text

    def test_bidirected_edge_emits_lt_gt(self):
        """bidirected_edges produces <-> not ~~."""
        spec = {
            "nodes": ["X", "Z", "Y"],
            "edges": [("X", "Y"), ("Z", "Y")],
            "bidirected_edges": [("X", "Z")],
        }
        text = self._dag_spec_to_dagitty(spec)
        assert "X <-> Z" in text
        assert "~~" not in text
        assert "X -> Z" not in text

    def test_bidirected_and_directed_both_in_string(self):
        """Combined spec has both -> and <-> lines."""
        spec = {
            "nodes": ["X", "Z", "Y"],
            "edges": [("X", "Y"), ("Z", "Y")],
            "bidirected_edges": [("X", "Z")],
        }
        text = self._dag_spec_to_dagitty(spec)
        assert "X -> Y" in text
        assert "Z -> Y" in text
        assert "X <-> Z" in text

    # ── 7.2 _dag_spec_for_model bidirected_edges ─────────────────────────

    def test_dag_spec_includes_bidirected_when_causal(self):
        """_dag_spec_for_model includes causal <-> in bidirected_edges."""
        from dyadic.engine import DyadicEngine
        from registry.loader import RegistryLoader

        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "component",
                "fixed_status": "causal",
            },
            {
                "comp_id": "C0005",
                "type": "edge",
                "source": "Z",
                "target": "Y",
                "direction": "->",
                "description": "component",
            },
            {
                "comp_id": "C0006",
                "type": "edge",
                "source": "X",
                "target": "Z",
                "direction": "<->",
                "description": "component",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
            {"model_id": "M0001", "comp_id": "C0003", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0005", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0006", "status": "causal"},
        ]
        from state.tensor import StateTensor

        state = StateTensor.from_records(reg, state_records)
        engine = DyadicEngine()

        spec = engine._dag_spec_for_model(
            "M0001", state, reg, exposure="X", outcome="Y"
        )
        assert ("X", "Z") in spec["bidirected_edges"]
        assert spec["edges"] == [("X", "Y"), ("Z", "Y")]
        assert spec["bidirected_edges"] == [("X", "Z")]

    def test_dag_spec_omits_non_causal_bidirected(self):
        """bidirected_edges is empty when <-> status is unknown or non-causal."""
        from dyadic.engine import DyadicEngine
        from registry.loader import RegistryLoader

        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "component",
                "fixed_status": "causal",
            },
            {
                "comp_id": "C0005",
                "type": "edge",
                "source": "X",
                "target": "Z",
                "direction": "<->",
                "description": "component",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
            {"model_id": "M0001", "comp_id": "C0003", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0005", "status": "unknown"},
        ]
        from state.tensor import StateTensor

        state = StateTensor.from_records(reg, state_records)
        engine = DyadicEngine()

        spec = engine._dag_spec_for_model(
            "M0001", state, reg, exposure="X", outcome="Y"
        )
        assert spec["bidirected_edges"] == []

    # ── 7.2 MAS impact and compatibility ─────────────────────────────────

    def test_mas_changes_with_bidirected(self):
        """MAS differs when bidirected_edges alters the graph."""
        specs_received = []

        class FakeWrapper:
            @staticmethod
            def compute_adjustment_sets(dag_spec):
                specs_received.append(dag_spec.get("bidirected_edges", []))
                if dag_spec.get("bidirected_edges"):
                    return [["Z"]]
                return []

            @staticmethod
            def compare_mas(mas_a, mas_b):
                sets_a = {frozenset(s) for s in mas_a}
                sets_b = {frozenset(s) for s in mas_b}
                return {
                    "compatible": (
                        bool(sets_a & sets_b)
                        if sets_a and sets_b
                        else not (sets_a or sets_b)
                    )
                }

        result = DyadicEngine().compare_pairs(
            self._make_confounded_state(),
            self._make_confounded_registry(),
            mode="full",
            causal_wrapper=FakeWrapper(),
            exposure="X",
            outcome="Y",
        )

        assert len(result) >= 1
        assert len(specs_received) >= 2
        assert [("X", "Z")] in specs_received
        assert [] in specs_received
        assert any(dyad.get("mas_compatible") is False for dyad in result)
        assert all(dyad.get("identified_compatible") is not None for dyad in result)

    @staticmethod
    def _make_confounded_registry():
        from registry.loader import RegistryLoader

        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "component",
                "fixed_status": "causal",
            },
            {
                "comp_id": "C0005",
                "type": "edge",
                "source": "Z",
                "target": "Y",
                "direction": "->",
                "description": "component",
            },
            {
                "comp_id": "C0006",
                "type": "edge",
                "source": "X",
                "target": "Z",
                "direction": "<->",
                "description": "component",
            },
        ]
        return RegistryLoader.from_records(records)

    @staticmethod
    def _make_confounded_state():
        from state.tensor import StateTensor

        reg = TestBidirectionalCausal._make_confounded_registry()
        # M0001: X->Y causal, Z->Y causal, X<->Z causal
        # M0002: X->Y causal, Z->Y causal, X<->Z non-causal
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
            {"model_id": "M0001", "comp_id": "C0003", "status": "present", "timing": 3},
            {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0005", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0006", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 2},
            {"model_id": "M0002", "comp_id": "C0003", "status": "present", "timing": 3},
            {"model_id": "M0002", "comp_id": "C0004", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0005", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0006", "status": "non-causal"},
        ]
        return StateTensor.from_records(reg, records)

    # ── 7.2 bidirected does not affect timing compatibility ──────────────

    def test_bidirected_does_not_break_timing_compat(self):
        """Causal <-> with reversed timings still yields timing_compatible=True."""
        from dyadic.engine import DyadicEngine

        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Z",
                "direction": "<->",
                "description": "X <-> Z",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 2},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 2},
            {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0003", "status": "causal"},
        ]
        state = StateTensor.from_records(reg, state_records)
        engine = DyadicEngine()
        compat = engine._check_timing_compatibility("M0001", "M0002", state, reg)
        assert compat is True

    # ── 7.2 bidirected does not trigger cycle ────────────────────────────

    def test_bidirected_does_not_trigger_cycle(self):
        """A <-> edge does not make _validate_acyclic raise."""
        from dyadic.engine import DyadicEngine

        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "component",
            },
            {
                "comp_id": "C0003",
                "type": "edge",
                "source": "X",
                "target": "Z",
                "direction": "->",
                "description": "component",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "Z",
                "target": "X",
                "direction": "<->",
                "description": "component",
            },
        ]
        reg = RegistryLoader.from_records(records)
        state_records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
            {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
            {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
        ]
        state = StateTensor.from_records(reg, state_records)
        engine = DyadicEngine()
        engine._validate_acyclic("M0001", state, reg)


class TestHybridDyadicEngine:
    """Tests for Story 2.3 -- HybridDyadicEngine in dyadic/hybrid.py."""

    @staticmethod
    def _make_three_model_registry():
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
        ]
        from registry.loader import RegistryLoader

        return RegistryLoader.from_records(records)

    @staticmethod
    def _make_three_model_state(registry):
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
            {
                "model_id": "M0003",
                "comp_id": "C0001",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0003",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
        ]
        from state.tensor import StateTensor

        return StateTensor.from_records(registry, records)

    def test_compare_batch_matches_compare_pairs(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.engine import DyadicEngine
        from dyadic.hybrid import HybridDyadicEngine

        base = DyadicEngine()
        hybrid = HybridDyadicEngine(base)

        base_results = base.compare_pairs(state, reg, mode="basic")
        batch_results = hybrid.compare_batch(state, reg, mode="basic")

        assert len(batch_results) == len(base_results)
        assert batch_results == base_results

    def test_compare_chunked_matches_compare_pairs(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.engine import DyadicEngine
        from dyadic.hybrid import HybridDyadicEngine

        base = DyadicEngine()
        hybrid = HybridDyadicEngine(base)

        base_results = base.compare_pairs(state, reg, mode="basic")
        chunked_results = hybrid.compare_chunked(
            state,
            reg,
            chunk_size_mb=1,
            mode="basic",
        )

        assert len(chunked_results) == len(base_results)
        assert chunked_results == base_results

    def test_compare_chunked_respects_chunk_size(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_chunked(
            state,
            reg,
            chunk_size_mb=1,
            mode="basic",
        )

        # 3 models => 3*2 = 6 directed non-self pairs, small chunk_size_mb=1 (~512 pairs min)
        assert len(results) == 6
        for r in results:
            assert r["ego_id"] != r["alter_id"]

    def test_compare_chunked_with_model_ids(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_chunked(
            state,
            reg,
            model_ids=["M0001", "M0002"],
            chunk_size_mb=1,
            mode="basic",
        )

        assert len(results) == 2
        dyad_ids = {r["dyad_id"] for r in results}
        assert dyad_ids == {"M0001__M0002", "M0002__M0001"}

    def test_compare_single_ref_returns_n_minus_one(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_single_ref(
            "M0001",
            state,
            reg,
            mode="basic",
        )

        assert len(results) == 2
        dyad_ids = {r["dyad_id"] for r in results}
        assert dyad_ids == {"M0001__M0002", "M0001__M0003"}

    def test_compare_single_ref_returns_n_minus_one_with_model_ids(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_single_ref(
            "M0001",
            state,
            reg,
            model_ids=["M0001", "M0002", "M0003"],
            mode="basic",
        )

        assert len(results) == 2
        for r in results:
            assert r["ego_id"] == "M0001"

    def test_compare_single_ref_rejects_unknown_reference(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        with pytest.raises(ValueError, match="not found"):
            hybrid.compare_single_ref(
                "M0999",
                state,
                reg,
                mode="basic",
            )

    def test_compare_single_ref_all_models_use_reference(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_single_ref(
            "M0001",
            state,
            reg,
            mode="basic",
        )

        for r in results:
            assert r["ego_id"] == "M0001"
            assert r["alter_id"] != "M0001"

    def test_compare_two_stage_returns_top_k_details(self, monkeypatch):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.causal import CausalWrapper
        from dyadic.hybrid import HybridDyadicEngine

        monkeypatch.setattr(
            CausalWrapper,
            "compute_adjustment_sets",
            lambda self, dag_spec: [["X"]],
        )
        monkeypatch.setattr(
            CausalWrapper,
            "check_identification",
            lambda self, dag_spec: True,
        )

        hybrid = HybridDyadicEngine()
        wrapper = CausalWrapper()

        result = hybrid.compare_two_stage(
            state,
            reg,
            top_k=2,
            causal_wrapper=wrapper,
        )

        assert "heatmap_summary" in result
        assert "detailed_comparisons" in result
        hs = result["heatmap_summary"]
        assert hs["model_count"] == 3
        assert hs["dyad_count"] == 6
        assert hs["top_k"] == 2

        dc = result["detailed_comparisons"]
        assert len(dc) == 2
        for d in dc:
            assert "mas_ego" in d
            assert "identified_compatible" in d

    def test_compare_two_stage_top_k_greater_than_pairs(self, monkeypatch):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.causal import CausalWrapper
        from dyadic.hybrid import HybridDyadicEngine

        monkeypatch.setattr(
            CausalWrapper,
            "compute_adjustment_sets",
            lambda self, dag_spec: [["X"]],
        )
        monkeypatch.setattr(
            CausalWrapper,
            "check_identification",
            lambda self, dag_spec: True,
        )

        hybrid = HybridDyadicEngine()
        wrapper = CausalWrapper()

        result = hybrid.compare_two_stage(
            state,
            reg,
            top_k=100,
            causal_wrapper=wrapper,
        )

        assert result["heatmap_summary"]["top_k"] == 6
        assert len(result["detailed_comparisons"]) == 6

    def test_compare_two_stage_rejects_invalid_top_k(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        with pytest.raises(ValueError, match="top_k must be positive"):
            hybrid.compare_two_stage(state, reg, top_k=0)

        with pytest.raises(ValueError, match="top_k must be positive"):
            hybrid.compare_two_stage(state, reg, top_k=-5)

    def test_compare_two_stage_without_wrapper_returns_basic_top_k(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        result = hybrid.compare_two_stage(state, reg, top_k=2)

        assert result["heatmap_summary"]["top_k"] == 2
        assert len(result["detailed_comparisons"]) == 2
        for d in result["detailed_comparisons"]:
            assert "similarity_rate" in d
            assert "mas_ego" not in d

    def test_compare_batch_default_engine(self):
        reg = self._make_three_model_registry()
        state = self._make_three_model_state(reg)

        from dyadic.hybrid import HybridDyadicEngine

        hybrid = HybridDyadicEngine()
        results = hybrid.compare_batch(state, reg)

        assert len(results) == 6
        for r in results:
            assert r["ego_id"] != r["alter_id"]

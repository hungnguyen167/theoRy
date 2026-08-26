from __future__ import annotations

import threading

import pandas as pd
import pytest
import torch

from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from dyadic.engine import DyadicEngine
from simulation.delta_u import (
    DeltaUEngine,
    DeltaUError,
)
from simulation.scoring import CompatibilityScorer
from simulation.suite import SimulationError, SimulationInputError, SimulationSuite

# ── helpers ──────────────────────────────────────────────────────────────────


def _make_small_world():
    """Build a minimal registry + state for Delta-U testing.

    3 nodes (X, Y, Z) + 1 edge (X->Y), 2 models.
    M0001: all causal
    M0002: X, Y, Z causal, edge unknown
    """
    data = pd.DataFrame(
        [
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
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "Z",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
            },
        ]
    )
    registry = ComponentRegistry(data)

    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0002", "comp_id": "C0004", "status": "unknown"},
    ]
    state = StateTensor.from_records(registry, records)
    engine = DyadicEngine()
    dyads = engine.compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_fully_resolved_world():
    """All models have causal status for all components."""
    data = pd.DataFrame(
        [
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
    )
    registry = ComponentRegistry(data)
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
    ]
    state = StateTensor.from_records(registry, records)
    engine = DyadicEngine()
    dyads = engine.compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_closed_world(fixed_direct=False):
    """Build a resolution-closed multiverse for marginal/global crux testing.

    3 nodes (X, Y, Z) + 2 edges (X->Y, Y->Z). The multiverse contains all
    nine combinations of (unknown, causal, non-causal) per edge on a fixed
    context, so every hypothetical marginal resolution has an exact match.
    """
    data = pd.DataFrame(
        [
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
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "Z",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
                **({"fixed_status": "causal"} if fixed_direct else {}),
            },
            {
                "comp_id": "C0005",
                "type": "edge",
                "source": "Y",
                "target": "Z",
                "direction": "->",
                "description": "Y->Z",
            },
        ]
    )
    registry = ComponentRegistry(data)
    records = []
    model_number = 0
    for e1 in ("unknown", "causal", "non-causal"):
        for e2 in ("unknown", "causal", "non-causal"):
            model_number += 1
            model_id = f"M{model_number:04d}"
            records.extend(
                [
                    {
                        "model_id": model_id,
                        "comp_id": "C0001",
                        "status": "causal",
                        "timing": 1,
                    },
                    {
                        "model_id": model_id,
                        "comp_id": "C0002",
                        "status": "causal",
                        "timing": 2,
                    },
                    {
                        "model_id": model_id,
                        "comp_id": "C0003",
                        "status": "causal",
                        "timing": 3,
                    },
                    {
                        "model_id": model_id,
                        "comp_id": "C0004",
                        "status": "causal" if fixed_direct else e1,
                    },
                    {"model_id": model_id, "comp_id": "C0005", "status": e2},
                ]
            )
    state = StateTensor.from_records(registry, records)
    dyads = DyadicEngine().compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_world_with_mixed_uncertainty():
    """6 components, 3 models with various uncertainty patterns."""
    data = pd.DataFrame(
        [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "A",
                "target": None,
                "direction": None,
                "description": "A",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "B",
                "target": None,
                "direction": None,
                "description": "B",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "C",
                "target": None,
                "direction": None,
                "description": "C",
            },
            {
                "comp_id": "C0004",
                "type": "node",
                "source": "D",
                "target": None,
                "direction": None,
                "description": "D",
            },
            {
                "comp_id": "C0005",
                "type": "edge",
                "source": "A",
                "target": "B",
                "direction": "->",
                "description": "A->B",
            },
            {
                "comp_id": "C0006",
                "type": "edge",
                "source": "B",
                "target": "C",
                "direction": "->",
                "description": "B->C",
            },
        ]
    )
    registry = ComponentRegistry(data)
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0001", "comp_id": "C0004", "status": "causal", "timing": 4},
        {"model_id": "M0001", "comp_id": "C0005", "status": "causal"},
        {"model_id": "M0001", "comp_id": "C0006", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0002", "comp_id": "C0004", "status": "unknown", "timing": 4},
        {"model_id": "M0002", "comp_id": "C0005", "status": "unknown"},
        {"model_id": "M0002", "comp_id": "C0006", "status": "unknown"},
        {"model_id": "M0003", "comp_id": "C0001", "status": "unknown", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0003", "comp_id": "C0004", "status": "causal", "timing": 4},
        {"model_id": "M0003", "comp_id": "C0005", "status": "causal"},
        {"model_id": "M0003", "comp_id": "C0006", "status": "causal"},
    ]
    state = StateTensor.from_records(registry, records)
    engine = DyadicEngine()
    dyads = engine.compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_resolved_edge_world():
    """Return the closed world with C0004 resolved in every model."""
    registry, original_state, _ = _make_closed_world()
    model_ids = [
        model_id
        for model_id in original_state.model_ids
        if original_state.get_status(model_id, "C0004") != "unknown"
    ]
    records = [
        record
        for record in original_state.to_records()
        if record["model_id"] in model_ids
    ]
    state = StateTensor.from_records(registry, records, model_ids=model_ids)
    dyads = DyadicEngine().compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_flexible_timing_world(direction="->"):
    """X2 has timing 1/2; X1=2 and Y=3 for timing-pruning tests."""
    data = pd.DataFrame(
        [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X1",
                "target": None,
                "direction": None,
                "description": "X1",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "X2",
                "target": None,
                "direction": None,
                "description": "X2",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X2",
                "target": "X1",
                "direction": direction,
                "description": "X2->X1",
            },
        ]
    )
    registry = ComponentRegistry(data)
    specs = [
        ("M0001", 1, "unknown"),
        ("M0002", 1, "causal"),
        ("M0003", 1, "non-causal"),
        ("M0004", 2, "unknown"),
        ("M0005", 2, "causal"),
        ("M0006", 2, "non-causal"),
    ]
    records = []
    for model_id, x2_timing, edge_status in specs:
        records.extend(
            [
                {
                    "model_id": model_id,
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 2,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0002",
                    "status": "causal",
                    "timing": x2_timing,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0003",
                    "status": "causal",
                    "timing": 3,
                },
                {"model_id": model_id, "comp_id": "C0004", "status": edge_status},
            ]
        )
    state = StateTensor.from_records(registry, records)
    dyads = DyadicEngine().compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


def _make_missing_timing_world(direction="->"):
    """Build matching missing-timing and valid-timing status groups."""
    data = pd.DataFrame(
        [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X1",
                "target": None,
                "direction": None,
                "description": "X1",
            },
            {
                "comp_id": "C0002",
                "type": "node",
                "source": "X2",
                "target": None,
                "direction": None,
                "description": "X2",
            },
            {
                "comp_id": "C0003",
                "type": "node",
                "source": "Y",
                "target": None,
                "direction": None,
                "description": "Y",
            },
            {
                "comp_id": "C0004",
                "type": "edge",
                "source": "X2",
                "target": "X1",
                "direction": direction,
                "description": "X2->X1",
            },
        ]
    )
    registry = ComponentRegistry(data)
    specs = [
        ("M0001", None, "unknown"),
        ("M0002", None, "causal"),
        ("M0003", None, "non-causal"),
        ("M0004", 1, "unknown"),
        ("M0005", 1, "causal"),
        ("M0006", 1, "non-causal"),
    ]
    records = []
    for model_id, x2_timing, edge_status in specs:
        records.extend(
            [
                {
                    "model_id": model_id,
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 2,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0002",
                    "status": "causal",
                    **({} if x2_timing is None else {"timing": x2_timing}),
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0003",
                    "status": "causal",
                    "timing": 3,
                },
                {"model_id": model_id, "comp_id": "C0004", "status": edge_status},
            ]
        )
    state = StateTensor.from_records(registry, records)
    dyads = DyadicEngine().compare_pairs(state, registry, mode="basic")
    return registry, state, dyads


# ── core marginal crux tests ────────────────────────────────────────────────


def test_delta_u_marginal_preserves_model_and_dyad_count():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0004", state, dyads, reg)
    assert result["component_id"] == "C0004"
    assert result["type"] == "edge"
    assert result["source"] == "X"
    assert result["target"] == "Y"
    assert result["models_changed_causal"] == 3
    assert result["models_changed_non_causal"] == 3
    assert result["mapping_coverage_causal"] == 1.0
    assert result["mapping_coverage_non_causal"] == 1.0
    assert result["delta_u_causal"] == pytest.approx(
        result["post_compatibility_causal"] - result["baseline_compatibility"]
    )
    assert result["crux_mode"] == "marginal"


def test_delta_u_marginal_evaluates_both_directions():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0004", state, dyads, reg)
    assert isinstance(result["delta_u_causal"], float)
    assert isinstance(result["delta_u_non_causal"], float)
    assert result["delta_u"] == pytest.approx(
        max(result["delta_u_causal"], result["delta_u_non_causal"], 0.0)
    )
    assert result["best_resolution"] in ("causal", "non-causal", "none")
    assert result["dyads_improved"] >= 0
    assert result["dyads_worsened"] >= 0


def test_delta_u_already_resolved_returns_none():
    reg, state, _ = _make_closed_world()
    resolved_ids = [
        model_id
        for model_id in state.model_ids
        if state.get_status(model_id, "C0004") != "unknown"
    ]
    records = [
        record for record in state.to_records() if record["model_id"] in resolved_ids
    ]
    state = StateTensor.from_records(reg, records, model_ids=resolved_ids)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0004", state, dyads, reg)
    assert result["best_resolution"] == "none"
    assert result["delta_u_causal"] == 0.0
    assert result["delta_u_non_causal"] == 0.0
    assert result["delta_u"] == 0.0
    assert result["dyads_improved"] == 0
    assert result["dyads_worsened"] == 0


def test_delta_u_unknown_component_raises():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="Unknown component"):
        engine.compute_delta_u("C9999", state, dyads, reg)


def test_delta_u_rejects_node_component():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="not an edge"):
        engine.compute_delta_u("C0001", state, dyads, reg)


def test_delta_u_rejects_missing_structural_score():
    reg, state, dyads = _make_closed_world()
    dyads[0]["similarity_rate"] = None
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="similarity_rate.*unavailable"):
        engine.compute_delta_u("C0004", state, dyads, reg)


def test_delta_u_does_not_mutate_original_state():
    reg, state, dyads = _make_closed_world()
    original = {mid: state.get_status(mid, "C0004") for mid in state.model_ids}
    engine = DeltaUEngine()
    engine.compute_delta_u("C0004", state, dyads, reg)
    engine.compute_global_crux("causal", state, dyads, reg)
    for mid in state.model_ids:
        assert state.get_status(mid, "C0004") == original[mid]


def test_delta_u_remaps_unknown_to_existing_models():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    post_causal = engine.resolve_dyads("C0004", "causal", state, dyads, reg)
    post_non = engine.resolve_dyads("C0004", "non-causal", state, dyads, reg)
    assert len(post_causal) == len(dyads)
    assert len(post_non) == len(dyads)
    by_key = {(d["ego_id"], d["alter_id"]): d for d in dyads}
    for d in post_causal:
        assert "source_ego_id" in d
        assert "source_alter_id" in d
        assert (d["ego_id"], d["alter_id"]) in by_key
    # A model with C0004 unknown maps to its causal variant (itself or an
    # identical state); every model keeps one slot in the post universe.
    assert {d["ego_id"] for d in post_causal} == set(state.model_ids)


def test_delta_u_self_source_dyads_use_self_similarity():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    post = engine.resolve_dyads("C0004", "causal", state, dyads, reg)
    self_rows = [d for d in post if d["source_ego_id"] == d["source_alter_id"]]
    assert self_rows
    for d in self_rows:
        assert d["similarity_rate"] == pytest.approx(1.0)
        assert d["timing_compatible"] is True
        assert d["existence_conflict"] is False
        assert d["conflicting_components"] == []
        assert d["repair_cost"] == 0


def test_delta_u_aligns_reordered_baseline_dyads():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ordered = engine.compute_delta_u("C0004", state, dyads, reg)
    reordered = engine.compute_delta_u("C0004", state, list(reversed(dyads)), reg)

    assert reordered["delta_u"] == ordered["delta_u"]
    assert reordered["dyads_improved"] == ordered["dyads_improved"]
    assert reordered["dyads_worsened"] == ordered["dyads_worsened"]


def test_delta_u_ranking_uses_marginal_semantics():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    rankings = engine.rank_lynchpins(state, dyads, reg, top_k=10)
    ids = {r["component_id"] for r in rankings}
    assert ids == {"C0004", "C0005"}
    for r in rankings:
        assert "delta_u_causal" in r
        assert "delta_u_non_causal" in r
        assert "mapping_coverage_causal" in r
        assert r["mapping_coverage_causal"] == 1.0


def test_delta_u_unmatched_multiverse_raises_coverage_error():
    reg, state, dyads = _make_small_world()
    engine = DeltaUEngine()
    # M0002 has C0004 unknown; its non-causal variant is absent from the
    # multiverse, so a strict resolution must fail.
    with pytest.raises(DeltaUError, match="No existing model matches"):
        engine.resolve_dyads("C0004", "non-causal", state, dyads, reg)
    with pytest.raises(DeltaUError, match="resolution-closed"):
        engine.rank_lynchpins(state, dyads, reg, top_k=10)


def test_delta_u_rounds_to_six_decimals():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0004", state, dyads, reg)
    for key in ("delta_u_causal", "delta_u_non_causal", "delta_u"):
        val_str = repr(result[key])
        if "." in val_str:
            decimal_part = val_str.split(".")[1]
            assert len(decimal_part) <= 6


# ── global crux tests ────────────────────────────────────────────────────────


def test_global_crux_resolves_all_unknowns_jointly():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    result = engine.compute_global_crux("causal", state, dyads, reg)
    assert result["crux_mode"] == "global"
    assert result["target_status"] == "causal"
    assert result["feasible"] is True
    assert result["models_changed"] > 0
    assert result["unknown_instances_forced"] > 0
    assert result["mapping_coverage"] == 1.0
    assert result["model_count"] == len(state.model_ids)
    assert result["dyad_count"] == len(dyads)
    assert result["post_dyads"] is not None


def test_global_crux_non_causal():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    result = engine.compute_global_crux("non-causal", state, dyads, reg)
    assert result["feasible"] is True
    assert result["target_status"] == "non-causal"
    assert result["compatibility_change"] == pytest.approx(
        result["post_compatibility"] - result["baseline_compatibility"]
    )
    assert result["delta_u"] == pytest.approx(max(result["compatibility_change"], 0.0))


def test_global_crux_no_unknowns_is_noop():
    reg, state, dyads = _make_fully_resolved_world()
    engine = DeltaUEngine()
    result = engine.compute_global_crux("causal", state, dyads, reg)
    assert result["feasible"] is True
    assert result["unknown_instances_forced"] == 0
    assert result["models_changed"] == 0
    assert result["compatibility_change"] == 0.0
    assert result["post_compatibility"] == result["baseline_compatibility"]


def test_global_crux_rejects_invalid_status():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="target_status"):
        engine.compute_global_crux("unknown", state, dyads, reg)


def test_global_crux_infeasible_when_unmatched():
    reg, state, dyads = _make_small_world()
    engine = DeltaUEngine()
    # M0002's C0004 non-causal variant is absent -> infeasible.
    with pytest.raises(DeltaUError, match="resolution-closed"):
        engine.compute_global_crux("non-causal", state, dyads, reg)


def test_global_crux_invalid_when_timing_breaks():
    data = pd.DataFrame(
        [
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
        ]
    )
    reg = ComponentRegistry(data)
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0003", "status": "unknown"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0003", "status": "non-causal"},
        {"model_id": "M0003", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal"},
    ]
    state = StateTensor.from_records(reg, records)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="invalid models.*M0001"):
        engine.compute_global_crux("causal", state, dyads, reg)


def test_global_ranking_forces_unknown_and_opposite_but_marginal_only_unknown():
    reg, state, dyads = _make_closed_world()
    global_engine = DeltaUEngine(crux_mode="global")
    global_result = next(
        row
        for row in global_engine.rank_lynchpins(state, dyads, reg, top_k=10)
        if row["component_id"] == "C0004"
    )

    assert global_result["feasible_causal"] is True
    assert global_result["feasible_non_causal"] is True
    # Three models are unknown and three are already non-causal.  A global
    # causal branch changes both groups, unlike marginal resolution.
    assert global_result["instances_forced_causal"] == 6
    assert global_result["models_changed_causal"] == 6
    assert global_result["instances_forced_non_causal"] == 6
    assert global_result["models_changed_non_causal"] == 6

    marginal_result = DeltaUEngine().compute_delta_u("C0004", state, dyads, reg)
    assert marginal_result["instances_forced_causal"] == 3
    assert marginal_result["models_changed_causal"] == 3
    assert marginal_result["instances_forced_non_causal"] == 3
    assert marginal_result["models_changed_non_causal"] == 3


def test_global_ranking_includes_fully_resolved_non_fixed_edge():
    reg, state, dyads = _make_resolved_edge_world()
    global_ids = {
        row["component_id"]
        for row in DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
    }
    marginal_ids = {
        row["component_id"]
        for row in DeltaUEngine().rank_lynchpins(state, dyads, reg, top_k=10)
    }

    assert "C0004" in global_ids
    assert "C0004" not in marginal_ids


def test_global_ranking_excludes_fixed_edges_without_removing_state_components():
    reg, state, dyads = _make_resolved_edge_world()
    original_component_ids = list(state.component_ids)
    original_statuses = {
        (model_id, "C0004"): state.get_status(model_id, "C0004")
        for model_id in state.model_ids
    }
    fixed_data = reg.data.copy()
    fixed_data.loc[fixed_data["comp_id"] == "C0004", "fixed_status"] = "causal"
    fixed_reg = ComponentRegistry(fixed_data)

    ranking = DeltaUEngine(crux_mode="global").rank_lynchpins(
        state, dyads, fixed_reg, top_k=10
    )

    assert "C0004" not in {row["component_id"] for row in ranking}
    assert list(state.component_ids) == original_component_ids
    assert len(fixed_reg.data) == len(reg.data)
    assert {
        (model_id, "C0004"): state.get_status(model_id, "C0004")
        for model_id in state.model_ids
    } == original_statuses


def test_global_ranking_validates_duplicate_dyads_without_candidates():
    reg, state, dyads = _make_closed_world()
    fixed_data = reg.data.copy()
    fixed_data.loc[fixed_data["type"] == "edge", "fixed_status"] = "causal"
    fixed_reg = ComponentRegistry(fixed_data)
    malformed = [dict(dyads[0]), dict(dyads[0]), *map(dict, dyads[2:])]

    with pytest.raises(DeltaUError, match="duplicate directed model pairs"):
        DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, malformed, fixed_reg, top_k=10
        )


def test_global_ranking_returns_empty_for_valid_no_candidate_input():
    reg, state, dyads = _make_closed_world()
    fixed_data = reg.data.copy()
    fixed_data.loc[fixed_data["type"] == "edge", "fixed_status"] = "causal"
    fixed_reg = ComponentRegistry(fixed_data)

    assert (
        DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, fixed_reg, top_k=10
        )
        == []
    )


def test_global_ranking_evaluates_both_directions_and_applies_top_k():
    reg, state, dyads = _make_closed_world()
    ranking = DeltaUEngine().rank_lynchpins(
        state,
        dyads,
        reg,
        top_k=1,
        crux_mode="global",
    )

    assert len(ranking) == 1
    row = ranking[0]
    assert row["rank"] == 1
    assert row["crux_mode"] == "global"
    assert isinstance(row["delta_u_causal"], float)
    assert isinstance(row["delta_u_non_causal"], float)
    assert row["delta_u"] == pytest.approx(
        max(row["delta_u_causal"], row["delta_u_non_causal"], 0.0)
    )


def test_global_ranking_preserves_counts_and_original_state():
    reg, state, dyads = _make_closed_world()
    original_tensor = state.tensor.clone()
    original_node_mask = state.node_present_mask.clone()
    original_edge_mask = state.edge_applicable_mask.clone()
    original_timing = dict(state.timing)
    model_count = len(state.model_ids)
    dyad_count = len(dyads)

    ranking = DeltaUEngine(crux_mode="global").rank_lynchpins(
        state, dyads, reg, top_k=10
    )

    assert ranking
    for row in ranking:
        assert row["model_count"] == model_count
        assert row["dyad_count"] == dyad_count
        assert row["post_model_count_causal"] == model_count
        assert row["post_model_count_non_causal"] == model_count
        assert row["post_dyad_count_causal"] == dyad_count
        assert row["post_dyad_count_non_causal"] == dyad_count
        assert row["post_dyad_length_causal"] == dyad_count
        assert row["post_dyad_length_non_causal"] == dyad_count

    assert torch.equal(state.tensor, original_tensor)
    assert torch.equal(state.node_present_mask, original_node_mask)
    assert torch.equal(state.edge_applicable_mask, original_edge_mask)
    assert state.timing == original_timing


def test_global_ranking_prunes_timing_invalid_direction():
    data = pd.DataFrame(
        [
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
        ]
    )
    reg = ComponentRegistry(data)
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0003", "status": "unknown"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0003", "status": "non-causal"},
        {"model_id": "M0003", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal"},
        {"model_id": "M0004", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0004", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0004", "comp_id": "C0003", "status": "non-causal"},
    ]
    state = StateTensor.from_records(reg, records)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")

    ranking = DeltaUEngine(crux_mode="global").rank_lynchpins(
        state, dyads, reg, top_k=10
    )
    row = ranking[0]
    assert row["feasible_causal"] is True
    assert row["invalid_models_causal"] == []
    assert row["timing_pruned_models_causal"] == ["M0001", "M0002"]
    assert row["post_model_count_causal"] == 2
    assert row["post_dyad_count_causal"] == 2
    assert row["feasible_non_causal"] is True
    assert row["delta_u_non_causal"] is not None


def test_global_ranking_prunes_already_target_timing_invalid_models():
    data = pd.DataFrame(
        [
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
        ]
    )
    reg = ComponentRegistry(data)
    records = []
    model_specs = {
        "M0001": (2, 1, "causal"),
        "M0002": (1, 2, "non-causal"),
        "M0003": (1, 2, "causal"),
        "M0004": (1, 2, "non-causal"),
        "M0005": (2, 1, "non-causal"),
    }
    for model_id, (x_timing, y_timing, edge_status) in model_specs.items():
        records.extend(
            [
                {
                    "model_id": model_id,
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": x_timing,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0002",
                    "status": "causal",
                    "timing": y_timing,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0003",
                    "status": edge_status,
                },
            ]
        )
    state = StateTensor.from_records(reg, records)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")

    row = DeltaUEngine(crux_mode="global").rank_lynchpins(state, dyads, reg, top_k=10)[
        0
    ]

    assert row["feasible_causal"] is True
    assert row["invalid_models_causal"] == []
    assert row["timing_pruned_models_causal"] == ["M0001", "M0005"]
    assert row["instances_forced_causal"] == 3
    assert row["models_changed_causal"] == 2
    assert row["post_model_count_causal"] == 3
    assert row["post_dyad_count_causal"] == 6
    assert row["feasible_non_causal"] is True
    assert row["instances_forced_non_causal"] == 2
    assert row["models_changed_non_causal"] == 2


# ── marginal causality / timing constraints ─────────────────────────────────


def test_delta_u_causal_branch_prunes_timing_invalid_unknown_model():
    data = pd.DataFrame(
        [
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
        ]
    )
    reg = ComponentRegistry(data)
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0003", "status": "unknown"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0003", "status": "non-causal"},
    ]
    state = StateTensor.from_records(reg, records)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0003", state, dyads, reg)
    assert result["timing_pruned_models_causal"] == ["M0001"]
    assert result["models_pruned_causal"] == 1
    assert result["insufficient_post_models_causal"] is True
    assert result["feasible_causal"] is False
    assert result["delta_u_causal"] is None
    assert result["feasible_non_causal"] is True


def test_global_flexible_timing_prunes_same_time_slots_only_for_causal():
    reg, state, dyads = _make_flexible_timing_world()
    row = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )

    assert row["timing_pruned_models_causal"] == ["M0004", "M0005", "M0006"]
    assert row["timing_pruned_models_non_causal"] == []
    assert row["models_pruned_causal"] == 3
    assert row["models_pruned_non_causal"] == 0
    assert row["post_model_count_causal"] == 3
    assert row["post_dyad_count_causal"] == 3 * 2
    assert row["post_model_count_non_causal"] == 6
    assert row["post_dyad_count_non_causal"] == 6 * 5
    assert row["mapping_coverage_causal"] == 1.0
    assert row["mapping_coverage_non_causal"] == 1.0
    assert "post_model_count" not in row
    assert "post_dyad_count" not in row
    assert row["delta_u_causal"] == pytest.approx(
        row["post_compatibility_causal"] - row["baseline_compatibility"]
    )


def test_marginal_timing_prunes_only_unknown_slots():
    reg, state, dyads = _make_flexible_timing_world()
    engine = DeltaUEngine()
    result = engine.compute_delta_u("C0004", state, dyads, reg)

    assert result["timing_pruned_models_causal"] == ["M0004"]
    assert result["models_pruned_causal"] == 1
    assert result["timing_pruned_models_non_causal"] == []
    assert result["post_model_count_causal"] == 5
    assert result["post_dyad_count_causal"] == 5 * 4
    assert result["post_model_count_non_causal"] == 6
    assert result["post_dyad_count_non_causal"] == 6 * 5
    assert result["mapping_coverage_causal"] == 1.0
    assert result["mapping_coverage_non_causal"] == 1.0
    # M0005 is already causal and malformed, but marginal semantics retain it.
    assert result["models_changed_causal"] == 1
    assert result["instances_forced_causal"] == 2
    reduced = engine.resolve_dyads("C0004", "causal", state, dyads, reg)
    assert len(reduced) == 5 * 4
    assert "M0004" not in {
        model_id for dyad in reduced for model_id in (dyad["ego_id"], dyad["alter_id"])
    }


def test_timing_pruning_counts_and_state_registry_are_branch_local():
    reg, state, dyads = _make_flexible_timing_world()
    original_tensor = state.tensor.clone()
    original_node_mask = state.node_present_mask.clone()
    original_edge_mask = state.edge_applicable_mask.clone()
    original_timing = dict(state.timing)
    original_component_ids = list(state.component_ids)
    original_registry = reg.data.copy(deep=True)

    marginal = DeltaUEngine().compute_delta_u("C0004", state, dyads, reg)
    global_row = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )

    for row in (marginal, global_row):
        for direction in ("causal", "non_causal"):
            retained = row[f"post_model_count_{direction}"]
            assert row[f"post_dyad_count_{direction}"] == retained * (retained - 1)
    assert list(state.component_ids) == original_component_ids
    assert torch.equal(state.tensor, original_tensor)
    assert torch.equal(state.node_present_mask, original_node_mask)
    assert torch.equal(state.edge_applicable_mask, original_edge_mask)
    assert state.timing == original_timing
    pd.testing.assert_frame_equal(reg.data, original_registry)


def test_missing_timing_prunes_directed_causal_but_not_bidirected():
    directed_reg, directed_state, directed_dyads = _make_missing_timing_world("->")
    directed = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            directed_state, directed_dyads, directed_reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )
    assert directed["timing_pruned_models_causal"] == ["M0001", "M0002", "M0003"]
    assert directed["post_model_count_causal"] == 3

    bidirected_reg, bidirected_state, bidirected_dyads = _make_missing_timing_world(
        "<->"
    )
    bidirected = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            bidirected_state, bidirected_dyads, bidirected_reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )
    assert bidirected["timing_pruned_models_causal"] == []
    assert bidirected["models_pruned_causal"] == 0
    assert bidirected["post_model_count_causal"] == 6


def test_cycle_invalidity_is_not_timing_pruning():
    reg = ComponentRegistry(
        pd.DataFrame(
            [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "A",
                    "target": None,
                    "direction": None,
                    "description": "A",
                },
                {
                    "comp_id": "C0002",
                    "type": "node",
                    "source": "B",
                    "target": None,
                    "direction": None,
                    "description": "B",
                },
                {
                    "comp_id": "C0003",
                    "type": "node",
                    "source": "C",
                    "target": None,
                    "direction": None,
                    "description": "C",
                },
                {
                    "comp_id": "C0004",
                    "type": "edge",
                    "source": "A",
                    "target": "B",
                    "direction": "->",
                    "description": "A->B",
                },
                {
                    "comp_id": "C0005",
                    "type": "edge",
                    "source": "B",
                    "target": "C",
                    "direction": "->",
                    "fixed_status": "causal",
                    "description": "B->C",
                },
                {
                    "comp_id": "C0006",
                    "type": "edge",
                    "source": "C",
                    "target": "A",
                    "direction": "->",
                    "fixed_status": "causal",
                    "description": "C->A",
                },
            ]
        )
    )
    records = []
    for model_id, target_status in (("M0001", "unknown"), ("M0002", "non-causal")):
        records.extend(
            [
                {
                    "model_id": model_id,
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 1,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0002",
                    "status": "causal",
                    "timing": 2,
                },
                {"model_id": model_id, "comp_id": "C0003", "status": "causal"},
                {"model_id": model_id, "comp_id": "C0004", "status": target_status},
                {"model_id": model_id, "comp_id": "C0005", "status": "causal"},
                {"model_id": model_id, "comp_id": "C0006", "status": "causal"},
            ]
        )
    state = StateTensor.from_records(reg, records)
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
    row = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )
    assert row["timing_pruned_models_causal"] == []
    assert row["invalid_models_causal"] == ["M0001", "M0002"]
    assert row["feasible_causal"] is False
    assert row["feasible_non_causal"] is True


def test_global_branch_raises_unmatched_even_with_other_invalid_models():
    from state.completions import CompletionIndex

    registry = ComponentRegistry(
        pd.DataFrame(
            [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "A",
                    "target": None,
                    "direction": None,
                    "description": "A",
                },
                {
                    "comp_id": "C0002",
                    "type": "node",
                    "source": "B",
                    "target": None,
                    "direction": None,
                    "description": "B",
                },
                {
                    "comp_id": "C0003",
                    "type": "node",
                    "source": "C",
                    "target": None,
                    "direction": None,
                    "description": "C",
                },
                {
                    "comp_id": "C0004",
                    "type": "node",
                    "source": "D",
                    "target": None,
                    "direction": None,
                    "description": "D",
                },
                {
                    "comp_id": "C0005",
                    "type": "edge",
                    "source": "A",
                    "target": "B",
                    "direction": "->",
                    "description": "A->B",
                },
                {
                    "comp_id": "C0006",
                    "type": "edge",
                    "source": "B",
                    "target": "C",
                    "direction": "->",
                    "description": "B->C",
                },
                {
                    "comp_id": "C0007",
                    "type": "edge",
                    "source": "C",
                    "target": "A",
                    "direction": "->",
                    "description": "C->A",
                },
            ]
        )
    )
    records = []
    model_specs = {
        "M0001": (False, "unknown", "causal", "causal"),
        "M0002": (False, "non-causal", "causal", "causal"),
        "M0003": (True, "non-causal", "non-causal", "causal"),
    }
    for model_id, (
        has_d,
        target_status,
        edge_2_status,
        edge_3_status,
    ) in model_specs.items():
        records.extend(
            [
                {
                    "model_id": model_id,
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 1,
                },
                {
                    "model_id": model_id,
                    "comp_id": "C0002",
                    "status": "causal",
                    "timing": 2,
                },
                {"model_id": model_id, "comp_id": "C0003", "status": "causal"},
                {"model_id": model_id, "comp_id": "C0005", "status": target_status},
                {"model_id": model_id, "comp_id": "C0006", "status": edge_2_status},
                {"model_id": model_id, "comp_id": "C0007", "status": edge_3_status},
            ]
        )
        if has_d:
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": "C0004",
                    "status": "causal",
                    "timing": 4,
                }
            )

    state = StateTensor.from_records(registry, records)
    dyads = DyadicEngine().compare_pairs(state, registry, mode="basic")
    engine = DeltaUEngine(crux_mode="global")
    analysis_ids = engine._analysis_model_ids(state)
    index = CompletionIndex(state, registry)
    baseline_scores = engine._scorer.score_dyads(dyads)
    baseline_compatibility = float(baseline_scores.mean().item())

    with pytest.raises(DeltaUError, match="unmatched models.*M0003"):
        engine._global_branch(
            "C0005",
            "causal",
            state,
            dyads,
            registry,
            analysis_ids,
            index,
            baseline_scores,
            baseline_compatibility,
        )


def test_fewer_than_two_retained_models_is_cleanly_unavailable():
    reg, state, dyads = _make_flexible_timing_world()
    selected = {"M0003", "M0004", "M0006"}
    records = [
        record for record in state.to_records() if record["model_id"] in selected
    ]
    state = StateTensor.from_records(reg, records, model_ids=sorted(selected))
    dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
    row = next(
        result
        for result in DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
        if result["component_id"] == "C0004"
    )
    assert row["insufficient_post_models_causal"] is True
    assert row["feasible_causal"] is False
    assert row["delta_u_causal"] is None
    assert row["post_compatibility_causal"] is None
    assert row["post_model_count_causal"] == 1
    assert row["post_dyad_count_causal"] == 0
    assert row["feasible_non_causal"] is True
    assert row["delta_u_non_causal"] is not None


def test_simulation_suite_uses_underscore_pruning_key_for_non_causal(monkeypatch):
    registry, state, dyads = _make_flexible_timing_world()
    suite = SimulationSuite()
    fake_ranking = {
        "component_id": "C0004",
        "best_resolution": "non-causal",
        "models_changed_non_causal": 2,
        "mapping_coverage_non_causal": 1.0,
        "timing_pruned_models_non_causal": [],
        # A hyphenated key must not be consumed by the suite.
        "timing_pruned_models_non-causal": ["M0001"],
    }
    monkeypatch.setattr(
        DeltaUEngine,
        "rank_lynchpins",
        lambda self, *args, **kwargs: [fake_ranking],
    )

    result, _artifacts = suite._analyze_lynchpin_result(
        registry,
        state,
        dyads,
        state.to_records(),
        list(state.model_ids),
        seed=42,
        metric_diagnostics={
            "compatibility_rate": suite._compute_metric_rate(dyads, "similarity_rate")
        },
        crux_mode="marginal",
    )

    assert result["models_retained"] == len(state.model_ids)


# ── ranking tests ────────────────────────────────────────────────────────────


def test_rank_lynchpins_returns_top_k():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(state, dyads, reg, top_k=3)
    assert 1 <= len(ranking) <= 3
    for entry in ranking:
        assert "rank" in entry
        assert "component_id" in entry
        assert "delta_u" in entry
        assert "type" in entry
        assert "source" in entry
        assert "target" in entry
        assert "best_resolution" in entry
        assert entry["delta_u"] >= 0


def test_rank_lynchpins_filters_resolved_components():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(state, dyads, reg, top_k=10)
    # Only edge components with unknown instances are candidates
    uncertain_ids = {"C0004", "C0005"}
    for entry in ranking:
        assert entry["component_id"] in uncertain_ids
    # Verify node components never appear
    ranked_ids = {r["component_id"] for r in ranking}
    assert "C0001" not in ranked_ids
    assert "C0002" not in ranked_ids
    assert "C0003" not in ranked_ids


def test_rank_lynchpins_sorts_by_delta_desc():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(state, dyads, reg, top_k=10)
    deltas = [r["delta_u"] for r in ranking]
    assert deltas == sorted(deltas, reverse=True)


def test_rank_lynchpins_empty_when_no_uncertainty():
    reg, state, dyads = _make_fully_resolved_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(state, dyads, reg, top_k=10)
    assert ranking == []


def test_rank_lynchpins_two_stage_threshold():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(
        state,
        dyads,
        reg,
        top_k=10,
        mode="two-stage",
        heatmap_threshold=0.0,
    )
    assert len(ranking) >= 1


def test_rank_lynchpins_two_stage_applies_threshold():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    ranking = engine.rank_lynchpins(
        state,
        dyads,
        reg,
        top_k=10,
        mode="two-stage",
        heatmap_threshold=1.0,
    )
    assert ranking == []


def test_rank_lynchpins_rejects_invalid_top_k():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="top_k must be positive"):
        engine.rank_lynchpins(state, dyads, reg, top_k=0)
    with pytest.raises(DeltaUError, match="top_k must be positive"):
        engine.rank_lynchpins(state, dyads, reg, top_k=-1)


def test_rank_lynchpins_two_stage_rejects_invalid_threshold():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="heatmap_threshold"):
        engine.rank_lynchpins(
            state,
            dyads,
            reg,
            top_k=5,
            mode="two-stage",
            heatmap_threshold=1.5,
        )
    with pytest.raises(DeltaUError, match="heatmap_threshold"):
        engine.rank_lynchpins(
            state,
            dyads,
            reg,
            top_k=5,
            mode="two-stage",
            heatmap_threshold=-0.1,
        )


# ── synergistic sets tests ───────────────────────────────────────────────────


def test_synergistic_sets_pair_discovery():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
    )
    assert len(sets) >= 1
    for entry in sets:
        assert "components" in entry
        assert len(entry["components"]) == 2
        assert isinstance(entry["delta_u_combined"], float)
        assert isinstance(entry["synergy_score"], float)
        assert entry["label"] in ("additive", "super-additive")


def test_synergistic_sets_triple_discovery():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=3,
        top_n=5,
        search_strategy="greedy",
    )
    for entry in sets:
        assert len(entry["components"]) == 3


def test_synergistic_sets_positive_synergy_label():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
    )
    for entry in sets:
        assert entry["label"] in ("additive", "super-additive")
        assert isinstance(entry["synergy_score"], float)


def test_synergistic_sets_additive_label():
    reg, state, dyads = _make_small_world()
    engine = DeltaUEngine()
    # Only 1 uncertain component -> set_size=2 requires at least 2
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
    )
    # Since there's only 1 uncertain component, no pairs can be formed
    assert sets == []


def test_synergistic_sets_greedy_and_beam_match_small_case():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    greedy = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
        search_strategy="greedy",
    )
    beam = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
        search_strategy="beam",
        beam_width=5,
    )
    assert len(greedy) > 0
    assert len(beam) > 0
    # With small candidate pool, greedy and beam top sets should overlap
    greedy_comps = {tuple(s["components"]) for s in greedy}
    beam_comps = {tuple(s["components"]) for s in beam}
    assert greedy_comps & beam_comps


def test_synergistic_sets_empty_when_no_uncertainty():
    reg, state, dyads = _make_fully_resolved_world()
    engine = DeltaUEngine()
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
    )
    assert sets == []


def test_synergistic_sets_validates_arguments():
    reg, state, dyads = _make_closed_world()
    engine = DeltaUEngine()
    with pytest.raises(DeltaUError, match="set_size"):
        engine.compute_synergistic_sets(state, dyads, reg, set_size=1)
    with pytest.raises(DeltaUError, match="top_n"):
        engine.compute_synergistic_sets(state, dyads, reg, top_n=0)
    with pytest.raises(DeltaUError, match="beam_width"):
        engine.compute_synergistic_sets(state, dyads, reg, beam_width=0)
    with pytest.raises(DeltaUError, match="search_strategy"):
        engine.compute_synergistic_sets(
            state,
            dyads,
            reg,
            search_strategy="invalid",
        )


# ── scoring tests ─────────────────────────────────────────────────────────────


def test_delta_u_similarity_metric_matches_default_results():
    reg, state, dyads = _make_closed_world()
    engine_old = DeltaUEngine()
    engine_new = DeltaUEngine(compatibility_metric="similarity_rate")
    old_result = engine_old.compute_delta_u("C0004", state, dyads, reg)
    new_result = engine_new.compute_delta_u("C0004", state, dyads, reg)
    assert new_result["delta_u"] == pytest.approx(old_result["delta_u"])
    assert new_result["best_resolution"] == old_result["best_resolution"]


def _mock_causal_backends(monkeypatch):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )
    monkeypatch.setattr(
        "dyadic.identification.IdentificationWrapper.identify_total_effect",
        lambda self, **kwargs: (True, "P(Y|X)"),
    )


def _with_completion_support(registry, state):
    from state.completions import materialize_missing_completions

    support = materialize_missing_completions(state, registry, state.model_ids)
    return StateTensor.from_records(registry, state.to_records() + support)


def test_delta_u_uses_identified_compatible(monkeypatch):
    from dyadic.causal import CausalWrapper
    from dyadic.identification import IdentificationWrapper

    _mock_causal_backends(monkeypatch)
    reg, state, dyads = _make_closed_world(fixed_direct=True)
    for d in dyads:
        d["identified_compatible"] = True
        d["identified_ego"] = True
        d["identification_nodes_ego"] = ["X", "Y", "Z"]
    dyads[0]["identified_compatible"] = False

    engine = DeltaUEngine(
        causal_wrapper=CausalWrapper(),
        compatibility_metric="identified_compatible",
        exposure="X",
        outcome="Y",
        identification_wrapper=IdentificationWrapper(),
    )
    result = engine.compute_delta_u("C0005", state, dyads, reg)
    assert result["component_id"] == "C0005"


def test_identified_self_dyads_accept_r_serialized_false():
    reg, state, dyads = _make_closed_world()
    for dyad in dyads:
        dyad["identified_compatible"] = False
        dyad["identified_ego"] = "FALSE"
        dyad["identification_nodes_ego"] = None

    engine = DeltaUEngine(compatibility_metric="identified_compatible")
    post = engine.resolve_dyads("C0004", "causal", state, dyads, reg)
    self_rows = [
        dyad for dyad in post if dyad["source_ego_id"] == dyad["source_alter_id"]
    ]

    assert self_rows
    assert all(dyad["identified_compatible"] is False for dyad in self_rows)


def test_delta_u_uses_mas_compatible(monkeypatch):
    from dyadic.causal import CausalWrapper

    _mock_causal_backends(monkeypatch)
    reg, state, dyads = _make_closed_world(fixed_direct=True)
    for d in dyads:
        d["mas_compatible"] = True
        d["mas_ego"] = [["Z"]]
    engine = DeltaUEngine(
        causal_wrapper=CausalWrapper(),
        compatibility_metric="mas_compatible",
        exposure="X",
        outcome="Y",
    )
    result = engine.compute_delta_u("C0005", state, dyads, reg)
    assert result["component_id"] == "C0005"


def test_rank_lynchpins_two_stage_causal_reranks_candidates(monkeypatch):
    from dyadic.causal import CausalWrapper

    _mock_causal_backends(monkeypatch)
    reg, state, dyads = _make_closed_world(fixed_direct=True)
    analysis_model_ids = list(state.model_ids)
    for d in dyads:
        d["mas_compatible"] = True
        d["mas_ego"] = [["Z"]]

    engine = DeltaUEngine(
        causal_wrapper=CausalWrapper(),
        compatibility_metric="mas_compatible",
        exposure="X",
        outcome="Y",
        model_ids=analysis_model_ids,
    )
    ranking = engine.rank_lynchpins(
        state,
        dyads,
        reg,
        top_k=10,
        mode="two-stage",
        heatmap_threshold=0.0,
    )
    assert len(ranking) >= 1
    deltas = [r["delta_u"] for r in ranking]
    assert deltas == sorted(deltas, reverse=True)


def test_synergistic_sets_use_selected_compatibility_metric(monkeypatch):
    from dyadic.causal import CausalWrapper

    _mock_causal_backends(monkeypatch)
    reg, state, dyads = _make_closed_world(fixed_direct=True)
    analysis_model_ids = list(state.model_ids)
    for d in dyads:
        d["mas_compatible"] = True
        d["mas_ego"] = [["Z"]]

    engine = DeltaUEngine(
        causal_wrapper=CausalWrapper(),
        compatibility_metric="mas_compatible",
        exposure="X",
        outcome="Y",
        model_ids=analysis_model_ids,
    )
    sets = engine.compute_synergistic_sets(
        state,
        dyads,
        reg,
        set_size=2,
        top_n=10,
    )
    # The only edge other than the fixed direct query edge is resolved in
    # this compact fixture, so no size-two synergistic set is eligible.
    assert sets == []


def test_delta_u_cuda_requested_unavailable_errors():
    from dyadic.tensor_engine import resolve_device

    if torch.cuda.is_available():
        pytest.skip("CUDA is available")
    else:
        with pytest.raises(ValueError, match="CUDA requested but not available"):
            resolve_device("cuda")


# ── scoring unit tests ────────────────────────────────────────────────────────


def test_similarity_rate_scorer_uses_similarity_rate():
    dyads = [
        {"dyad_id": "A__B", "similarity_rate": 0.8},
        {"dyad_id": "B__A", "similarity_rate": 0.3},
    ]
    scorer = CompatibilityScorer(compatibility_metric="similarity_rate")
    scores = scorer.score_dyads(dyads)
    assert torch.allclose(scores, torch.tensor([0.8, 0.3]))
    assert not scorer.requires_causal()


def test_compatibility_scorer_similarity_rate():
    scorer = CompatibilityScorer(compatibility_metric="similarity_rate")
    assert not scorer.requires_causal()
    dyads = [{"dyad_id": "A__B", "similarity_rate": 0.8}]
    scores = scorer.score_dyads(dyads)
    assert scores[0] == 0.8


def test_compatibility_scorer_mas_compatible():
    scorer = CompatibilityScorer(compatibility_metric="mas_compatible")
    assert scorer.requires_causal()
    dyads = [
        {"dyad_id": "A__B", "mas_compatible": True},
        {"dyad_id": "B__A", "mas_compatible": False},
    ]
    scores = scorer.score_dyads(dyads)
    assert scores[0] == 1.0
    assert scores[1] == 0.0


def test_compatibility_scorer_identified_compatible():
    scorer = CompatibilityScorer(compatibility_metric="identified_compatible")
    assert scorer.requires_causal()
    dyads = [{"dyad_id": "A__B", "identified_compatible": True}]
    scores = scorer.score_dyads(dyads)
    assert scores[0] == 1.0


def test_invalid_compatibility_metric_rejected():
    with pytest.raises(ValueError, match="compatibility_metric"):
        CompatibilityScorer(compatibility_metric="invalid")


# ── Epic 5: Simulation Suite Tests ────────────────────────────────────────────


def _make_suite(random_state=42):
    return SimulationSuite(random_state=random_state)


# ── Scenario A: Consensus Illusion ─────────────────────────────────────────────


_NM = 20
_NC = 50


@pytest.mark.parametrize(
    "scenario",
    ["consensus_illusion", "lynchpin_of_certainty", "ghost_discovery"],
)
def test_scenarios_reject_bidirected_generation(scenario):
    with pytest.raises(SimulationInputError, match="directed components only"):
        _make_suite().run_scenario(
            scenario,
            n_models=20,
            n_components=50,
            include_bidirectional=True,
            enforce_thresholds=False,
        )


@pytest.mark.parametrize(
    "scenario",
    ["consensus_illusion", "crux_of_certainty", "ghost_discovery"],
)
@pytest.mark.parametrize(
    "compatibility_metric",
    ["similarity_rate", "mas_compatible", "identified_compatible"],
)
def test_each_scenario_accepts_each_compatibility_metric(
    scenario, compatibility_metric, monkeypatch
):
    _mock_causal_backends(monkeypatch)
    kwargs = {
        "n_models": 10,
        "n_components": 10,
        "compatibility_metric": compatibility_metric,
        "enforce_thresholds": False,
    }
    if scenario == "consensus_illusion" and compatibility_metric == "similarity_rate":
        with pytest.raises(SimulationInputError, match="mas_compatible"):
            _make_suite(42).run_scenario(scenario, **kwargs)
        return
    if compatibility_metric != "similarity_rate" and scenario != "consensus_illusion":
        exposure, outcome = "X1", "X2"
        if scenario == "crux_of_certainty" and compatibility_metric == "mas_compatible":
            exposure, outcome = "X2", "X3"
        kwargs.update(exposure=exposure, outcome=outcome)

    result = _make_suite(42).run_scenario(scenario, **kwargs)
    metrics = result["results"]

    assert result["scenario"] == scenario
    assert metrics["compatibility_metric"] == compatibility_metric
    expected_dyads = (
        192 * 191
        if scenario == "consensus_illusion"
        else result["n_models"] * (result["n_models"] - 1)
    )
    assert metrics["n_dyads"] == expected_dyads
    assert metrics["n_comparable_dyads"] == metrics["n_dyads"]
    assert metrics["n_unavailable_dyads"] == 0


# ── Scenario B: Lynchpin of Certainty ─────────────────────────────────────────


def test_scenario_lynchpin_generates_fragmented_multiverse():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    assert "results" in result
    assert "artifacts" in result
    assert result["results"]["compatibility_metric"] == "similarity_rate"
    assert result["n_models"] == _NM


def test_scenario_lynchpin_baseline_is_measured_rate():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    baseline = result["results"]["baseline_compatibility"]
    assert 0.0 <= baseline <= 1.0


def test_scenario_lynchpin_post_resolution_exceeds_baseline():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    results = result["results"]
    assert results["post_resolution_compatibility"] > results["baseline_compatibility"]


def test_scenario_lynchpin_phase_transition_is_measured_difference():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    results = result["results"]
    assert results["phase_transition_score"] > 0
    assert results["phase_transition_score"] == pytest.approx(
        results["post_resolution_compatibility"] - results["baseline_compatibility"],
        abs=1e-6,
    )


def test_scenario_lynchpin_ranked_first_by_delta_u():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    assert result["results"]["lynchpin_rank"] == 1
    rankings = result["artifacts"]["rankings"]
    assert rankings
    assert rankings[0]["component_id"] == result["results"]["lynchpin_component_id"]


def test_scenario_lynchpin_resolves_only_unknown_instances():
    reg, state, dyads = _make_closed_world()

    engine = DeltaUEngine()
    post = engine.resolve_dyads("C0004", "causal", state, dyads, reg)
    assert len(post) == len(dyads)
    by_key = {(d["ego_id"], d["alter_id"]): d for d in post}
    assert set(by_key) == {(d["ego_id"], d["alter_id"]) for d in dyads}
    for d in post:
        assert "source_ego_id" in d


def test_causal_lynchpin_ranking_stays_on_calling_thread(monkeypatch):
    engine = DeltaUEngine(compatibility_metric="mas_compatible")
    calling_thread = threading.get_ident()
    observed_threads = []

    def compute(component_id, *_args):
        observed_threads.append(threading.get_ident())
        return {"component_id": component_id}

    monkeypatch.setattr(engine, "compute_delta_u", compute)

    rankings = engine._stage1_all(
        ["C0001", "C0002"], state=None, dyads=[], registry=None
    )

    assert [row["component_id"] for row in rankings] == ["C0001", "C0002"]
    assert observed_threads == [calling_thread, calling_thread]


def test_parallel_lynchpin_ranking_propagates_component_failures(monkeypatch):
    engine = DeltaUEngine()

    def compute(component_id, *_args):
        if component_id == "C0002":
            raise DeltaUError("component failed")
        return {"component_id": component_id}

    monkeypatch.setattr(engine, "compute_delta_u", compute)

    with pytest.raises(DeltaUError, match="component failed"):
        engine._stage1_all(["C0001", "C0002"], state=None, dyads=[], registry=None)


def test_scenario_lynchpin_compatibility_timeline_has_two_steps():
    suite = _make_suite()
    result = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    assert len(result["results"]["compatibility_timeline"]) == 2
    assert result["results"]["compatibility_timeline"][0]["step"] == "baseline"
    assert result["results"]["compatibility_timeline"][1]["step"].startswith(
        "resolved_"
    )


def test_scenario_lynchpin_reproducible_with_seed():
    suite1 = _make_suite(42)
    suite2 = _make_suite(42)
    r1 = suite1.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    r2 = suite2.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    assert (
        r1["results"]["baseline_compatibility"]
        == r2["results"]["baseline_compatibility"]
    )
    assert (
        r1["results"]["phase_transition_score"]
        == r2["results"]["phase_transition_score"]
    )


# ── Scenario C: Ghost Discovery ───────────────────────────────────────────────


def test_scenario_ghost_discovers_ghost_cluster():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    assert result["results"]["ghost_cluster_found"] is True


def test_scenario_ghost_internal_compatibility_above_070():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    ghosts = result["results"]["ghost_clusters"]
    assert len(ghosts) > 0
    assert ghosts[0]["internal_compatibility"] > 0.70


def test_scenario_ghost_prior_compatibility_below_030():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    ghosts = result["results"]["ghost_clusters"]
    assert len(ghosts) > 0
    assert ghosts[0]["prior_compatibility"] < 0.30


def test_scenario_ghost_at_least_two_clusters():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    assert result["results"]["clusters_detected"] >= 2


def test_scenario_ghost_noise_models_present():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    assert result["results"]["noise_count"] > 0


def test_scenario_ghost_embedding_has_coordinates():
    suite = _make_suite()
    result = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    embedding = result["artifacts"]["embedding_2d"]
    assert "model_ids" in embedding
    assert "x" in embedding
    assert "y" in embedding
    assert len(embedding["model_ids"]) > 0


def test_scenario_ghost_reproducible_with_seed():
    suite1 = _make_suite(42)
    suite2 = _make_suite(42)
    r1 = suite1.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    r2 = suite2.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    assert r1["results"]["ghost_cluster_found"] == r2["results"]["ghost_cluster_found"]
    assert r1["results"]["clusters_detected"] == r2["results"]["clusters_detected"]


# ── Suite-level tests ─────────────────────────────────────────────────────────


def test_run_scenario_invalid_scenario_raises():
    suite = _make_suite()
    with pytest.raises(SimulationError, match="Unknown scenario"):
        suite.run_scenario("invalid_scenario")


def test_run_scenario_dispatches_correctly(monkeypatch):
    _mock_causal_backends(monkeypatch)
    suite = _make_suite()
    r1 = suite.run_scenario(
        "consensus_illusion",
        compatibility_metric="mas_compatible",
        enforce_thresholds=False,
    )
    assert r1["scenario"] == "consensus_illusion"
    r2 = suite.run_scenario("lynchpin_of_certainty", n_models=_NM, n_components=_NC)
    assert r2["scenario"] == "lynchpin_of_certainty"
    r3 = suite.run_scenario("ghost_discovery", n_models=_NM, n_components=_NC)
    assert r3["scenario"] == "ghost_discovery"

from __future__ import annotations

import pandas as pd
import pytest
import torch

from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from dyadic.engine import DyadicEngine
from dyadic.tensor_engine import (
    causal_mask,
    resolve_device,
    status_codes,
    structural_dyad_scores,
    structural_similarity_matrix,
)


def _make_simple_world():
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
    registry = ComponentRegistry(data)

    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "unknown", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "unknown"},
    ]
    state = StateTensor.from_records(registry, records)
    return registry, state


def _make_timing_world():
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
    registry = ComponentRegistry(data)

    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal"},
    ]
    state = StateTensor.from_records(registry, records)
    return registry, state


def test_causal_mask_shape_and_values():
    reg, state = _make_simple_world()
    mask = causal_mask(state)
    assert mask.shape == (2, 3)
    assert mask.dtype == torch.bool

    assert (
        mask[state.model_index["M0001"], state.component_index["C0001"]].item() is True
    )
    assert (
        mask[state.model_index["M0002"], state.component_index["C0002"]].item() is False
    )


def test_status_codes():
    reg, state = _make_simple_world()
    codes = status_codes(state)
    assert codes.shape == (2, 3)

    i1 = state.model_index["M0001"]
    j1 = state.component_index["C0001"]
    jb = state.component_index["C0002"]
    assert codes[i1, j1].item() == 1

    i2 = state.model_index["M0002"]
    assert codes[i2, jb].item() == 0


def test_structural_similarity_matches_dyadic_engine_small_world():
    reg, state = _make_simple_world()
    engine = DyadicEngine()
    dyads = engine.compare_pairs(state, reg, mode="basic")

    matrix, ordered_ids = structural_similarity_matrix(state, reg, device="cpu")
    assert ordered_ids == ["M0001", "M0002"]

    for d in dyads:
        i = ordered_ids.index(d["ego_id"])
        j = ordered_ids.index(d["alter_id"])
        tensor_sim = round(float(matrix[i, j]), 6)
        assert tensor_sim == pytest.approx(d["similarity_rate"], abs=1e-6)


def test_structural_dyad_scores_excludes_self_dyads():
    reg, state = _make_simple_world()
    scores, ids = structural_dyad_scores(state, reg)
    assert len(scores) == 2
    assert len(ids) == 2
    for did in ids:
        parts = did.split("__")
        assert parts[0] != parts[1]


def test_structural_dyad_scores_preserves_directed_order():
    reg, state = _make_simple_world()
    scores, ids = structural_dyad_scores(state, reg)
    assert "M0001__M0002" in ids
    assert "M0002__M0001" in ids


def test_structural_dyad_scores_respects_model_id_subset():
    reg, state = _make_simple_world()
    scores, ids = structural_dyad_scores(state, reg, model_ids=["M0001"])
    assert len(scores) == 0
    assert ids == []


def test_tensor_engine_respects_temporally_invalid_edges():
    reg, state = _make_timing_world()
    engine = DyadicEngine()

    dyads = engine.compare_pairs(state, reg, mode="basic")
    matrix, ordered_ids = structural_similarity_matrix(state, reg, device="cpu")

    for d in dyads:
        i = ordered_ids.index(d["ego_id"])
        j = ordered_ids.index(d["alter_id"])
        tensor_sim = round(float(matrix[i, j]), 6)
        assert tensor_sim == pytest.approx(
            d["similarity_rate"], abs=1e-6
        ), f"Tensor {tensor_sim} != Dyadic {d['similarity_rate']} for {d['dyad_id']}"


def test_resolve_device_cpu():
    dev = resolve_device("cpu")
    assert dev.type == "cpu"


def test_resolve_device_cuda_unavailable_errors():
    if not torch.cuda.is_available():
        with pytest.raises(ValueError, match="CUDA requested but not available"):
            resolve_device("cuda")
    else:
        dev = resolve_device("cuda")
        assert dev.type == "cuda"


def test_resolve_device_auto():
    dev = resolve_device("auto")
    assert dev.type in ("cpu", "cuda")

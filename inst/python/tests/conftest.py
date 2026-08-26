import pytest
import pandas as pd
from fastapi.testclient import TestClient
from api.main import app
from registry.schema import ComponentRegistry
from state.tensor import StateTensor
from dyadic.engine import DyadicEngine


@pytest.fixture
def client():
    """Return type: TestClient. Purpose: exercise FastAPI routes in tests."""
    return TestClient(app)


@pytest.fixture
def sample_registry():
    """Return type: ComponentRegistry. Shape: 10 components, 6 nodes, 4 edges."""
    data = pd.DataFrame(
        [
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
                "type": "node",
                "source": "Z",
                "target": None,
                "direction": None,
                "description": "Variable Z",
            },
            {
                "comp_id": "C0004",
                "type": "node",
                "source": "A",
                "target": None,
                "direction": None,
                "description": "Variable A",
            },
            {
                "comp_id": "C0005",
                "type": "node",
                "source": "B",
                "target": None,
                "direction": None,
                "description": "Variable B",
            },
            {
                "comp_id": "C0006",
                "type": "node",
                "source": "C",
                "target": None,
                "direction": None,
                "description": "Variable C",
            },
            {
                "comp_id": "C0007",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X to Y",
            },
            {
                "comp_id": "C0008",
                "type": "edge",
                "source": "Y",
                "target": "Z",
                "direction": "->",
                "description": "Y to Z",
            },
            {
                "comp_id": "C0009",
                "type": "edge",
                "source": "A",
                "target": "B",
                "direction": "<->",
                "description": "A to B bidirectional",
            },
            {
                "comp_id": "C0010",
                "type": "edge",
                "source": "B",
                "target": "C",
                "direction": "->",
                "description": "B to C",
            },
        ]
    )
    return ComponentRegistry(data)


@pytest.fixture
def sample_state(sample_registry):
    """Return type: StateTensor. Shape: 3 models x 10 components."""
    records = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "unknown", "timing": None},
        {"model_id": "M0001", "comp_id": "C0004", "status": "causal", "timing": 1},
        {
            "model_id": "M0001",
            "comp_id": "C0005",
            "status": "non-causal",
            "timing": None,
        },
        {"model_id": "M0001", "comp_id": "C0006", "status": "causal", "timing": 3},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "unknown", "timing": None},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal", "timing": 3},
        {
            "model_id": "M0002",
            "comp_id": "C0004",
            "status": "non-causal",
            "timing": None,
        },
        {"model_id": "M0002", "comp_id": "C0005", "status": "causal", "timing": None},
        {"model_id": "M0002", "comp_id": "C0006", "status": "unknown", "timing": None},
        {"model_id": "M0003", "comp_id": "C0001", "status": "unknown", "timing": None},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0003", "comp_id": "C0004", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0005", "status": "causal", "timing": None},
        {
            "model_id": "M0003",
            "comp_id": "C0006",
            "status": "non-causal",
            "timing": None,
        },
    ]
    return StateTensor.from_records(sample_registry, records)


@pytest.fixture
def sample_dyad_matrix(sample_registry, sample_state):
    """Return type: list[list[dict]]. Shape: 3 x 3 dyad matrix."""
    engine = DyadicEngine()
    return engine.compare_matrix(sample_state, sample_registry)


@pytest.fixture
def rpy2_mock(monkeypatch):
    """Return type: dict. Purpose: predefined causal wrapper mock results."""
    result = {"adjustment_sets": [["X", "Z"]], "identified": True}

    try:
        from dyadic import causal
    except ImportError:
        return result

    monkeypatch.setattr(
        causal.CausalWrapper,
        "compute_adjustment_sets",
        lambda self, dag_spec: result["adjustment_sets"],
    )
    return result

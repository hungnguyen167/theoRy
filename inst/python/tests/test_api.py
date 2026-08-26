import json

import pytest
from fastapi import FastAPI
from fastapi.testclient import TestClient
from api.middleware import add_request_logging_middleware
from api.models import (
    MAX_API_TIMING,
    DagSpec,
    ExpandModelStatesRequest,
    NodeSpec,
    StateRecord,
)

INVALID_API_TIMINGS = [
    pytest.param(True, id="boolean"),
    pytest.param(1.5, id="fraction"),
    pytest.param("1", id="string"),
    pytest.param(0, id="zero"),
    pytest.param(-1, id="negative"),
    pytest.param(MAX_API_TIMING + 1, id="above-int32-max"),
]


def _minimal_registry_data():
    return [
        {
            "comp_id": "C0001",
            "type": "node",
            "source": "X",
            "target": None,
            "direction": None,
            "description": "X",
        }
    ]


@pytest.mark.parametrize("invalid_timing", INVALID_API_TIMINGS)
def test_state_record_rejects_invalid_api_timing(invalid_timing):
    with pytest.raises(ValueError):
        StateRecord(
            model_id="M0001",
            comp_id="C0001",
            status="present",
            timing=invalid_timing,
        )


@pytest.mark.parametrize("invalid_timing", INVALID_API_TIMINGS)
def test_node_spec_rejects_invalid_api_timing(invalid_timing):
    with pytest.raises(ValueError):
        NodeSpec(name="X", timing=invalid_timing)
    with pytest.raises(ValueError):
        NodeSpec(name="X", timing_options=[1, invalid_timing])


@pytest.mark.parametrize("invalid_timing", INVALID_API_TIMINGS)
def test_expand_request_rejects_invalid_api_timing(invalid_timing):
    registry_data = _minimal_registry_data()
    with pytest.raises(ValueError):
        ExpandModelStatesRequest(
            registry_data=registry_data,
            node_timing={"X": invalid_timing},
        )
    with pytest.raises(ValueError):
        ExpandModelStatesRequest(
            registry_data=registry_data,
            timing_options={"X": [invalid_timing]},
        )


@pytest.mark.parametrize("invalid_timing", INVALID_API_TIMINGS)
def test_dag_spec_rejects_invalid_api_timing(invalid_timing):
    with pytest.raises(ValueError):
        DagSpec(
            nodes=["X", "Y"],
            edges=[],
            exposure="X",
            outcome="Y",
            timing={"X": invalid_timing, "Y": 1},
        )


@pytest.mark.parametrize("invalid_timing", INVALID_API_TIMINGS[:3])
def test_json_timing_values_are_not_coerced(invalid_timing):
    payloads = [
        (
            StateRecord,
            {
                "model_id": "M0001",
                "comp_id": "C0001",
                "status": "present",
                "timing": invalid_timing,
            },
        ),
        (NodeSpec, {"name": "X", "timing": invalid_timing}),
        (
            NodeSpec,
            {"name": "X", "timing_options": [invalid_timing]},
        ),
        (
            ExpandModelStatesRequest,
            {
                "registry_data": _minimal_registry_data(),
                "node_timing": {"X": invalid_timing},
            },
        ),
        (
            ExpandModelStatesRequest,
            {
                "registry_data": _minimal_registry_data(),
                "timing_options": {"X": [invalid_timing]},
            },
        ),
        (
            DagSpec,
            {
                "nodes": ["X", "Y"],
                "edges": [],
                "exposure": "X",
                "outcome": "Y",
                "timing": {"X": invalid_timing},
            },
        ),
    ]

    for model, payload in payloads:
        with pytest.raises(ValueError):
            model.model_validate_json(json.dumps(payload))


def test_api_timing_maximum_and_existing_none_values_are_valid():
    assert (
        StateRecord(
            model_id="M0001",
            comp_id="C0001",
            status="present",
            timing=MAX_API_TIMING,
        ).timing
        == MAX_API_TIMING
    )
    assert NodeSpec(name="X", timing=MAX_API_TIMING, timing_options=[MAX_API_TIMING])
    assert ExpandModelStatesRequest(
        registry_data=_minimal_registry_data(),
        node_timing={"X": MAX_API_TIMING},
        timing_options={"X": [MAX_API_TIMING]},
    )
    assert DagSpec(
        nodes=["X", "Y"],
        edges=[],
        exposure="X",
        outcome="Y",
        timing={"X": None, "Y": MAX_API_TIMING},
    )


def test_health_endpoint_returns_success_wrapper(client):
    response = client.get("/api/v1/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["status"] == "healthy"
    assert data["data"]["version"] == "0.2.0"


def test_middleware_adds_x_request_id(client):
    response = client.get("/api/v1/health")
    assert "X-Request-ID" in response.headers


def test_unhandled_route_error_returns_internal_error():
    test_app = FastAPI()
    add_request_logging_middleware(test_app)

    @test_app.get("/error")
    async def error():
        raise ValueError("test error")

    client = TestClient(test_app)
    response = client.get("/error")
    assert response.status_code == 500
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INTERNAL_ERROR"
    assert data["message"] == "An unexpected error occurred"
    assert "X-Request-ID" in response.headers


def test_valid_dyad_matrix_request_returns_200(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [
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
                    "description": "X to Y",
                },
            ],
            "state_data": [
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
                    "status": "causal",
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
                    "status": "unknown",
                    "timing": None,
                },
                {
                    "model_id": "M0002",
                    "comp_id": "C0003",
                    "status": "causal",
                    "timing": None,
                },
            ],
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert "dyads" in data["data"]
    assert data["data"]["model_count"] == 2
    assert data["data"]["dyad_count"] == 2  # 2 models => 2 directed pairs
    assert len(data["data"]["dyads"]) == 2
    dyad = data["data"]["dyads"][0]
    assert dyad["ego_id"] == "M0001"
    assert dyad["alter_id"] == "M0002"
    assert dyad["dyad_id"] == "M0001__M0002"


def test_empty_registry_returns_400(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [],
            "state_data": [],
        },
    )
    assert response.status_code == 400
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "EMPTY_REGISTRY"
    assert data["message"] == "Registry must contain at least one component"


def test_missing_registry_data_returns_422(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "state_data": [],
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "VALIDATION_ERROR"


def test_invalid_state_data_returns_422(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                },
            ],
            "state_data": [
                {
                    "model_id": "M0001",
                    "comp_id": "C0001",
                    "status": "invalid_status",
                    "timing": 1,
                },
            ],
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "VALIDATION_ERROR"


def test_dyadic_error_returns_500(client):
    # Under sparse semantics, edge X->Y with no Y node is inapplicable
    # and handled gracefully, so this returns 200.
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [
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
                    "type": "edge",
                    "source": "X",
                    "target": "Y",
                    "direction": "->",
                    "description": "X to Y",
                },
            ],
            "state_data": [
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
                    "timing": None,
                },
            ],
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"


def test_swagger_docs_include_endpoint(client):
    response = client.get("/openapi.json")
    assert response.status_code == 200
    paths = response.json()["paths"]
    assert "/api/v1/dyad-matrix" in paths


def test_dyad_matrix_with_inferred_model_ids(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                },
            ],
            "state_data": [
                {
                    "model_id": "M0002",
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 1,
                },
                {
                    "model_id": "M0001",
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 1,
                },
            ],
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["model_count"] == 2
    assert data["data"]["dyad_count"] == 2  # 2 models => 2 directed pairs
    assert len(data["data"]["dyads"]) == 2


def test_dyad_matrix_with_explicit_model_ids(client):
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                },
            ],
            "state_data": [
                {
                    "model_id": "M0001",
                    "comp_id": "C0001",
                    "status": "causal",
                    "timing": 1,
                },
            ],
            "model_ids": ["M0001", "M0002"],
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["model_count"] == 2
    assert data["data"]["dyad_count"] == 2  # 2 models => 2 directed pairs
    assert len(data["data"]["dyads"]) == 2


# --- Story 1.2A / 1.3A endpoint tests ----------------------------------------


def test_build_component_registry_basic(client):
    response = client.post(
        "/api/v1/component-registry",
        json={
            "nodes": [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ],
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["status"] == "success"
    assert "registry_data" in body["data"]
    assert "summary" in body["data"]
    assert body["data"]["summary"]["nodes"] == 2
    assert body["data"]["summary"]["edges"] >= 1


def test_build_component_registry_no_timing(client):
    response = client.post(
        "/api/v1/component-registry",
        json={
            "nodes": [
                {"name": "X"},
                {"name": "Y"},
                {"name": "Z"},
            ],
            "respect_timing": False,
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["data"]["summary"]["edges"] == 5


def test_build_component_registry_bidirectional(client):
    response = client.post(
        "/api/v1/component-registry",
        json={
            "nodes": [
                {"name": "X1", "timing": 1},
                {"name": "X2", "timing": 1},
                {"name": "Y", "timing": 2},
            ],
            "include_bidirectional": True,
            "exposure": "X1",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["data"]["summary"]["bidirectional_edges"] == 1
    bidirected = [
        (row["source"], row["target"])
        for row in body["data"]["registry_data"]
        if row["direction"] == "<->"
    ]
    assert bidirected == [("X1", "X2")]


def test_build_component_registry_requires_exposure_and_outcome(client):
    response = client.post(
        "/api/v1/component-registry",
        json={"nodes": []},
    )
    assert response.status_code == 422


def test_expand_model_states_with_seed_claims(client):
    response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": [
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
            ],
            "mode": "sampled",
            "n_models": 5,
            "seed": 42,
            "seed_claims": [
                {
                    "model_id": "M0001",
                    "comp_id": "C0001",
                    "status": "causal",
                },
                {
                    "model_id": "M0001",
                    "comp_id": "C0003",
                    "status": "causal",
                },
            ],
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["data"]["component_count"] == 3
    assert "pruning_report" in body["data"]
    assert "seeded_model_ids" in body["data"]
    assert "M0001" in body["data"]["seeded_model_ids"]
    # All state records should have a seeded field
    for rec in body["data"]["state_data"]:
        assert "seeded" in rec


def test_expand_model_states_exhaustive(client):
    response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": [
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
            ],
            "mode": "exhaustive",
            "node_timing": {"X": 1, "Y": 2},
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["data"]["model_count"] == 3  # 1 edge => 3^1 models
    assert body["data"]["component_count"] == 3


def test_implicit_exposure_outcome_order_survives_registry_and_state_api(client):
    registry_response = client.post(
        "/api/v1/component-registry",
        json={
            "nodes": [{"name": "X"}, {"name": "Z"}, {"name": "Y"}],
            "include_bidirectional": True,
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert registry_response.status_code == 200
    registry_data = registry_response.json()["data"]["registry_data"]
    fixed = [record for record in registry_data if record["fixed_status"] == "causal"]
    assert [(record["source"], record["target"]) for record in fixed] == [("X", "Y")]
    assert not any(
        record["source"] == "Y" and record["target"] == "X" for record in registry_data
    )

    state_response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": registry_data,
            "mode": "sampled",
            "n_models": 5,
            "seed": 42,
            "node_policy": "vary",
        },
    )
    assert state_response.status_code == 200
    state_data = state_response.json()["data"]["state_data"]
    fixed_cid = fixed[0]["comp_id"]
    assert {
        record["status"] for record in state_data if record["comp_id"] == fixed_cid
    } == {"causal"}


def test_symbolic_universe_rejects_unknown_exposure_outcome(client):
    response = client.post(
        "/api/v1/symbolic/universe",
        json={
            "nodes": [{"name": "X"}, {"name": "Y"}],
            "exposure": "X",
            "outcome": "Z",
        },
    )
    assert response.status_code == 422


def test_expand_model_states_vary_requires_exposure_outcome_nodes(client):
    response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": [
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
                {
                    "comp_id": "C0005",
                    "type": "edge",
                    "source": "X",
                    "target": "Z",
                    "direction": "->",
                    "description": "X->Z",
                },
                {
                    "comp_id": "C0006",
                    "type": "edge",
                    "source": "Y",
                    "target": "Z",
                    "direction": "->",
                    "description": "Y->Z",
                },
            ],
            "mode": "exhaustive",
            "node_timing": {"X": 1, "Y": 2, "Z": 3},
            "node_policy": "vary",
            "exposure": "X",
            "outcome": "Z",
        },
    )

    assert response.status_code == 200
    body = response.json()
    assert body["data"]["model_count"] == 30

    node_ids = {"C0001", "C0002", "C0003"}
    required = {"C0001", "C0003"}
    nodes_by_model = {}
    for rec in body["data"]["state_data"]:
        if rec["comp_id"] in node_ids:
            nodes_by_model.setdefault(rec["model_id"], set()).add(rec["comp_id"])

    assert all(required.issubset(nodes) for nodes in nodes_by_model.values())


def test_expand_model_states_sampled(client):
    response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": [
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
            ],
            "mode": "sampled",
            "n_models": 2,
            "seed": 42,
            "node_timing": {"X": 1, "Y": 2},
        },
    )
    assert response.status_code == 200
    body = response.json()
    assert body["data"]["model_count"] == 2


def test_expand_model_states_invalid_mode(client):
    response = client.post(
        "/api/v1/model-states",
        json={
            "registry_data": [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                },
            ],
            "mode": "invalid",
        },
    )
    assert response.status_code == 422


# --- Story 2.4 enhanced dyad-matrix endpoint tests ---------------------------


def _sample_registry_and_state():
    registry_data = [
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
    state_data = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal", "timing": None},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal", "timing": None},
        {"model_id": "M0003", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal", "timing": None},
    ]
    return registry_data, state_data


def test_dyad_matrix_basic_default_mode(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={"registry_data": reg, "state_data": st},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["mode"] == "basic"
    assert "dyads" in data["data"]
    assert data["data"]["model_count"] == 3
    assert data["data"]["dyad_count"] == 6
    dyad = data["data"]["dyads"][0]
    assert "mas_ego" not in dyad
    assert "identified_compatible" not in dyad


def test_dyad_matrix_full_mode_includes_causal_metrics(client, monkeypatch):
    from dyadic.causal import CausalWrapper
    from dyadic.identification import IdentificationWrapper

    monkeypatch.setattr(
        CausalWrapper,
        "compute_adjustment_sets",
        lambda self, dag_spec: [["Z"]],
    )
    monkeypatch.setattr(
        CausalWrapper,
        "check_identification",
        lambda self, dag_spec: True,
    )
    monkeypatch.setattr(
        IdentificationWrapper,
        "identify_total_effect",
        lambda self, **kwargs: (True, "identified"),
    )

    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["mode"] == "full"
    assert data["data"]["causal_backend"] == "r"
    dyad = data["data"]["dyads"][0]
    assert "mas_ego" in dyad
    assert "mas_alter" in dyad
    assert "mas_compatible" in dyad
    assert "identified_ego" in dyad
    assert "identified_alter" in dyad
    assert "identified_compatible" in dyad


def test_dyad_matrix_full_mode_uses_native_causal_backend(client):
    reg, st = _sample_registry_and_state()
    for record in st:
        if record["model_id"] == "M0002" and record["comp_id"] == "C0002":
            record["status"] = "causal"
            record["timing"] = 2

    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "Y",
            "causal_backend": "native",
        },
    )

    assert response.status_code == 200
    data = response.json()["data"]
    assert data["causal_backend"] == "native"
    assert data["dyads"][0]["mas_ego"] == [[]]
    assert data["dyads"][0]["identified_ego"] is True


def test_dyad_matrix_rejects_query_without_fixed_direct_edge(client):
    reg = [
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
            "source": "M",
            "target": None,
            "direction": None,
            "description": "M",
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
            "source": "X",
            "target": "M",
            "direction": "->",
            "description": "X->M",
        },
        {
            "comp_id": "C0005",
            "type": "edge",
            "source": "M",
            "target": "Y",
            "direction": "->",
            "description": "M->Y",
        },
        {
            "comp_id": "C0006",
            "type": "edge",
            "source": "X",
            "target": "Y",
            "direction": "<->",
            "description": "X confounded with Y",
        },
    ]
    st = [
        {
            "model_id": model_id,
            "comp_id": component_id,
            "status": "causal",
            "timing": timing,
        }
        for model_id in ("M0001", "M0002")
        for component_id, timing in (
            ("C0001", 1),
            ("C0002", 2),
            ("C0003", 3),
            ("C0004", None),
            ("C0005", None),
            ("C0006", None),
        )
    ]

    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "Y",
            "causal_backend": "native",
        },
    )

    assert response.status_code == 422
    assert response.json()["code"] == "INVALID_CAUSAL_QUERY"


def test_dyad_matrix_full_mode_causal_error_returns_wrapper(client, monkeypatch):
    from dyadic.causal import CausalError, CausalWrapper
    from api.routes import engine

    engine._causal_cache.clear()

    def raise_causal_error(self, dag_spec):
        raise CausalError(
            "Cannot load R package dagitty. "
            "Install in R with: install.packages('dagitty')\n"
            "Underlying error: package not found"
        )

    monkeypatch.setattr(
        CausalWrapper,
        "compute_adjustment_sets",
        raise_causal_error,
    )

    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 500
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "CAUSAL_ERROR"
    assert "dagitty" in data["message"]


def test_dyad_matrix_single_ref_requires_reference_id(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "single-ref",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_REFERENCE"
    assert "reference_id" in data["message"]


def test_dyad_matrix_single_ref_returns_n_minus_one(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "single-ref",
            "reference_id": "M0001",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["mode"] == "single-ref"
    assert data["data"]["reference_id"] == "M0001"
    assert data["data"]["model_count"] == 3
    assert data["data"]["dyad_count"] == 2  # N-1
    dyads = data["data"]["dyads"]
    for d in dyads:
        assert d["ego_id"] == "M0001"


def test_dyad_matrix_single_ref_rejects_unknown_reference(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "single-ref",
            "reference_id": "M0999",
        },
    )
    assert response.status_code == 400
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_REFERENCE"


def test_dyad_matrix_two_stage_requires_positive_top_k(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "two-stage",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_TOP_K"


def test_dyad_matrix_two_stage_rejects_non_positive_top_k(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "two-stage",
            "top_k": 0,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_TOP_K"


def test_dyad_matrix_two_stage_returns_heatmap_and_details(client, monkeypatch):
    from dyadic.causal import CausalWrapper

    monkeypatch.setattr(
        CausalWrapper,
        "compute_adjustment_sets",
        lambda self, dag_spec: [["Z"]],
    )
    monkeypatch.setattr(
        CausalWrapper,
        "check_identification",
        lambda self, dag_spec: True,
    )

    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "two-stage",
            "top_k": 2,
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["mode"] == "two-stage"
    assert "heatmap_summary" in data["data"]
    assert "detailed_comparisons" in data["data"]
    hs = data["data"]["heatmap_summary"]
    assert hs["model_count"] == 3
    assert hs["dyad_count"] == 6
    assert hs["top_k"] == 2
    dc = data["data"]["detailed_comparisons"]
    assert len(dc) == 2
    for d in dc:
        assert "mas_ego" in d
        assert "identified_compatible" in d


def test_dyad_matrix_invalid_mode_returns_422(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "invalid-mode",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "VALIDATION_ERROR"


def test_dyad_matrix_preserves_directed_pair_schema(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "basic",
        },
    )
    assert response.status_code == 200
    data = response.json()
    dyads = data["data"]["dyads"]
    assert len(dyads) == 6
    ego_alter_pairs = {(d["ego_id"], d["alter_id"]) for d in dyads}
    assert ("M0001", "M0002") in ego_alter_pairs
    assert ("M0002", "M0001") in ego_alter_pairs
    for d in dyads:
        assert d["ego_id"] != d["alter_id"]
        assert d["dyad_id"] == f"{d['ego_id']}__{d['alter_id']}"
        assert "similarity_rate" in d
        assert "timing_compatible" in d
        assert "existence_conflict" in d
        assert "repair_cost" in d


def test_dyad_matrix_full_with_exposure_outcome(client, monkeypatch):
    from dyadic.causal import CausalWrapper
    from dyadic.identification import IdentificationWrapper

    monkeypatch.setattr(
        CausalWrapper,
        "compute_adjustment_sets",
        lambda self, dag_spec: [["Z"]],
    )
    monkeypatch.setattr(
        CausalWrapper,
        "check_identification",
        lambda self, dag_spec: True,
    )
    monkeypatch.setattr(
        IdentificationWrapper,
        "identify_total_effect",
        lambda self, **kwargs: (True, "identified"),
    )

    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["exposure"] == "X"
    assert data["data"]["outcome"] == "Y"


def test_dyad_matrix_rejects_partial_exposure_outcome(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "INVALID_CAUSAL_TARGET"


def test_dyad_matrix_rejects_invalid_exposure_node(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "Missing",
            "outcome": "Y",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "INVALID_CAUSAL_TARGET"


def test_dyad_matrix_same_exposure_outcome_returns_422(client):
    reg, st = _sample_registry_and_state()
    response = client.post(
        "/api/v1/dyad-matrix",
        json={
            "registry_data": reg,
            "state_data": st,
            "mode": "full",
            "exposure": "X",
            "outcome": "X",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "INVALID_CAUSAL_TARGET"


# --- Story 3.3 delta-u endpoint tests -----------------------------------------


def _populate_dyad_context(client):
    """Helper: call /dyad-matrix with uncertain components so delta-u works."""
    registry_data = [
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
    state_data = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0001", "comp_id": "C0004", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0002", "comp_id": "C0004", "status": "unknown"},
        {"model_id": "M0003", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0003", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0003", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0003", "comp_id": "C0004", "status": "causal"},
        # Resolution-closed variants of M0002 for marginal crux.
        {"model_id": "M0004", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0004", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0004", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0004", "comp_id": "C0004", "status": "causal"},
        {"model_id": "M0005", "comp_id": "C0001", "status": "causal", "timing": 1},
        {"model_id": "M0005", "comp_id": "C0002", "status": "causal", "timing": 2},
        {"model_id": "M0005", "comp_id": "C0003", "status": "causal", "timing": 3},
        {"model_id": "M0005", "comp_id": "C0004", "status": "non-causal"},
    ]
    resp = client.post(
        "/api/v1/dyad-matrix",
        json={"registry_data": registry_data, "state_data": state_data},
    )
    assert resp.status_code == 200
    return resp


def test_delta_u_without_dyads_returns_no_dyads(client):
    from api.session import clear_latest_dyad_context

    clear_latest_dyad_context()
    response = client.post(
        "/api/v1/delta-u",
        json={"top_k": 5},
    )
    assert response.status_code == 400
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "NO_DYADS"
    _populate_dyad_context(client)


def test_delta_u_single_component_returns_result(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"component_id": "C0004"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert "result" in data["data"]
    result = data["data"]["result"]
    assert result["component_id"] == "C0004"
    assert "delta_u_causal" in result
    assert "delta_u_non_causal" in result
    assert "delta_u" in result
    assert "best_resolution" in result
    assert "dyads_improved" in result
    assert "dyads_worsened" in result
    assert result["crux_mode"] == "marginal"
    assert result["models_changed_causal"] == 1
    assert result["mapping_coverage_causal"] == 1.0
    assert "timing_pruned_models_causal" in result
    assert "timing_pruned_models_non_causal" in result
    assert "models_pruned_causal" in result
    assert "models_pruned_non_causal" in result
    assert "post_model_count_causal" in result
    assert "post_model_count_non_causal" in result
    assert "post_dyad_count_causal" in result
    assert "post_dyad_count_non_causal" in result
    assert "insufficient_post_models_causal" in result
    assert "insufficient_post_models_non_causal" in result
    assert data["data"]["crux_mode"] == "marginal"
    assert "computation_mode" in data["data"]


def test_delta_u_ranking_returns_rankings(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"top_k": 5},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert "rankings" in data["data"]
    rankings = data["data"]["rankings"]
    assert isinstance(rankings, list)
    if rankings:
        first = rankings[0]
        assert first["rank"] == 1
        assert "component_id" in first
        assert "delta_u" in first
        assert "type" in first
        assert "source" in first
        assert "best_resolution" in first
    assert "computation_mode" in data["data"]
    assert "component_count" in data["data"]


def test_delta_u_non_closed_multiverse_returns_actionable_422(client):
    registry_data = [
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
    state_data = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "unknown"},
    ]
    dyad_response = client.post(
        "/api/v1/dyad-matrix",
        json={"registry_data": registry_data, "state_data": state_data},
    )
    assert dyad_response.status_code == 200

    response = client.post("/api/v1/delta-u", json={"top_k": 5})
    assert response.status_code == 422
    assert response.json()["code"] == "DELTA_U_ERROR"
    assert "resolution-closed" in response.json()["message"]


def test_delta_u_two_stage_with_threshold(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "top_k": 5,
            "mode": "two-stage",
            "heatmap_threshold": 0.0,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["computation_mode"] == "two-stage"


def test_delta_u_default_mode_is_exhaustive(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"top_k": 5},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["computation_mode"] == "exhaustive"


def test_delta_u_unknown_component_returns_422(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"component_id": "C9999"},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "COMPONENT_NOT_FOUND"


def test_delta_u_invalid_top_k_returns_422(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"top_k": 0},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_TOP_K"


def test_delta_u_synergistic_sets_returns_field(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "top_k": 10,
            "synergistic_set_size": 2,
            "synergistic_search": "greedy",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert "rankings" in data["data"]
    assert "synergistic_sets" in data["data"]
    for entry in data["data"]["synergistic_sets"]:
        assert "components" in entry
        assert "delta_u_combined" in entry
        assert "synergy_score" in entry
        assert "label" in entry


def test_delta_u_invalid_synergy_set_size_returns_422(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "top_k": 5,
            "synergistic_set_size": 1,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_SYNERGY_REQUEST"


def test_delta_u_invalid_heatmap_threshold_returns_422(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "top_k": 5,
            "mode": "two-stage",
            "heatmap_threshold": 1.5,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_HEATMAP_THRESHOLD"


# --- Compatibility metric API tests ------------------------------------------


def test_delta_u_defaults_to_similarity_rate(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"top_k": 5},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["compatibility_metric"] == "similarity_rate"
    assert data["data"]["device"] == "auto"


def test_delta_u_response_includes_compatibility_metric_metadata(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "top_k": 5,
            "compatibility_metric": "similarity_rate",
            "device": "cpu",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["compatibility_metric"] == "similarity_rate"
    assert data["data"]["device"] == "cpu"


def test_delta_u_rejects_invalid_compatibility_metric(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "compatibility_metric": "invalid_mode",
        },
    )
    assert response.status_code == 422


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("scoring", "hybrid"),
        ("structural_weight", 0.5),
        ("causal_weight", 0.5),
        ("causal_metrics", ["mas_compatible"]),
    ],
)
def test_delta_u_rejects_removed_request_fields(client, field, value):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={field: value},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "VALIDATION_ERROR"
    assert "Extra inputs are not permitted" in data["message"]


@pytest.mark.parametrize(
    "compatibility_metric", ["mas_compatible", "identified_compatible"]
)
def test_delta_u_causal_metric_requires_exposure_outcome(client, compatibility_metric):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"compatibility_metric": compatibility_metric},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "INVALID_CAUSAL_TARGET"
    assert "requires both exposure and outcome" in data["message"]


def test_delta_u_cuda_unavailable_returns_422(client):
    import torch

    if torch.cuda.is_available():
        pytest.skip("CUDA is available")
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"device": "cuda"},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "CUDA_UNAVAILABLE"


# --- Story 4.3 clusters endpoint tests ----------------------------------------


def _populate_dyad_context_for_clustering(client):
    """Helper: call /dyad-matrix with enough models for clustering."""
    registry_data = [
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
    state_data = []
    for i in range(1, 8):
        mid = f"M{i:04d}"
        state_data.extend(
            [
                {"model_id": mid, "comp_id": "C0001", "status": "causal", "timing": 1},
                {"model_id": mid, "comp_id": "C0002", "status": "causal", "timing": 2},
                {
                    "model_id": mid,
                    "comp_id": "C0003",
                    "status": "causal" if i <= 4 else "unknown",
                },
            ]
        )
    resp = client.post(
        "/api/v1/dyad-matrix",
        json={"registry_data": registry_data, "state_data": state_data},
    )
    assert resp.status_code == 200
    return resp


def test_clusters_without_dyads_returns_no_dyads(client):
    from api.session import clear_latest_dyad_context

    clear_latest_dyad_context()
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2},
    )
    assert response.status_code == 400
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "NO_DYADS"


def test_clusters_basic_returns_assignments_and_summaries(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert "cluster_assignments" in data["data"]
    assert "cluster_summaries" in data["data"]
    assert "embedding_2d" in data["data"]
    assert data["data"]["model_count"] == 7
    assert "umap_components" in data["data"]
    assert "eps" in data["data"]
    assert "min_samples" in data["data"]


def test_clusters_without_prior_empty_ghost_list(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["data"]["ghost_clusters"] == []


def test_clusters_with_prior_returns_ghost_clusters(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={
            "eps": 0.5,
            "min_samples": 2,
            "prior_model_id": "M0001",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert "ghost_clusters" in data["data"]
    assert "prior_model_id" in data["data"]
    assert data["data"]["prior_model_id"] == "M0001"


def test_clusters_invalid_prior_model_returns_422(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={
            "eps": 0.5,
            "min_samples": 2,
            "prior_model_id": "INVALID",
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "MODEL_NOT_FOUND"


def test_clusters_invalid_eps_returns_422(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": -0.1, "min_samples": 2},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_EPS"


def test_clusters_invalid_min_samples_returns_422(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 1},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_MIN_SAMPLES"


def test_clusters_invalid_umap_components_returns_422(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2, "umap_components": 5},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_UMAP_COMPONENTS"


def test_clusters_invalid_umap_min_dist_returns_422(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2, "umap_min_dist": 1.5},
    )
    assert response.status_code == 422
    data = response.json()
    assert data["status"] == "error"
    assert data["code"] == "INVALID_UMAP_MIN_DIST"


def test_clusters_custom_score_field_is_echoed(client):
    dyads = []
    model_ids = [f"M{i:04d}" for i in range(1, 5)]
    for ego in model_ids:
        for alter in model_ids:
            if ego != alter:
                dyads.append(
                    {
                        "dyad_id": f"{ego}__{alter}",
                        "ego_id": ego,
                        "alter_id": alter,
                        "similarity_rate": 0.1,
                        "identified_compatible": True,
                    }
                )

    response = client.post(
        "/api/v1/clusters",
        json={
            "dyads": dyads,
            "model_ids": model_ids,
            "eps": 0.5,
            "min_samples": 2,
            "score_field": "identified_compatible",
        },
    )
    assert response.status_code == 200
    data = response.json()
    result = data["data"]
    assert result["score_field"] == "identified_compatible"
    assert result["metric_unique_values"] == [1.0]
    assert result["all_pairs_compatible"] is True
    assert result["all_pairs_incompatible"] is False
    assert result["profile_variance"] == 0.0
    assert result["degenerate_metric"] is True
    assert result["cluster_count"] == 0
    assert result["noise_count"] == len(model_ids)
    assert all(a["cluster_id"] is None for a in result["cluster_assignments"])


def test_clusters_response_includes_embedding_2d(client):
    _populate_dyad_context_for_clustering(client)
    response = client.post(
        "/api/v1/clusters",
        json={"eps": 0.5, "min_samples": 2},
    )
    assert response.status_code == 200
    data = response.json()
    embedding = data["data"]["embedding_2d"]
    assert "model_ids" in embedding
    assert "x" in embedding
    assert "y" in embedding
    assert len(embedding["model_ids"]) == 7
    assert len(embedding["x"]) == 7
    assert len(embedding["y"]) == 7


# --- Story 5.4 simulate endpoint tests ----------------------------------------


def test_simulate_consensus_illusion_returns_results(client, monkeypatch):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [
            [
                f"X{i}"
                for i in range(2, 7)
                if (f"X{i}", "X1") in set(map(tuple, dag_spec["edges"]))
            ]
        ],
    )
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 20,
            "n_components": 50,
            "compatibility_metric": "mas_compatible",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["scenario"] == "consensus_illusion"
    results = data["data"]["results"]
    assert data["data"]["n_models"] == 192
    assert results["mean_similarity_rate"] > results["compatibility_rate"]
    assert results["resolved_model_count"] == 128
    assert results["partial_model_count"] == 64
    registry = data["data"]["artifacts"]["registry_data"]
    assert all(
        "visibility" not in row and "illusion_role" not in row for row in registry
    )
    assert all(row["direction"] != "<->" for row in registry)


def test_simulate_include_bidirectional_is_rejected(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 10,
            "n_components": 10,
            "include_bidirectional": True,
            "enforce_thresholds": False,
        },
    )
    assert response.status_code == 422
    assert "directed components only" in response.text


def test_simulate_seeded_rejects_include_bidirectional(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "include_bidirectional": True,
            "registry_data": [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "X",
                }
            ],
            "state_data": [
                {"model_id": "M0001", "comp_id": "C0001", "status": "present"}
            ],
        },
    )

    assert response.status_code == 422
    assert "directed components only" in response.text


def test_simulate_lynchpin_of_certainty_returns_phase_transition(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "lynchpin_of_certainty",
            "n_models": 20,
            "n_components": 50,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["scenario"] == "lynchpin_of_certainty"
    results = data["data"]["results"]
    assert results["post_resolution_compatibility"] > results["baseline_compatibility"]
    assert results["phase_transition_score"] == pytest.approx(
        results["post_resolution_compatibility"] - results["baseline_compatibility"],
        abs=1e-6,
    )


def test_seeded_lynchpin_non_closed_multiverse_returns_actionable_422(client):
    registry_data = [
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
    state_data = [
        {"model_id": "M0001", "comp_id": "C0001", "status": "present", "timing": 1},
        {"model_id": "M0001", "comp_id": "C0002", "status": "present", "timing": 2},
        {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        {"model_id": "M0002", "comp_id": "C0001", "status": "present", "timing": 1},
        {"model_id": "M0002", "comp_id": "C0002", "status": "present", "timing": 2},
        {"model_id": "M0002", "comp_id": "C0003", "status": "unknown"},
    ]

    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "lynchpin_of_certainty",
            "registry_data": registry_data,
            "state_data": state_data,
        },
    )

    assert response.status_code == 422
    assert response.json()["code"] == "DELTA_U_ERROR"
    assert "resolution-closed" in response.json()["message"]


def test_simulate_ghost_discovery_returns_ghost_clusters(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "ghost_discovery",
            "n_models": 20,
            "n_components": 50,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["scenario"] == "ghost_discovery"
    assert data["data"]["results"]["ghost_cluster_found"] is True


def test_simulate_ghost_discovery_accepts_exposure_outcome(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "ghost_discovery",
            "n_models": 20,
            "n_components": 50,
            "exposure": "X1",
            "outcome": "X2",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["scenario"] == "ghost_discovery"


def test_simulate_invalid_scenario_returns_422(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "invalid_scenario",
            "n_models": 30,
            "n_components": 15,
        },
    )
    assert response.status_code == 422


@pytest.mark.parametrize(
    "compatibility_metric", ["mas_compatible", "identified_compatible"]
)
def test_generated_consensus_illusion_infers_exposure_outcome(
    client, compatibility_metric, monkeypatch
):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )
    monkeypatch.setattr(
        "dyadic.identification.IdentificationWrapper.identify_total_effect",
        lambda self, **kwargs: (True, "identified"),
    )
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 10,
            "n_components": 10,
            "compatibility_metric": compatibility_metric,
            "enforce_thresholds": False,
        },
    )
    assert response.status_code == 200
    results = response.json()["data"]["results"]
    assert results["exposure"] == "X1"
    assert results["outcome"] == "Y"


def test_simulate_invalid_n_models_returns_422(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 3,
            "n_components": 15,
        },
    )
    assert response.status_code == 422


def test_simulate_invalid_n_components_returns_422(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 30,
            "n_components": 2,
        },
    )
    assert response.status_code == 422


def test_simulate_response_has_artifacts(client, monkeypatch):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "consensus_illusion",
            "n_models": 20,
            "n_components": 50,
            "compatibility_metric": "mas_compatible",
            "enforce_thresholds": False,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert "artifacts" in data["data"]
    assert "registry_data" in data["data"]["artifacts"]
    assert "state_data" in data["data"]["artifacts"]
    assert "model_ids" in data["data"]["artifacts"]
    assert "summary_stats" in data["data"]["artifacts"]


def test_simulate_deterministic_with_seed(client, monkeypatch):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )
    payload = {
        "scenario": "consensus_illusion",
        "n_models": 20,
        "n_components": 50,
        "random_state": 42,
        "compatibility_metric": "mas_compatible",
        "enforce_thresholds": False,
    }
    r1 = client.post("/api/v1/simulate", json=payload)
    r2 = client.post("/api/v1/simulate", json=payload)
    assert r1.json() == r2.json()


def test_clusters_explicit_dyads_without_session(client):
    from api.session import clear_latest_dyad_context

    clear_latest_dyad_context()

    dyads = []
    model_ids = [f"M{i:04d}" for i in range(1, 6)]
    for ego in model_ids:
        for alter in model_ids:
            if ego != alter:
                dyads.append(
                    {
                        "dyad_id": f"{ego}__{alter}",
                        "ego_id": ego,
                        "alter_id": alter,
                        "similarity_rate": 0.8,
                    }
                )

    response = client.post(
        "/api/v1/clusters",
        json={
            "dyads": dyads,
            "model_ids": model_ids,
            "eps": 0.5,
            "min_samples": 2,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["model_count"] == 5
    assert data["data"]["degenerate_metric"] is True
    assert data["data"]["cluster_count"] == 0
    assert data["data"]["noise_count"] == 5


def test_clusters_causal_score_field_recomputes_context_dyads(client, monkeypatch):
    from dyadic.causal import CausalWrapper
    from dyadic.identification import IdentificationWrapper

    monkeypatch.setattr(
        CausalWrapper,
        "compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )
    monkeypatch.setattr(
        IdentificationWrapper,
        "identify_total_effect",
        lambda self, **kwargs: (True, "P(Y|X)"),
    )

    reg, st = _sample_registry_and_state()
    for record in st:
        if record["model_id"] == "M0002" and record["comp_id"] == "C0002":
            record["status"] = "causal"
            record["timing"] = 2
    dyad_response = client.post(
        "/api/v1/dyad-matrix",
        json={"registry_data": reg, "state_data": st},
    )
    assert dyad_response.status_code == 200

    response = client.post(
        "/api/v1/clusters",
        json={
            "score_field": "identified_compatible",
            "exposure": "X",
            "outcome": "Y",
            "min_samples": 2,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["score_field"] == "identified_compatible"
    assert data["data"]["degenerate_metric"] is True


def test_clusters_causal_score_field_rejects_dyads_without_field(client):
    dyads = [
        {
            "dyad_id": "M0001__M0002",
            "ego_id": "M0001",
            "alter_id": "M0002",
            "similarity_rate": 0.8,
        },
        {
            "dyad_id": "M0002__M0001",
            "ego_id": "M0002",
            "alter_id": "M0001",
            "similarity_rate": 0.8,
        },
    ]
    response = client.post(
        "/api/v1/clusters",
        json={
            "dyads": dyads,
            "model_ids": ["M0001", "M0002"],
            "score_field": "identified_compatible",
            "exposure": "X",
            "outcome": "Y",
            "min_samples": 2,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "MISSING_CAUSAL_SCORE_FIELD"


def test_clusters_rejects_unavailable_canonical_score(client):
    dyads = [
        {
            "dyad_id": "M0001__M0002",
            "ego_id": "M0001",
            "alter_id": "M0002",
            "identified_compatible": None,
        },
        {
            "dyad_id": "M0002__M0001",
            "ego_id": "M0002",
            "alter_id": "M0001",
            "identified_compatible": True,
        },
    ]
    response = client.post(
        "/api/v1/clusters",
        json={
            "dyads": dyads,
            "model_ids": ["M0001", "M0002"],
            "score_field": "identified_compatible",
            "min_samples": 2,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "CLUSTERING_ERROR"
    assert "identified_compatible" in data["message"]
    assert "unavailable for 1 dyad" in data["message"]


def test_clusters_requires_complete_directed_dyads(client):
    response = client.post(
        "/api/v1/clusters",
        json={
            "dyads": [
                {
                    "dyad_id": "M0001__M0002",
                    "ego_id": "M0001",
                    "alter_id": "M0002",
                    "similarity_rate": 0.5,
                }
            ],
            "model_ids": ["M0001", "M0002"],
            "min_samples": 2,
        },
    )
    assert response.status_code == 422
    data = response.json()
    assert data["code"] == "CLUSTERING_ERROR"
    assert "missing 1 directed pair" in data["message"]


# ── Symbolic API tests ─────────────────────────────────────────────────────────


def test_symbolic_universe_endpoint(client):
    response = client.post(
        "/api/v1/symbolic/universe",
        json={
            "nodes": [{"name": "X"}, {"name": "Y"}, {"name": "A"}],
            "exposure": "X",
            "outcome": "Y",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["edge_count"] >= 0
    assert data["data"]["exposure"] == "X"
    assert data["data"]["outcome"] == "Y"


def test_symbolic_query_classes_endpoint(client):
    response = client.post(
        "/api/v1/symbolic/query-classes",
        json={
            "nodes": [{"name": "X", "timing": 1}, {"name": "Y", "timing": 2}],
            "exposure": "X",
            "outcome": "Y",
            "mode": "full",
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert len(data["data"]["classes"]) >= 1


def test_symbolic_compare_endpoint(client):
    response = client.post(
        "/api/v1/symbolic/compare",
        json={
            "nodes": [{"name": "X"}, {"name": "Y"}, {"name": "A"}],
            "exposure": "X",
            "outcome": "Y",
            "theory_a": {
                "nodes": ["X", "Y", "A"],
                "edges": [["X", "Y"]],
                "exposure": "X",
                "outcome": "Y",
            },
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"


def test_symbolic_delta_u_endpoint(client):
    response = client.post(
        "/api/v1/symbolic/delta-u",
        json={
            "nodes": [{"name": "X", "timing": 1}, {"name": "Y", "timing": 2}],
            "exposure": "X",
            "outcome": "Y",
            "mode": "full",
            "top_k": 5,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"


def test_symbolic_simulate_endpoint(client):
    response = client.post(
        "/api/v1/symbolic/simulate",
        json={
            "scenario": "consensus_illusion",
            "seed": 42,
        },
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"


# --- Crux mode / global crux tests -------------------------------------------


def test_delta_u_global_crux_returns_rankings(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"crux_mode": "global", "global_status": "causal"},
    )
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["data"]["crux_mode"] == "global"
    assert "global_result" not in data["data"]
    rankings = data["data"]["rankings"]
    assert rankings
    assert len(rankings) <= 10
    result = rankings[0]
    assert result["crux_mode"] == "global"
    assert result["feasible_causal"] is True
    assert result["feasible_non_causal"] is True
    assert "baseline_compatibility" in result
    assert "post_compatibility_causal" in result
    assert "post_compatibility_non_causal" in result
    assert "models_changed_causal" in result
    assert "mapping_coverage_causal" in result
    assert "timing_pruned_models_causal" in result
    assert "timing_pruned_models_non_causal" in result
    assert "models_pruned_causal" in result
    assert "models_pruned_non_causal" in result
    assert "post_model_count_causal" in result
    assert "post_model_count_non_causal" in result
    assert "post_dyad_count_causal" in result
    assert "post_dyad_count_non_causal" in result
    assert "insufficient_post_models_causal" in result
    assert "insufficient_post_models_non_causal" in result
    assert result["direction"] == "->"


def test_delta_u_global_status_is_optional_and_legacy_value_is_ignored(client):
    _populate_dyad_context(client)
    without_status = client.post(
        "/api/v1/delta-u",
        json={"crux_mode": "global"},
    )
    with_legacy_status = client.post(
        "/api/v1/delta-u",
        json={"crux_mode": "global", "global_status": "causal"},
    )
    assert without_status.status_code == 200
    assert with_legacy_status.status_code == 200
    assert (
        without_status.json()["data"]["rankings"]
        == with_legacy_status.json()["data"]["rankings"]
    )


def test_delta_u_marginal_rejects_global_status(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"global_status": "causal"},
    )
    assert response.status_code == 422
    assert "global_status" in response.text


def test_delta_u_global_rejects_component_id(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "crux_mode": "global",
            "global_status": "causal",
            "component_id": "C0004",
        },
    )
    assert response.status_code == 422
    assert "component_id" in response.text


def test_delta_u_global_rejects_two_stage(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "crux_mode": "global",
            "global_status": "causal",
            "mode": "two-stage",
        },
    )
    assert response.status_code == 422
    assert "two-stage" in response.text


def test_delta_u_global_rejects_synergy(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={
            "crux_mode": "global",
            "global_status": "causal",
            "synergistic_set_size": 2,
        },
    )
    assert response.status_code == 422
    assert "synergistic" in response.text


def test_delta_u_rejects_old_resolution_strategy(client):
    _populate_dyad_context(client)
    response = client.post(
        "/api/v1/delta-u",
        json={"resolution_strategy": "condition"},
    )
    assert response.status_code == 422


def test_simulate_global_status_rejected_for_ghost(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "ghost_discovery",
            "n_models": 20,
            "n_components": 50,
            "global_status": "causal",
        },
    )
    assert response.status_code == 422


def test_simulate_global_crux_mode_rejected_for_non_crux_scenario(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "ghost_discovery",
            "n_models": 20,
            "n_components": 50,
            "crux_mode": "global",
            "global_status": "causal",
        },
    )
    assert response.status_code == 422
    assert "only apply" in response.text
    assert "global_status" in response.text


def test_simulate_crux_global_requires_status(client):
    response = client.post(
        "/api/v1/simulate",
        json={
            "scenario": "crux_of_certainty",
            "n_models": 20,
            "n_components": 50,
            "crux_mode": "global",
        },
    )
    assert response.status_code == 422
    assert "global_status" in response.text

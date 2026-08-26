from __future__ import annotations

import pytest

from dyadic.causal import CausalError, CausalWrapper
from dyadic.identification import IdentificationError, IdentificationWrapper
from registry.builder import ComponentRegistryBuilder
from registry.loader import RegistryLoader
from simulation.suite import SimulationInputError, SimulationSuite
from symbolic.dsep import candidate_adjustment_sets
from symbolic.universe import build_symbolic_universe


def _mock_mas(monkeypatch):
    def adjustment_sets(self, dag_spec):
        edges = set(map(tuple, dag_spec["edges"]))
        active = [f"X{i}" for i in range(2, 7) if (f"X{i}", "X1") in edges]
        return [active]

    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets", adjustment_sets
    )


def _mock_identification(monkeypatch, calls):
    monkeypatch.setattr(
        "dyadic.causal.CausalWrapper.compute_adjustment_sets",
        lambda self, dag_spec: [[]],
    )

    def identify(self, **kwargs):
        calls.append(kwargs)
        edges = set(map(tuple, kwargs["directed_edges"]))
        projected = {tuple(sorted(edge)) for edge in kwargs["bidirected_edges"]}
        assert "U" not in kwargs["nodes"]
        assert ("X1", "Y") in projected
        identified = ("X1", "Y") not in edges
        return identified, "front-door" if identified else None

    monkeypatch.setattr(
        "dyadic.identification.IdentificationWrapper.identify_total_effect", identify
    )


def test_mas_consensus_illusion_is_deterministic_and_completion_closed(monkeypatch):
    _mock_mas(monkeypatch)
    result = SimulationSuite(42).run_scenario(
        "consensus_illusion",
        compatibility_metric="mas_compatible",
        include_plot_data=True,
    )

    assert result["n_models"] == 192
    assert result["results"]["resolved_model_count"] == 128
    assert result["results"]["partial_model_count"] == 64
    assert result["results"]["n_unavailable_dyads"] == 0
    assert (
        result["results"]["mean_similarity_rate"]
        > result["results"]["compatibility_rate"]
    )
    assert result["results"]["consensus_illusion_gap"] > 0
    assert result["results"]["exposure"] == "X1"
    assert result["results"]["outcome"] == "Y"

    registry = result["artifacts"]["registry_data"]
    assert {row["source"] for row in registry if row["type"] == "node"} == {
        "X1",
        "X2",
        "X3",
        "X4",
        "X5",
        "X6",
        "X7",
        "X8",
        "Y",
    }
    assert all(row["direction"] != "<->" for row in registry)
    assert all(
        "visibility" not in row and "illusion_role" not in row for row in registry
    )
    edges = {
        (row["source"], row["target"]): row["comp_id"]
        for row in registry
        if row["type"] == "edge"
    }
    expected_fixed = {
        ("X1", "X7"),
        ("X7", "X8"),
        ("X8", "Y"),
        ("X1", "Y"),
        *((f"X{i}", "Y") for i in range(2, 7)),
    }
    expected_variable = {
        ("X6", "X1"),
        *((f"X{i}", "X1") for i in range(2, 6)),
        ("X2", "X7"),
        ("X3", "X7"),
    }
    assert set(edges) == expected_fixed | expected_variable

    statuses = {}
    for row in result["artifacts"]["state_data"]:
        statuses.setdefault(row["comp_id"], set()).add(row["status"])
    assert all(statuses[edges[edge]] == {"causal"} for edge in expected_fixed)
    assert statuses[edges[("X6", "X1")]] == {
        "causal",
        "non-causal",
        "unknown",
    }
    assert all(
        statuses[edges[edge]] == {"causal", "non-causal"}
        for edge in expected_variable - {("X6", "X1")}
    )

    forbidden_aliases = {
        "baseline_similarity_rate",
        "mean_selected_compatibility",
        "selected_compatibility_rate",
    }
    assert not forbidden_aliases.intersection(result["results"])
    model_metric = result["artifacts"]["plot_data"]["model_metrics"][0]
    assert set(model_metric) == {
        "model_id",
        "mean_similarity_rate",
        "compatibility_rate",
        "consensus_illusion_gap",
        "compatibility_metric",
    }


def test_identification_consensus_illusion_uses_forced_conditioning_collider(
    monkeypatch,
):
    calls = []
    _mock_identification(monkeypatch, calls)
    result = SimulationSuite(7).run_scenario(
        "consensus_illusion", compatibility_metric="identified_compatible"
    )

    assert result["n_models"] == 192
    assert result["results"]["resolved_model_count"] == 128
    assert result["results"]["partial_model_count"] == 64
    assert (
        result["results"]["mean_similarity_rate"]
        > result["results"]["compatibility_rate"]
    )
    # identified_compatible uses native complete-conditioning d-separation;
    # the legacy general-ID wrapper is intentionally not called.
    assert calls == []

    registry = result["artifacts"]["registry_data"]
    assert all(row["direction"] != "<->" for row in registry)
    edges = {
        (row["source"], row["target"]) for row in registry if row["type"] == "edge"
    }
    assert edges == {
        ("X2", "X1"),
        ("X2", "Y"),
        ("X1", "Y"),
        ("Y", "X6"),
        ("X1", "X6"),
        ("X3", "X4"),
        ("X3", "X5"),
        ("X4", "X5"),
        ("X3", "Y"),
        ("X4", "Y"),
        ("X5", "Y"),
    }


def test_forced_conditioning_design_uses_requested_timings():
    suite = SimulationSuite(42)
    _, _, _, _, node_timing, design = suite._build_consensus_illusion_design(
        "identified_compatible"
    )

    assert design == "forced_conditioning_collider"
    assert node_timing == {
        "X2": 1,
        "X1": 2,
        "X3": 3,
        "X4": 4,
        "X5": 5,
        "Y": 6,
        "X6": 7,
    }


def test_simulations_reject_bidirected_generation_and_seed_data():
    suite = SimulationSuite(42)
    with pytest.raises(SimulationInputError, match="directed components only"):
        suite.run_scenario(
            "ghost_discovery", include_bidirectional=True, enforce_thresholds=False
        )

    registry = [
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
            "direction": "<->",
            "description": "X confounded with Y",
        },
    ]
    with pytest.raises(SimulationInputError, match="bidirected component"):
        suite.run_scenario(
            "consensus_illusion",
            registry_data=registry,
            state_data=[
                {"model_id": "M1", "comp_id": "C0001", "status": "present"},
                {"model_id": "M2", "comp_id": "C0001", "status": "present"},
            ],
            compatibility_metric="identified_compatible",
            exposure="X",
            outcome="Y",
        )


def test_observed_defaults_true_and_latent_nodes_are_not_adjustment_candidates():
    registry = ComponentRegistryBuilder.from_nodes(
        [
            {"name": "U", "observed": False},
            {"name": "X"},
            {"name": "Y"},
        ],
        respect_timing=False,
    )
    rows = registry.data[registry.data["type"] == "node"].set_index("source")
    assert bool(rows.loc["U", "observed"]) is False
    assert bool(rows.loc["X", "observed"]) is True
    assert bool(rows.loc["Y", "observed"]) is True

    loaded = RegistryLoader.from_records(registry.data.to_dict(orient="records"))
    universe = build_symbolic_universe(loaded.data, exposure="X", outcome="Y")
    assert all(
        "U" not in adjustment for adjustment in candidate_adjustment_sets(universe)
    )


def test_consensus_illusion_rejects_similarity_as_selected_metric():
    with pytest.raises(SimulationInputError, match="mas_compatible"):
        SimulationSuite().run_scenario("consensus_illusion")


def test_real_dagitty_never_returns_latent_adjustment_candidates():
    wrapper = CausalWrapper()
    dag_spec = {
        "nodes": ["U", "X", "Y"],
        "observed_nodes": ["X", "Y"],
        "latent_nodes": ["U"],
        "edges": [("U", "X"), ("U", "Y"), ("X", "Y")],
        "exposure": "X",
        "outcome": "Y",
    }
    try:
        adjustment_sets = wrapper.compute_adjustment_sets(dag_spec)
    except CausalError as error:
        pytest.skip(str(error))
    assert all("U" not in adjustment for adjustment in adjustment_sets)


def test_real_general_identification_distinguishes_front_door_completion():
    wrapper = IdentificationWrapper()
    common = {
        "nodes": ["X1", "X2", "Y"],
        "bidirected_edges": [("X1", "Y")],
        "exposure": "X1",
        "outcome": "Y",
    }
    try:
        front_door, _ = wrapper.identify_total_effect(
            directed_edges=[("X1", "X2"), ("X2", "Y")], **common
        )
        direct_edge, _ = wrapper.identify_total_effect(
            directed_edges=[("X1", "X2"), ("X2", "Y"), ("X1", "Y")],
            **common,
        )
    except IdentificationError as error:
        pytest.skip(str(error))
    assert front_door is True
    assert direct_edge is False

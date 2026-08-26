from __future__ import annotations

import pytest

from dyadic.causal import native_complete_conditioning_identified
from dyadic.engine import DyadicEngine, DyadicError
from dyadic.profiles import (
    CausalQueryProfile,
    identified_compatible,
    identification_nodes_from_dag_spec,
)
from registry.builder import ComponentRegistryBuilder
from state.tensor import StateTensor


class _AdjustmentOnlyWrapper:
    def compute_adjustment_sets(self, _dag_spec):
        return [[]]


class _FailingIdentificationWrapper:
    def identify_total_effect(self, **_kwargs):
        raise AssertionError("legacy general-ID wrapper must not be called")


def _profile(model_id, identified, nodes):
    return CausalQueryProfile(
        model_id=model_id,
        mas=[[]],
        robust_mas=frozenset({frozenset()}),
        identified=identified,
        identification_formula=None,
        is_resolved=True,
        completion_ids=(model_id,),
        completion_count=1,
        expected_completion_count=1,
        completion_coverage_complete=True,
        identification_nodes=nodes,
    )


def test_complete_conditioning_removes_only_direct_edge():
    assert (
        native_complete_conditioning_identified(
            {
                "nodes": ["X", "M", "Y"],
                "edges": [("X", "Y"), ("X", "M"), ("M", "Y")],
                "exposure": "X",
                "outcome": "Y",
            },
            {"M"},
        )
        is True
    )


@pytest.mark.parametrize(
    ("edges", "conditioning", "expected"),
    [
        ([("X", "Y"), ("C", "X"), ("C", "Y")], {"C"}, True),
        ([("X", "Y"), ("X", "C"), ("Y", "C")], {"C"}, False),
    ],
)
def test_complete_conditioning_respects_confounders_and_colliders(
    edges, conditioning, expected
):
    assert (
        native_complete_conditioning_identified(
            {
                "nodes": ["X", "C", "Y"],
                "edges": edges,
                "exposure": "X",
                "outcome": "Y",
            },
            conditioning,
        )
        is expected
    )


def test_complete_conditioning_keeps_bidirected_path():
    assert (
        native_complete_conditioning_identified(
            {
                "nodes": ["X", "Y"],
                "edges": [("X", "Y")],
                "bidirected_edges": [("X", "Y")],
                "exposure": "X",
                "outcome": "Y",
            },
            set(),
        )
        is False
    )


def test_identification_nodes_are_presence_based():
    assert identification_nodes_from_dag_spec(
        {
            "declared_nodes": ["X", "M", "C", "Y"],
            "declared_directed_edges": [("X", "M"), ("M", "Y")],
            "exposure": "X",
            "outcome": "Y",
        }
    ) == frozenset({"M", "C"})


def test_pair_metric_requires_exact_node_set_after_identification():
    assert (
        identified_compatible(
            _profile("A", True, frozenset({"M"})),
            _profile("B", True, frozenset({"M"})),
        )
        is True
    )
    assert (
        identified_compatible(
            _profile("A", True, frozenset({"M"})),
            _profile("B", True, frozenset({"C"})),
        )
        is False
    )
    assert (
        identified_compatible(
            _profile("A", False, frozenset({"M"})),
            _profile("B", False, frozenset({"C"})),
        )
        is False
    )


def test_resolved_profile_uses_native_predicate_without_legacy_wrapper():
    registry = ComponentRegistryBuilder.from_nodes(
        [
            {"name": "X", "timing": 1},
            {"name": "M", "timing": 2},
            {"name": "Y", "timing": 3},
        ],
        exposure="X",
        outcome="Y",
    )
    records = []
    for _, row in registry.data.iterrows():
        if row["type"] == "node":
            status = "present"
            timing = {"X": 1, "M": 2, "Y": 3}[row["source"]]
        else:
            status = (
                "causal"
                if (row["source"], row["target"])
                in {
                    ("X", "Y"),
                    ("X", "M"),
                    ("M", "Y"),
                }
                else "non-causal"
            )
            timing = None
        records.append(
            {
                "model_id": "M1",
                "comp_id": row["comp_id"],
                "status": status,
                "timing": timing,
            }
        )
    state = StateTensor.from_records(registry, records)

    profiles = DyadicEngine()._build_causal_profiles(
        state,
        registry,
        mode="full",
        causal_wrapper=_AdjustmentOnlyWrapper(),
        identification_wrapper=_FailingIdentificationWrapper(),
        exposure="X",
        outcome="Y",
    )
    profile = profiles["M1"]
    assert profile.identified is True
    assert profile.identification_nodes == frozenset({"M"})
    assert profile.identification_method == "complete_conditioning_dsep"


def test_causal_query_requires_fixed_direct_edge_but_basic_does_not():
    registry = ComponentRegistryBuilder.from_nodes(
        [{"name": "X", "timing": 1}, {"name": "Y", "timing": 2}],
        exposure="X",
        outcome="Y",
    )
    direct = registry.data[
        (registry.data["type"] == "edge")
        & (registry.data["source"] == "X")
        & (registry.data["target"] == "Y")
    ].index[0]
    registry.data.loc[direct, "fixed_status"] = None
    records = []
    for _, row in registry.data.iterrows():
        records.append(
            {
                "model_id": "M1",
                "comp_id": row["comp_id"],
                "status": "present" if row["type"] == "node" else "causal",
                "timing": (
                    1 if row["source"] == "X" else 2 if row["source"] == "Y" else None
                ),
            }
        )
    state = StateTensor.from_records(registry, records)
    with pytest.raises(DyadicError, match="fixed_status='causal'"):
        DyadicEngine().validate_causal_query(state, registry, "X", "Y")

    dyads = DyadicEngine().compare_pairs(
        state,
        registry,
        mode="basic",
        exposure="X",
        outcome="Y",
    )
    assert dyads == []

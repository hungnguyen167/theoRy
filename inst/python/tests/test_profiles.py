from __future__ import annotations

from unittest.mock import Mock

import pytest

from dyadic.profiles import (
    CausalProfileBuilder,
    CausalQueryProfile,
    _directed_path_intermediates,
    identification_nodes_from_dag_spec,
    identified_compatible,
    mas_compatible,
)
from registry.loader import RegistryLoader
from state.completions import CompletionIndex
from state.tensor import StateTensor


def _registry(edges, *, fixed=()):
    nodes = sorted({name for edge in edges for name in edge[:2]})
    records = [
        {
            "comp_id": f"C{i + 1:04d}",
            "type": "node",
            "source": name,
            "target": None,
            "direction": None,
            "description": name,
        }
        for i, name in enumerate(nodes)
    ]
    edge_ids = {}
    for i, (source, target, direction) in enumerate(edges, start=len(nodes) + 1):
        comp_id = f"C{i:04d}"
        edge_ids[(source, target)] = comp_id
        record = {
            "comp_id": comp_id,
            "type": "edge",
            "source": source,
            "target": target,
            "direction": direction,
            "description": f"{source}{direction}{target}",
        }
        if (source, target) in fixed:
            record["fixed_status"] = "causal"
        records.append(record)
    return RegistryLoader.from_records(records), edge_ids


def _state(registry, models, timing=None):
    timing = timing or {}
    node_rows = registry.data[registry.data["type"] == "node"]
    edge_rows = registry.data[registry.data["type"] == "edge"]
    records = []
    for model_id, statuses in models.items():
        for _, row in node_rows.iterrows():
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": row["comp_id"],
                    "status": "present",
                    "timing": timing.get(row["source"]),
                }
            )
        for _, row in edge_rows.iterrows():
            records.append(
                {
                    "model_id": model_id,
                    "comp_id": row["comp_id"],
                    "status": statuses[(row["source"], row["target"])],
                }
            )
    return StateTensor.from_records(registry, records)


def _dag_builder(state, registry):
    def build(model_id, *, exposure, outcome):
        node_rows = registry.data[registry.data["type"] == "node"]
        present_nodes = [
            row["source"]
            for _, row in node_rows.iterrows()
            if state.get_status(model_id, row["comp_id"]) in ("present", "causal")
        ]
        present_set = set(present_nodes)
        nodes = list(dict.fromkeys(present_nodes))
        edges = []
        bidirected = []
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows():
            if state.get_status(model_id, row["comp_id"]) != "causal":
                continue
            if row["source"] not in present_set or row["target"] not in present_set:
                continue
            target = edges if row["direction"] == "->" else bidirected
            target.append((row["source"], row["target"]))
        return {
            "nodes": nodes,
            "edges": edges,
            "bidirected_edges": bidirected,
            "declared_nodes": list(nodes),
            "declared_directed_edges": list(edges),
            "exposure": exposure,
            "outcome": outcome,
            "query_nodes_missing": exposure not in nodes or outcome not in nodes,
        }

    return build


def _builder(state, registry, *, mas=None, identified=None, dag_builder=None):
    causal = Mock()
    causal.compute_adjustment_sets.side_effect = mas or (lambda spec: [[]])
    identification = Mock()
    identification.identify_total_effect.side_effect = identified or (
        lambda **kwargs: (True, "formula")
    )
    builder = CausalProfileBuilder(
        state,
        registry,
        dag_builder or _dag_builder(state, registry),
        causal,
        identification,
    )
    return builder, causal, identification


def test_one_unknown_edge_finds_both_unique_resolved_descendants():
    registry, _ = _registry([("X", "Y", "->")])
    state = _state(
        registry,
        {
            "partial": {("X", "Y"): "unknown"},
            "causal": {("X", "Y"): "causal"},
            "causal_duplicate": {("X", "Y"): "causal"},
            "noncausal": {("X", "Y"): "non-causal"},
        },
    )
    index = CompletionIndex(state, registry)

    diagnostics = index.diagnostics("partial")
    assert set(index.resolved_descendants("partial")) == {"causal", "noncausal"}
    assert diagnostics.completion_count == 2
    assert diagnostics.expected_completion_count == 2
    assert diagnostics.completion_coverage_complete is True
    assert diagnostics.duplicate_resolved_state_count == 1


def test_two_unknown_edges_require_all_four_combinations():
    registry, _ = _registry([("X", "Z", "->"), ("Z", "Y", "->")])
    values = ("causal", "non-causal")
    models = {"partial": {("X", "Z"): "unknown", ("Z", "Y"): "unknown"}}
    for i, first in enumerate(values):
        for j, second in enumerate(values):
            models[f"resolved_{i}_{j}"] = {
                ("X", "Z"): first,
                ("Z", "Y"): second,
            }
    state = _state(registry, models)
    diagnostics = CompletionIndex(state, registry).diagnostics("partial")
    assert diagnostics.completion_count == 4
    assert diagnostics.expected_completion_count == 4
    assert diagnostics.completion_coverage_complete is True


def test_invalid_cyclic_completion_is_not_expected_or_sent_to_wrappers():
    registry, _ = _registry([("X", "Y", "->"), ("Y", "X", "->")])
    state = _state(
        registry,
        {
            "partial": {("X", "Y"): "unknown", ("Y", "X"): "causal"},
            "valid": {("X", "Y"): "non-causal", ("Y", "X"): "causal"},
            "cyclic": {("X", "Y"): "causal", ("Y", "X"): "causal"},
        },
    )
    index = CompletionIndex(state, registry)
    diagnostics = index.diagnostics("partial")
    assert diagnostics.expected_completion_count == 1
    assert diagnostics.completion_ids == ("valid",)
    assert diagnostics.completion_coverage_complete is True

    builder, causal, identification = _builder(state, registry)
    profile = builder.build("partial", "X", "Y")
    assert profile.identified is False
    assert causal.compute_adjustment_sets.call_count == 1
    assert identification.identify_total_effect.call_count == 0


def test_temporal_and_fixed_causal_constraints_filter_expected_signatures():
    registry, _ = _registry([("X", "Y", "->"), ("Y", "Z", "->")], fixed={("Y", "Z")})
    state = _state(
        registry,
        {
            "partial": {("X", "Y"): "unknown", ("Y", "Z"): "unknown"},
            "valid": {("X", "Y"): "non-causal", ("Y", "Z"): "causal"},
        },
        timing={"X": 3, "Y": 2, "Z": 4},
    )
    diagnostics = CompletionIndex(state, registry).diagnostics("partial")
    assert diagnostics.expected_completion_count == 1
    assert diagnostics.completion_ids == ("valid",)
    assert diagnostics.completion_coverage_complete is True


def test_signature_includes_exact_nodes_timing_and_constraints():
    registry, _ = _registry([("X", "Y", "->")])
    state = _state(
        registry,
        {
            "partial": {("X", "Y"): "unknown"},
            "wrong_constraint": {("X", "Y"): "causal"},
            "right": {("X", "Y"): "non-causal"},
        },
        timing={"X": 1, "Y": 2},
    )
    constraints = {
        "partial": {"cohort": "A"},
        "wrong_constraint": {"cohort": "B"},
        "right": {"cohort": "A"},
    }
    diagnostics = CompletionIndex(
        state, registry, model_constraints=constraints
    ).diagnostics("partial")
    assert diagnostics.completion_ids == ("right",)
    assert diagnostics.completion_coverage_complete is False


def test_signature_rejects_descendants_with_different_node_presence_or_timing():
    registry, edge_ids = _registry([("X", "Y", "->")])
    node_ids = {
        row["source"]: row["comp_id"]
        for _, row in registry.data[registry.data["type"] == "node"].iterrows()
    }
    edge_id = edge_ids[("X", "Y")]
    records = [
        {
            "model_id": "partial",
            "comp_id": node_ids["X"],
            "status": "present",
            "timing": 1,
        },
        {
            "model_id": "partial",
            "comp_id": node_ids["Y"],
            "status": "present",
            "timing": 2,
        },
        {"model_id": "partial", "comp_id": edge_id, "status": "unknown"},
        {
            "model_id": "wrong_timing",
            "comp_id": node_ids["X"],
            "status": "present",
            "timing": 1,
        },
        {
            "model_id": "wrong_timing",
            "comp_id": node_ids["Y"],
            "status": "present",
            "timing": 3,
        },
        {"model_id": "wrong_timing", "comp_id": edge_id, "status": "causal"},
        {
            "model_id": "missing_node",
            "comp_id": node_ids["X"],
            "status": "present",
            "timing": 1,
        },
    ]
    state = StateTensor.from_records(
        registry, records, model_ids=["partial", "wrong_timing", "missing_node"]
    )
    diagnostics = CompletionIndex(state, registry).diagnostics("partial")
    assert diagnostics.completion_ids == ()
    assert diagnostics.expected_completion_count == 2
    assert diagnostics.completion_coverage_complete is False


def test_incomplete_with_represented_false_is_false():
    registry, _ = _registry([("X", "Z", "->"), ("Z", "Y", "->")])
    models = {
        "partial": {("X", "Z"): "unknown", ("Z", "Y"): "unknown"},
        "one": {("X", "Z"): "causal", ("Z", "Y"): "causal"},
        "two": {("X", "Z"): "causal", ("Z", "Y"): "non-causal"},
    }
    state = _state(registry, models)

    all_true, _, _ = _builder(state, registry)
    profile = all_true.build("partial", "X", "Y")
    assert profile.completion_count == 2
    assert profile.expected_completion_count == 4
    assert profile.completion_coverage_complete is False
    assert profile.identified is False
    assert profile.robust_mas is None

    def identify(**kwargs):
        return (kwargs["directed_edges"] != [("X", "Z")], None)

    with_false, _, _ = _builder(state, registry, identified=identify)
    assert with_false.build("partial", "X", "Y").identified is False


def test_robust_mas_intersection_preserves_valid_empty_set():
    registry, _ = _registry([("X", "Y", "->")])
    state = _state(
        registry,
        {
            "partial": {("X", "Y"): "unknown"},
            "causal": {("X", "Y"): "causal"},
            "noncausal": {("X", "Y"): "non-causal"},
        },
    )

    def mas(spec):
        return [[], ["Z"]] if spec["edges"] else [[], ["W"]]

    builder, _, _ = _builder(state, registry, mas=mas)
    profile = builder.build("partial", "X", "Y")
    assert profile.robust_mas == frozenset({frozenset()})
    assert profile.mas == [[]]

    no_set_builder, _, _ = _builder(
        state, registry, mas=lambda spec: [] if spec["edges"] else [[]]
    )
    assert no_set_builder.build("partial", "X", "Y").robust_mas == frozenset()


def test_query_missing_is_unavailable_and_wrappers_are_not_called():
    registry, _ = _registry([("X", "Y", "->")])
    state = _state(registry, {"resolved": {("X", "Y"): "causal"}})
    dag_builder = Mock(
        return_value={
            "nodes": ["X", "Y"],
            "edges": [("X", "Y")],
            "bidirected_edges": [],
            "exposure": "X",
            "outcome": "Z",
            "query_nodes_missing": True,
        }
    )
    builder, causal, identification = _builder(state, registry, dag_builder=dag_builder)
    profile = builder.build("resolved", "X", "Z")
    assert profile.mas is None
    assert profile.robust_mas is None
    assert profile.identified is None
    causal.compute_adjustment_sets.assert_not_called()
    identification.identify_total_effect.assert_not_called()


def _profile(identified, robust=frozenset({frozenset()}), nodes=frozenset({"X", "Y"})):
    return CausalQueryProfile(
        model_id="M",
        mas=[[]] if robust is not None else None,
        robust_mas=robust,
        identified=identified,
        identification_formula=None,
        is_resolved=True,
        completion_ids=("M",),
        completion_count=1,
        expected_completion_count=1,
        completion_coverage_complete=True,
        identification_nodes=nodes,
    )


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        (True, True, True),
        (True, False, False),
        (False, True, False),
        (False, False, False),
        (None, True, None),
        (False, None, None),
    ],
)
def test_identified_compatibility_truth_table(left, right, expected):
    assert identified_compatible(_profile(left), _profile(right)) is expected


def test_mas_compatibility_truth_table():
    empty = frozenset({frozenset()})
    z = frozenset({frozenset({"Z"})})
    no_set = frozenset()
    assert mas_compatible(_profile(True, empty), _profile(True, empty)) is True
    assert mas_compatible(_profile(True, empty), _profile(True, z)) is False
    assert mas_compatible(_profile(True, no_set), _profile(True, no_set)) is False
    assert mas_compatible(_profile(True, None), _profile(True, empty)) is None


def test_profile_comparison_exposes_identification_nodes():
    left = _profile(True, nodes=frozenset({"X", "Y", "Z"}))
    right = _profile(True, nodes=frozenset({"W", "X", "Y"}))

    result = CausalProfileBuilder.compare(left, right)

    assert result["identification_nodes_ego"] == ["X", "Y", "Z"]
    assert result["identification_nodes_alter"] == ["W", "X", "Y"]


def test_cache_uses_semantic_signature_and_query():
    registry, _ = _registry([("X", "Y", "->")])
    state = _state(
        registry,
        {
            "first": {("X", "Y"): "causal"},
            "duplicate": {("X", "Y"): "causal"},
        },
    )
    builder, causal, identification = _builder(state, registry)
    first = builder.build("first", "X", "Y")
    duplicate = builder.build("duplicate", "X", "Y")
    assert first.model_id == "first"
    assert duplicate.model_id == "duplicate"
    assert first.completion_ids == ("first",)
    assert duplicate.completion_ids == ("duplicate",)
    assert causal.compute_adjustment_sets.call_count == 1
    assert identification.identify_total_effect.call_count == 0

    builder.build("first", "Y", "X")
    assert causal.compute_adjustment_sets.call_count == 2
    assert identification.identify_total_effect.call_count == 0


# --------------------------------------------------------------------------
# New identified-compatibility contract: exact declared conditioning-node equality
# --------------------------------------------------------------------------


def _dag_spec(nodes, edges, exposure="X", outcome="Y", bidirected=None, missing=False):
    return {
        "nodes": list(nodes),
        "edges": list(edges),
        "bidirected_edges": list(bidirected or []),
        "declared_nodes": list(nodes),
        "declared_directed_edges": list(edges),
        "exposure": exposure,
        "outcome": outcome,
        "query_nodes_missing": missing,
    }


def test_same_relevant_nodes_both_identified_is_true():
    a = _profile(True, nodes=frozenset({"X", "Y", "Z"}))
    b = _profile(True, nodes=frozenset({"X", "Y", "Z"}))
    assert identified_compatible(a, b) is True


def test_extra_non_intermediate_node_is_false():
    a = _profile(True, nodes=frozenset({"X", "Y", "Z"}))
    b = _profile(True, nodes=frozenset({"X", "Y", "Z", "W"}))
    assert identified_compatible(a, b) is False


def test_extra_directed_path_intermediate_normalizes_to_true():
    # Complete conditioning includes every declared node except X and Y.
    a = _profile(True, nodes=frozenset({"X", "Y"}))
    b_nodes = identification_nodes_from_dag_spec(
        _dag_spec(["X", "M", "Y"], [("X", "M"), ("M", "Y")])
    )
    assert b_nodes == frozenset({"M"})
    b = _profile(True, nodes=b_nodes)
    assert identified_compatible(a, b) is False


def test_node_timed_between_but_off_directed_path_remains_relevant():
    # W is temporally between X and Y but not on a directed path; it remains in Z.
    nodes = identification_nodes_from_dag_spec(_dag_spec(["X", "W", "Y"], [("X", "Y")]))
    assert nodes == frozenset({"W"})
    a = _profile(True, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=nodes)
    assert identified_compatible(a, b) is False


def test_pre_exposure_node_remains_relevant():
    # B is a pre-exposure cause of X; it remains in Z.
    nodes = identification_nodes_from_dag_spec(
        _dag_spec(["B", "X", "Y"], [("B", "X"), ("X", "Y")])
    )
    assert nodes == frozenset({"B"})
    a = _profile(True, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=nodes)
    assert identified_compatible(a, b) is False


def test_latent_declared_node_remains_relevant_unless_on_directed_path():
    # Latent L is a common cause and remains in Z.
    nodes_off = identification_nodes_from_dag_spec(
        _dag_spec(["L", "X", "Y"], [("L", "X"), ("L", "Y")])
    )
    assert nodes_off == frozenset({"L"})
    # A node on the directed path is still included in complete conditioning.
    nodes_on = identification_nodes_from_dag_spec(
        _dag_spec(["X", "M", "Y"], [("X", "M"), ("M", "Y")])
    )
    assert nodes_on == frozenset({"M"})


def test_bidirected_relation_does_not_make_intermediate():
    # W is bidirectionally associated with X and remains in Z.
    nodes = identification_nodes_from_dag_spec(
        _dag_spec(
            ["X", "W", "Y"],
            [("X", "Y")],
            bidirected=[("X", "W")],
        )
    )
    assert nodes == frozenset({"W"})
    a = _profile(True, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=nodes)
    assert identified_compatible(a, b) is False


def test_non_identified_returns_false_even_with_matching_nodes():
    a = _profile(False, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=frozenset({"X", "Y"}))
    assert identified_compatible(a, b) is False


def test_unavailable_identification_remains_unavailable():
    a = _profile(None, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=frozenset({"X", "Y"}))
    assert identified_compatible(a, b) is None


def test_missing_relevant_node_metadata_is_unavailable_when_identified():
    a = _profile(True, nodes=frozenset({"X", "Y"}))
    b = _profile(True, nodes=None)
    assert identified_compatible(a, b) is None


def test_directed_path_intermediates_helper_excludes_endpoints():
    inter = _directed_path_intermediates(
        ["X", "M1", "M2", "Y", "Z"],
        [("X", "M1"), ("M1", "M2"), ("M2", "Y"), ("Z", "Y")],
        "X",
        "Y",
    )
    assert inter == frozenset({"M1", "M2"})


def test_partial_excludes_node_intermediate_in_every_completion():
    # Z contains every declared present node other than exposure/outcome.
    registry, _ = _registry(
        [
            ("X", "M", "->"),
            ("M", "Y", "->"),
            ("X", "Z", "->"),
            ("Z", "Y", "->"),
        ],
        fixed={("X", "M"), ("M", "Y")},
    )
    combos = []
    for xz in ("causal", "non-causal"):
        for zy in ("causal", "non-causal"):
            combos.append(
                {
                    ("X", "M"): "causal",
                    ("M", "Y"): "causal",
                    ("X", "Z"): xz,
                    ("Z", "Y"): zy,
                }
            )
    models = {
        "partial": {
            ("X", "M"): "causal",
            ("M", "Y"): "causal",
            ("X", "Z"): "unknown",
            ("Z", "Y"): "unknown",
        }
    }
    for i, combo in enumerate(combos):
        models[f"r{i}"] = combo
    state = _state(registry, models)
    builder, _, _ = _builder(state, registry)
    profile = builder.build("partial", "X", "Y")
    assert profile.completion_coverage_complete is True
    assert profile.identification_nodes is not None
    assert profile.identification_nodes == frozenset({"M", "Z"})


def test_partial_retains_node_intermediate_in_only_one_completion():
    # Z contains every declared present node other than exposure/outcome,
    # independent of which completion edges are causal.
    registry, _ = _registry(
        [
            ("X", "M", "->"),
            ("M", "Y", "->"),
            ("X", "Z", "->"),
            ("Z", "Y", "->"),
        ],
        fixed={("X", "Z"), ("Z", "Y")},
    )
    combos = []
    for xm in ("causal", "non-causal"):
        for my in ("causal", "non-causal"):
            combos.append(
                {
                    ("X", "Z"): "causal",
                    ("Z", "Y"): "causal",
                    ("X", "M"): xm,
                    ("M", "Y"): my,
                }
            )
    models = {
        "partial": {
            ("X", "Z"): "causal",
            ("Z", "Y"): "causal",
            ("X", "M"): "unknown",
            ("M", "Y"): "unknown",
        }
    }
    for i, combo in enumerate(combos):
        models[f"r{i}"] = combo
    state = _state(registry, models)
    builder, _, _ = _builder(state, registry)
    profile = builder.build("partial", "X", "Y")
    assert profile.completion_coverage_complete is True
    assert profile.identification_nodes is not None
    assert profile.identification_nodes == frozenset({"M", "Z"})


def test_partial_relevant_node_unavailable_when_coverage_incomplete():
    registry, _ = _registry([("X", "M", "->"), ("M", "Y", "->")])
    models = {
        "partial": {("X", "M"): "unknown", ("M", "Y"): "unknown"},
        "cc": {("X", "M"): "causal", ("M", "Y"): "causal"},
        # Missing the other three valid completions -> incomplete coverage.
    }
    state = _state(registry, models)
    builder, _, _ = _builder(state, registry)
    profile = builder.build("partial", "X", "Y")
    assert profile.completion_coverage_complete is False
    assert profile.identification_nodes is None

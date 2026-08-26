from __future__ import annotations

import pandas as pd
import pytest

from registry.loader import RegistryLoader
from registry.schema import ComponentRegistry, ComponentSchema, RegistryError
from registry.seed import generate_seed_data
from registry.builder import ComponentRegistryBuilder


def _make_df(records):
    return pd.DataFrame(records)


def _write_parquet(df, path):
    df.to_parquet(path, index=False)


class TestComponentSchema:
    def test_valid_node(self):
        comp = ComponentSchema(
            comp_id="C0001",
            type="node",
            source="X",
            target=None,
            direction=None,
            description="Test node",
        )
        assert comp.comp_id == "C0001"

    def test_valid_edge_directed(self):
        comp = ComponentSchema(
            comp_id="C0002",
            type="edge",
            source="X",
            target="Y",
            direction="->",
            description="Test directed edge",
        )
        assert comp.direction == "->"

    def test_valid_edge_bidirectional(self):
        comp = ComponentSchema(
            comp_id="C0003",
            type="edge",
            source="X",
            target="Y",
            direction="<->",
            description="Test bidirectional edge",
        )
        assert comp.direction == "<->"

    def test_invalid_comp_id(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="bad",
                type="node",
                source="X",
                target=None,
                direction=None,
                description="Bad ID",
            )

    def test_node_with_target_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0004",
                type="node",
                source="X",
                target="Y",
                direction=None,
                description="Node with target",
            )

    def test_node_with_direction_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0005",
                type="node",
                source="X",
                target=None,
                direction="->",
                description="Node with direction",
            )

    def test_edge_without_target_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0006",
                type="edge",
                source="X",
                target=None,
                direction="->",
                description="Edge no target",
            )

    def test_edge_without_direction_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0007",
                type="edge",
                source="X",
                target="Y",
                direction=None,
                description="Edge no direction",
            )

    def test_empty_source_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0008",
                type="node",
                source="",
                target=None,
                direction=None,
                description="Empty source",
            )

    def test_empty_description_fails(self):
        with pytest.raises(Exception):
            ComponentSchema(
                comp_id="C0009",
                type="node",
                source="X",
                target=None,
                direction=None,
                description="",
            )


class TestComponentRegistry:
    def test_valid_registry(self):
        df = _make_df(
            [
                {
                    "comp_id": "C0001",
                    "type": "node",
                    "source": "X",
                    "target": None,
                    "direction": None,
                    "description": "Node X",
                },
                {
                    "comp_id": "C0002",
                    "type": "edge",
                    "source": "X",
                    "target": "Y",
                    "direction": "->",
                    "description": "X->Y",
                },
            ]
        )
        reg = ComponentRegistry(df)
        assert len(reg.data) == 2

    def test_missing_column_raises_error(self):
        df = pd.DataFrame([{"comp_id": "C0001", "type": "node"}])
        with pytest.raises(RegistryError):
            ComponentRegistry(df)

    def test_summary_counts(self):
        df = _make_df(
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
                    "type": "edge",
                    "source": "A",
                    "target": "B",
                    "direction": "->",
                    "description": "A->B",
                },
                {
                    "comp_id": "C0004",
                    "type": "edge",
                    "source": "B",
                    "target": "A",
                    "direction": "<->",
                    "description": "B<->A",
                },
            ]
        )
        reg = ComponentRegistry(df)
        s = reg.summary()
        assert s["total_components"] == 4
        assert s["nodes"] == 2
        assert s["edges"] == 2
        assert s["directed_edges"] == 1
        assert s["bidirectional_edges"] == 1


class TestRegistryLoader:
    def test_valid_parquet_loads(self, tmp_path):
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
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "X->Y",
            },
        ]
        path = tmp_path / "test.parquet"
        _write_parquet(_make_df(records), path)
        reg = RegistryLoader.load(path)
        assert isinstance(reg, ComponentRegistry)
        assert len(reg.data) == 2

    def test_from_records(self):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
            },
        ]
        reg = RegistryLoader.from_records(records)
        assert len(reg.data) == 1

    def test_from_records_preserves_extra_metadata(self):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "X",
                "visibility": "visible",
                "illusion_role": "node",
            },
        ]
        reg = RegistryLoader.from_records(records)
        assert reg.data.loc[0, "visibility"] == "visible"
        assert reg.data.loc[0, "illusion_role"] == "node"

    def test_empty_parquet_raises_error(self, tmp_path):
        path = tmp_path / "empty.parquet"
        df = pd.DataFrame(
            columns=[
                "comp_id",
                "type",
                "source",
                "target",
                "direction",
                "description",
            ]
        )
        _write_parquet(df, path)
        with pytest.raises(RegistryError):
            RegistryLoader.load(path)

    def test_missing_columns_parquet_raises_error(self, tmp_path):
        df = pd.DataFrame([{"comp_id": "C0001", "type": "node"}])
        path = tmp_path / "bad.parquet"
        _write_parquet(df, path)
        with pytest.raises(RegistryError):
            RegistryLoader.load(path)

    def test_nonexistent_file_raises_error(self):
        with pytest.raises(RegistryError):
            RegistryLoader.load("/nonexistent/path.parquet")

    def test_empty_records_raises_error(self):
        with pytest.raises(RegistryError):
            RegistryLoader.from_records([])

    def test_nan_normalization(self, tmp_path):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "Node X",
            },
        ]
        path = tmp_path / "nan_test.parquet"
        _write_parquet(_make_df(records), path)
        reg = RegistryLoader.load(path)
        row = reg.data.iloc[0]
        assert row["target"] is None
        assert row["direction"] is None

    def test_nan_normalization_with_mixed_types(self, tmp_path):
        records = [
            {
                "comp_id": "C0001",
                "type": "node",
                "source": "X",
                "target": None,
                "direction": None,
                "description": "Node X",
            },
            {
                "comp_id": "C0002",
                "type": "edge",
                "source": "X",
                "target": "Y",
                "direction": "->",
                "description": "Edge X->Y",
            },
        ]
        path = tmp_path / "mixed.parquet"
        _write_parquet(_make_df(records), path)
        reg = RegistryLoader.load(path)
        assert reg.data.iloc[0]["target"] is None
        assert reg.data.iloc[0]["direction"] is None
        assert reg.data.iloc[1]["target"] == "Y"
        assert reg.data.iloc[1]["direction"] == "->"


class TestSeedData:
    def test_seed_registry_loads(self, tmp_path):
        paths = generate_seed_data(output_dir=tmp_path)
        reg = RegistryLoader.load(paths["registry"])
        s = reg.summary()
        assert s["total_components"] >= 20
        assert s["nodes"] >= 1
        assert s["edges"] >= 1
        assert s["directed_edges"] >= 1
        assert s["bidirectional_edges"] >= 1

    def test_seed_states_have_correct_structure(self, tmp_path):
        paths = generate_seed_data(output_dir=tmp_path)
        states_df = pd.read_parquet(paths["states"])
        assert len(states_df) > 0
        assert "model_id" in states_df.columns
        assert "comp_id" in states_df.columns
        assert "status" in states_df.columns
        model_ids = states_df["model_id"].unique()
        assert 5 <= len(model_ids) <= 10

    def test_seed_has_status_conflicts(self, tmp_path):
        paths = generate_seed_data(output_dir=tmp_path)
        states_df = pd.read_parquet(paths["states"])
        comps = states_df["comp_id"].unique()
        found_causal_non_causal = False
        for cid in comps:
            statuses = (
                states_df[states_df["comp_id"] == cid]
                .groupby("model_id")["status"]
                .first()
                .tolist()
            )
            if "causal" in statuses and "non-causal" in statuses:
                found_causal_non_causal = True
                break
        assert (
            found_causal_non_causal
        ), "Expected at least one causal/non-causal conflict across components"

    def test_seed_has_all_statuses(self, tmp_path):
        paths = generate_seed_data(output_dir=tmp_path)
        states_df = pd.read_parquet(paths["states"])
        statuses = states_df["status"].unique()
        for s in ("causal", "unknown", "non-causal"):
            assert s in statuses, f"Missing status: {s}"


class TestComponentRegistryBuilder:
    @pytest.mark.parametrize("invalid_timing", [0, -1])
    def test_timing_inputs_must_be_at_least_one(self, invalid_timing):
        with pytest.raises(RegistryError, match="at least 1"):
            ComponentRegistryBuilder.from_nodes(
                [{"name": "X", "timing": invalid_timing}, {"name": "Y"}]
            )

        with pytest.raises(RegistryError, match="at least 1"):
            ComponentRegistryBuilder.from_nodes(
                [
                    {"name": "X", "timing_options": [1, invalid_timing]},
                    {"name": "Y"},
                ]
            )

    def test_basic_nodes_generate_correct_structure(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
            {"name": "Z", "timing": 3},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes)
        s = reg.summary()
        assert s["nodes"] == 3
        # timing: X<Y, X<Z, Y<Z = 3 directed edges
        assert s["edges"] == 3
        assert s["directed_edges"] == 3
        assert s["bidirectional_edges"] == 0

    def test_respect_timing_excludes_temporal_invalid_edges(self):
        nodes = [
            {"name": "X", "timing": 3},
            {"name": "Y", "timing": 1},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, respect_timing=True)
        assert reg.summary()["edges"] == 1  # Y->X only

    def test_respect_timing_false_includes_all(self):
        nodes = [
            {"name": "X", "timing": 3},
            {"name": "Y", "timing": 1},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, respect_timing=False)
        assert reg.summary()["edges"] == 2  # X->Y and Y->X

    def test_no_self_edges(self):
        nodes = [{"name": "X"}, {"name": "Y"}]
        reg = ComponentRegistryBuilder.from_nodes(nodes, respect_timing=False)
        sources = reg.data[reg.data["type"] == "edge"][["source", "target"]]
        assert not any(row["source"] == row["target"] for _, row in sources.iterrows())

    def test_include_bidirectional(self):
        nodes = [
            {"name": "A", "timing": 1},
            {"name": "B", "timing": 1},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, include_bidirectional=True)
        directions = set(reg.data[reg.data["type"] == "edge"]["direction"].tolist())
        assert "<->" in directions
        assert reg.summary()["bidirectional_edges"] == 1

    def test_no_duplicate_bidirectional(self):
        nodes = [
            {"name": "A", "timing": 1},
            {"name": "B", "timing": 1},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, include_bidirectional=True)
        bidis = reg.data[reg.data["direction"] == "<->"]
        assert len(bidis) == 1

    def test_bidirectional_generation_respects_fixed_timing(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X1", "timing": 1},
                {"name": "X2", "timing": 1},
                {"name": "Y", "timing": 2},
            ],
            include_bidirectional=True,
            exposure="X1",
            outcome="Y",
        )
        bidirected = reg.data.loc[
            reg.data["direction"] == "<->", ["source", "target"]
        ].to_dict("records")
        assert bidirected == [{"source": "X1", "target": "X2"}]

    def test_bidirectional_generation_uses_overlapping_timing_options(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "A", "timing_options": [1, 2]},
                {"name": "B", "timing_options": [2, 3]},
                {"name": "C", "timing_options": [4]},
            ],
            include_bidirectional=True,
        )
        bidirected = set(
            reg.data.loc[
                reg.data["direction"] == "<->", ["source", "target"]
            ].itertuples(index=False, name=None)
        )
        assert bidirected == {("A", "B")}

    def test_bidirectional_generation_ignores_timing_when_not_respected(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "A", "timing": 1},
                {"name": "B", "timing": 2},
            ],
            respect_timing=False,
            include_bidirectional=True,
        )
        assert reg.summary()["bidirectional_edges"] == 1

    def test_explicit_cross_time_allow_extends_automatic_candidates(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "A", "timing": 1},
                {"name": "B", "timing": 1},
                {"name": "C", "timing": 2},
            ],
            include_bidirectional=True,
            constraints=[
                {"source": "A", "target": "C", "direction": "<->", "rule": "allow"}
            ],
        )
        bidirected = set(
            reg.data.loc[
                reg.data["direction"] == "<->", ["source", "target"]
            ].itertuples(index=False, name=None)
        )
        assert bidirected == {("A", "B"), ("A", "C")}

    def test_constraints_allow_subset(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
            {"name": "Z", "timing": 3},
        ]
        constraints = [
            {
                "source": "X",
                "target": "Y",
                "direction": "->",
                "rule": "allow",
            },
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, constraints=constraints)
        assert reg.summary()["edges"] == 1

    def test_constraints_forbid(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
            {"name": "Z", "timing": 3},
        ]
        constraints = [
            {
                "source": "X",
                "target": "Y",
                "direction": "->",
                "rule": "forbid",
            },
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, constraints=constraints)
        edges = reg.data[reg.data["type"] == "edge"]
        forbidden = edges[(edges["source"] == "X") & (edges["target"] == "Y")]
        assert len(forbidden) == 0
        assert len(edges) == 2  # X->Z and Y->Z remain

    def test_constraints_require_adds_edge(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
        ]
        # Required edge already generated by timing.
        constraints = [
            {
                "source": "X",
                "target": "Y",
                "direction": "->",
                "rule": "require",
            },
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes, constraints=constraints)
        edges = reg.data[reg.data["type"] == "edge"]
        assert len(edges) == 1  # X->Y still generated
        assert edges.iloc[0]["source"] == "X"
        assert edges.iloc[0]["target"] == "Y"

    def test_constraints_forbid_and_require_contradiction_raises(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
        ]
        constraints = [
            {
                "source": "X",
                "target": "Y",
                "direction": "->",
                "rule": "forbid",
            },
            {
                "source": "X",
                "target": "Y",
                "direction": "->",
                "rule": "require",
            },
        ]
        with pytest.raises(RegistryError, match="Contradictory constraint"):
            ComponentRegistryBuilder.from_nodes(nodes, constraints=constraints)

    def test_constraints_unknown_node_raises(self):
        nodes = [{"name": "X"}]
        constraints = [
            {
                "source": "X",
                "target": "UNKNOWN",
                "rule": "allow",
            },
        ]
        with pytest.raises(RegistryError, match="unknown node"):
            ComponentRegistryBuilder.from_nodes(nodes, constraints=constraints)

    def test_deterministic_ids(self):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
            {"name": "Z", "timing": 3},
        ]
        reg1 = ComponentRegistryBuilder.from_nodes(nodes)
        reg2 = ComponentRegistryBuilder.from_nodes(nodes)
        ids1 = reg1.data["comp_id"].tolist()
        ids2 = reg2.data["comp_id"].tolist()
        assert ids1 == ids2
        assert ids1[:3] == ["C0001", "C0002", "C0003"]

    def test_parquet_roundtrip(self, tmp_path):
        nodes = [
            {"name": "X", "timing": 1},
            {"name": "Y", "timing": 2},
        ]
        reg = ComponentRegistryBuilder.from_nodes(nodes)
        path = tmp_path / "built.parquet"
        ComponentRegistryBuilder.to_parquet(reg, path)
        loaded = RegistryLoader.load(path)
        assert len(loaded.data) == len(reg.data)
        assert loaded.summary() == reg.summary()

    def test_duplicate_names_raises(self):
        nodes = [{"name": "X"}, {"name": "X"}]
        with pytest.raises(RegistryError, match="Duplicate"):
            ComponentRegistryBuilder.from_nodes(nodes)

    def test_empty_nodes_raises(self):
        with pytest.raises(RegistryError, match="At least one node"):
            ComponentRegistryBuilder.from_nodes([])

    def test_all_missing_timing_fixes_only_exposure_to_outcome(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [{"name": "X"}, {"name": "Z"}, {"name": "Y"}],
            include_bidirectional=True,
            exposure="X",
            outcome="Y",
        )
        edges = reg.data[reg.data["type"] == "edge"]
        fixed = edges[edges["fixed_status"] == "causal"]

        assert fixed[["source", "target", "direction"]].to_dict("records") == [
            {"source": "X", "target": "Y", "direction": "->"}
        ]
        assert not ((edges["source"] == "Y") & (edges["target"] == "X")).any()
        assert not (
            (edges["direction"] == "<->")
            & (edges["source"].isin(["X", "Y"]))
            & (edges["target"].isin(["X", "Y"]))
        ).any()
        assert {("X", "Z"), ("Z", "X")} <= set(
            edges.loc[edges["direction"] == "->", ["source", "target"]].itertuples(
                index=False, name=None
            )
        )

    @pytest.mark.parametrize(
        "constraints, match",
        [
            (
                [{"source": "X", "target": "Y", "rule": "forbid"}],
                "Cannot forbid X -> Y",
            ),
            (
                [{"source": "Y", "target": "X", "rule": "require"}],
                "Cannot require Y -> X",
            ),
        ],
    )
    def test_all_missing_timing_rejects_conflicting_constraints(
        self, constraints, match
    ):
        with pytest.raises(RegistryError, match=match):
            ComponentRegistryBuilder.from_nodes(
                [{"name": "X"}, {"name": "Z"}, {"name": "Y"}],
                constraints=constraints,
                exposure="X",
                outcome="Y",
            )

    def test_exposure_to_outcome_is_fixed_with_supplied_timing(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing": None},
                {"name": "Z", "timing": 1},
                {"name": "Y", "timing": None},
            ],
            exposure="X",
            outcome="Y",
        )
        fixed = reg.data[reg.data["fixed_status"] == "causal"]
        assert fixed[["source", "target", "direction"]].to_dict("records") == [
            {"source": "X", "target": "Y", "direction": "->"}
        ]

    def test_timing_options_generate_each_possible_direction(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing_options": [1, 3]},
                {"name": "Y", "timing_options": [2]},
            ]
        )
        edges = set(
            reg.data.loc[reg.data["type"] == "edge", ["source", "target"]].itertuples(
                index=False,
                name=None,
            )
        )
        assert edges == {("X", "Y"), ("Y", "X")}

    def test_required_directed_constraints_are_fixed_causal(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing_options": [1]},
                {"name": "Z", "timing_options": [2]},
                {"name": "Y", "timing_options": [3]},
            ],
            constraints=[
                {"source": "Z", "target": "Y", "direction": "->", "rule": "require"}
            ],
            exposure="X",
            outcome="Y",
        )
        fixed = reg.data.loc[
            reg.data["fixed_status"] == "causal",
            ["source", "target", "direction"],
        ].to_dict("records")
        assert fixed == [
            {"source": "X", "target": "Y", "direction": "->"},
            {"source": "Z", "target": "Y", "direction": "->"},
        ]

    def test_allowed_bidirected_constraint_generates_named_candidate_only(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "A", "timing": 1},
                {"name": "B", "timing": 2},
                {"name": "C", "timing": 3},
            ],
            constraints=[
                {"source": "A", "target": "C", "direction": "<->", "rule": "allow"}
            ],
        )
        bidirected = reg.data.loc[
            reg.data["direction"] == "<->",
            ["source", "target"],
        ].to_dict("records")
        assert bidirected == [{"source": "A", "target": "C"}]

    def test_required_bidirected_constraints_are_fixed_causal(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing": 1},
                {"name": "Z", "timing": 1},
                {"name": "Y", "timing": 2},
            ],
            constraints=[
                {"source": "X", "target": "Z", "direction": "<->", "rule": "require"},
                {"source": "Z", "target": "Y", "direction": "<->", "rule": "require"},
            ],
            exposure="X",
            outcome="Y",
        )
        fixed = reg.data.loc[
            (reg.data["direction"] == "<->") & (reg.data["fixed_status"] == "causal"),
            ["source", "target"],
        ].to_dict("records")
        assert fixed == [{"source": "X", "target": "Z"}, {"source": "Y", "target": "Z"}]

from __future__ import annotations

import pytest
import pandas as pd
import torch

from registry.loader import RegistryLoader
from registry.schema import ComponentRegistry
from state.sparse import state_to_dataframe, state_to_sparse
from state.semantics import normalize_sparse_records
from state.tensor import StateError, StateTensor
from state.expander import ModelStateExpander
from registry.builder import ComponentRegistryBuilder
from dyadic.engine import DyadicEngine
from simulation.delta_u import DeltaUEngine


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


class TestStateTensorCreate:
    def test_creation_shape(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        assert state.tensor.shape == (2, 3, 2)
        assert state.tensor.dtype == torch.uint8

    def test_default_is_unknown(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        assert state.get_status("M0001", "C0001") == "unknown"
        assert state.get_status("M0001", "C0002") == "unknown"
        assert state.get_status("M0001", "C0003") == "unknown"

    def test_model_ids_are_sorted(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0002", "M0001"])
        assert state.model_ids == ["M0001", "M0002"]

    def test_unknown_model_id_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown model ID"):
            state.get_status("M9999", "C0001")

    def test_unknown_component_id_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown component ID"):
            state.get_status("M0001", "C9999")


class TestStateTensorSetGet:
    def test_set_and_get_status(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        assert state.get_status("M0001", "C0001") == "causal"

    def test_set_non_causal(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "non-causal")
        assert state.get_status("M0001", "C0001") == "non-causal"

    def test_invalid_status_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Invalid status"):
            state.set_status("M0001", "C0001", "invalid_status")

    def test_set_status_unknown_model_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown model ID"):
            state.set_status("M9999", "C0001", "causal")

    def test_set_status_unknown_component_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown component ID"):
            state.set_status("M0001", "C9999", "causal")

    def test_multiple_models_independent(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        state.set_status("M0001", "C0001", "causal")
        state.set_status("M0002", "C0001", "non-causal")
        assert state.get_status("M0001", "C0001") == "causal"
        assert state.get_status("M0002", "C0001") == "non-causal"


class TestStateTensorTiming:
    def test_set_and_get_timing(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_timing("M0001", "C0001", 5)
        assert state.get_timing("M0001", "C0001") == 5

    def test_timing_defaults_to_none(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        assert state.get_timing("M0001", "C0001") is None

    def test_timing_must_be_integer(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Timing must be integer"):
            state.set_timing("M0001", "C0001", 3.5)

    @pytest.mark.parametrize("invalid_timing", [0, -1])
    def test_timing_must_be_at_least_one(self, invalid_timing):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="positive integer"):
            state.set_timing("M0001", "C0001", invalid_timing)

    @pytest.mark.parametrize(
        ("invalid_timing", "message"),
        [
            (True, "Timing must be integer"),
            (False, "Timing must be integer"),
            (1.5, "Timing must be integer"),
            (0, "positive integer"),
            (-1, "positive integer"),
        ],
    )
    def test_constructor_validates_supplied_timing_map(self, invalid_timing, message):
        reg = _make_registry()
        component_ids = sorted(reg.data["comp_id"].tolist())

        with pytest.raises(StateError, match=message):
            StateTensor(
                tensor=torch.zeros((1, len(component_ids), 2), dtype=torch.uint8),
                model_index={"M0001": 0},
                component_index={cid: i for i, cid in enumerate(component_ids)},
                component_ids=component_ids,
                model_ids=["M0001"],
                timing={("M0001", "C0001"): invalid_timing},
            )

    def test_timing_unknown_model_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown model ID"):
            state.set_timing("M9999", "C0001", 1)

    def test_timing_unknown_component_raises_error(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        with pytest.raises(StateError, match="Unknown component ID"):
            state.set_timing("M0001", "C9999", 1)


class TestStateTensorFromRecords:
    def test_from_records_basic(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
            {
                "model_id": "M0001",
                "comp_id": "C0002",
                "status": "unknown",
                "timing": None,
            },
            {
                "model_id": "M0002",
                "comp_id": "C0001",
                "status": "non-causal",
                "timing": None,
            },
        ]
        state = StateTensor.from_records(reg, records)
        assert state.get_status("M0001", "C0001") == "causal"
        assert state.get_status("M0001", "C0002") == "unknown"
        assert state.get_status("M0002", "C0001") == "non-causal"
        assert state.get_timing("M0001", "C0001") == 1

    def test_from_records_infers_model_ids(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
            {"model_id": "M0001", "comp_id": "C0002", "status": "causal", "timing": 2},
        ]
        state = StateTensor.from_records(reg, records)
        assert state.model_ids == ["M0001", "M0002"]

    def test_from_records_with_explicit_model_ids(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal", "timing": 1},
        ]
        state = StateTensor.from_records(
            reg, records, model_ids=["M0001", "M0002", "M0003"]
        )
        assert state.model_ids == ["M0001", "M0002", "M0003"]
        assert state.get_status("M0001", "C0001") == "unknown"
        assert state.get_status("M0002", "C0001") == "causal"

    def test_records_not_covering_all_components(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal", "timing": 1},
        ]
        state = StateTensor.from_records(reg, records)
        assert state.get_status("M0001", "C0001") == "causal"
        assert state.get_status("M0001", "C0002") == "unknown"
        assert state.get_status("M0001", "C0003") == "unknown"

    def test_edge_record_conflicts_with_explicit_absent_endpoint(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present"},
            {"model_id": "M0001", "comp_id": "C0002", "status": "absent"},
            {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        ]
        with pytest.raises(StateError, match="explicitly absent endpoint"):
            StateTensor.from_records(reg, records)

    def test_edge_record_rejects_node_status(self):
        reg = _make_registry()
        records = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "present"},
            {"model_id": "M0001", "comp_id": "C0002", "status": "present"},
            {"model_id": "M0001", "comp_id": "C0003", "status": "absent"},
        ]
        with pytest.raises(StateError, match="Invalid status for edge"):
            StateTensor.from_records(reg, records)


class TestNormalizeSparseRecords:
    @pytest.mark.parametrize(
        ("invalid_timing", "message"),
        [
            (True, "Timing must be integer"),
            (False, "Timing must be integer"),
            (1.5, "Timing must be integer"),
            (0, "positive integer"),
            (-1, "positive integer"),
        ],
    )
    def test_raw_records_validate_timing(self, invalid_timing, message):
        reg = _make_registry()
        records = [
            {
                "model_id": "M0001",
                "comp_id": "C0001",
                "status": "present",
                "timing": invalid_timing,
            }
        ]

        with pytest.raises(StateError, match=message):
            normalize_sparse_records(reg, records)


class TestStateTensorToRecords:
    def test_to_records_includes_all_combinations(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        records = state.to_dense_records()
        assert len(records) == 2 * 3
        for r in records:
            assert set(r.keys()) == {"model_id", "comp_id", "status", "timing"}

    def test_to_records_sparse_only_present_nodes(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "present")
        records = state.to_records()
        present_node_records = [r for r in records if r["comp_id"] == "C0001"]
        assert len(present_node_records) == 1
        assert present_node_records[0]["status"] == "present"

    def test_to_records_lowercase_statuses(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        records = state.to_dense_records()
        r = next(x for x in records if x["comp_id"] == "C0001")
        assert r["status"] == "causal"

    def test_to_records_timing_preserved(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_timing("M0001", "C0001", 42)
        state.set_status("M0001", "C0001", "present")
        records = state.to_records()
        r = next(x for x in records if x["comp_id"] == "C0001")
        assert r["timing"] == 42


class TestStateTensorSparse:
    def test_sparse_conversion_shape(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        state.set_status("M0001", "C0001", "causal")
        state.set_status("M0002", "C0001", "non-causal")
        sparse = state_to_sparse(state)
        assert sparse.shape == (2, 3)

    def test_sparse_encoding_values(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        state.set_status("M0001", "C0002", "non-causal")
        sparse = state_to_sparse(state).toarray()
        assert sparse[0, 0] == 1
        assert sparse[0, 1] == 2
        assert sparse[0, 2] == 0

    def test_sparse_via_to_sparse_method(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        sparse = state.to_sparse()
        assert sparse.shape == (1, 3)

    def test_sparse_dataframe_export(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        df = state_to_dataframe(state)
        assert list(df.columns) == ["model_id", "comp_id", "status"]
        assert len(df) == 3
        assert df.loc[df["comp_id"] == "C0001", "status"].iloc[0] == "causal"

    def test_to_dataframe_method(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        df = state.to_dataframe()
        assert list(df.columns) == ["model_id", "comp_id", "status"]

    def test_packed_tensor_uses_two_bit_status_codes(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        state.set_status("M0001", "C0002", "non-causal")
        # C0001=01 and C0002=10 are packed into the first byte as 0b1001.
        assert int(state.packed_tensor[0, 0]) == 9


class TestModelStateExpander:
    @staticmethod
    def _build_registry(nodes):
        return ComponentRegistryBuilder.from_nodes(nodes)

    @pytest.mark.parametrize("invalid_timing", [0, -1])
    def test_timing_inputs_must_be_at_least_one(self, invalid_timing):
        reg = self._build_registry([{"name": "X"}, {"name": "Y"}])

        with pytest.raises(StateError, match="at least 1"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                node_timing={"X": invalid_timing},
            )

        with pytest.raises(StateError, match="at least 1"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                timing_options={"X": [invalid_timing]},
            )

        with pytest.raises(StateError, match="at least 1"):
            ModelStateExpander.expand(
                reg,
                mode="sampled",
                n_models=1,
                seed_claims=[
                    {
                        "model_id": "seed",
                        "comp_id": "C0001",
                        "status": "present",
                        "timing": invalid_timing,
                    }
                ],
            )

    # --- seed_claims integration (sampled/exhaustive + seeds) ---------------

    def test_sampled_with_seed_claims(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        claims = [
            {"model_id": "M0001", "comp_id": "C0003", "status": "causal"},
        ]
        records = ModelStateExpander.expand(
            reg, mode="sampled", n_models=10, seed=42, seed_claims=claims
        )
        df = pd.DataFrame(records)
        assert "seeded" in df.columns
        seeded = df[df["seeded"]]
        assert set(seeded["model_id"].unique()) == {"M0001"}
        m1 = seeded[seeded["model_id"] == "M0001"]
        assert m1[m1["comp_id"] == "C0001"]["status"].iloc[0] == "present"
        assert m1[m1["comp_id"] == "C0002"]["status"].iloc[0] == "present"
        assert m1[m1["comp_id"] == "C0003"]["status"].iloc[0] == "causal"

    def test_seed_claims_defaults_unspecified_to_unknown(self):
        reg = self._build_registry([{"name": "X"}, {"name": "Y"}])
        claims = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal"},
        ]
        records = ModelStateExpander.expand(
            reg, mode="sampled", n_models=5, seed=42, seed_claims=claims
        )
        df = pd.DataFrame(records)
        seeded = df[(df["seeded"]) & (df["model_id"] == "M0001")]
        edge_rows = seeded[
            seeded["comp_id"].isin(reg.data[reg.data["type"] == "edge"]["comp_id"])
        ]
        assert all(edge_rows["status"] == "unknown")

    def test_edge_only_seed_infers_endpoints_and_omits_absent_nodes(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        claims = [
            {"model_id": "M_seed", "comp_id": "C0004", "status": "causal"},
        ]

        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            seed_claims=claims,
            node_policy="vary",
        )
        df = pd.DataFrame(records)
        seed_rows = df[df["model_id"] == "M_seed"]

        assert set(seed_rows["comp_id"]) == {"C0001", "C0002", "C0004"}
        assert seed_rows[seed_rows["comp_id"] == "C0001"]["status"].iloc[0] == "present"
        assert seed_rows[seed_rows["comp_id"] == "C0002"]["status"].iloc[0] == "present"
        assert seed_rows[seed_rows["comp_id"] == "C0004"]["status"].iloc[0] == "causal"

    def test_vary_node_policy_requires_exposure_and_outcome(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )

        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_timing={"X": 1, "Y": 2, "Z": 3},
            node_policy="vary",
            exposure="X",
            outcome="Z",
        )
        df = pd.DataFrame(records)

        node_ids = (
            reg.data[reg.data["type"] == "node"]
            .set_index("source")["comp_id"]
            .to_dict()
        )
        required = {node_ids["X"], node_ids["Z"]}
        node_records = df[df["comp_id"].isin(node_ids.values())]

        assert len(df["model_id"].unique()) == 30
        for _, group in node_records.groupby("model_id"):
            assert required.issubset(set(group["comp_id"]))

    def test_seed_claims_must_include_required_exposure_and_outcome(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        claims = [
            {"model_id": "M_seed", "comp_id": "C0001", "status": "present"},
        ]

        with pytest.raises(StateError, match="omits required exposure/outcome"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                seed_claims=claims,
                node_policy="vary",
                exposure="X",
                outcome="Z",
            )

    def test_seed_edge_conflicts_with_explicit_absent_endpoint(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        claims = [
            {"model_id": "M_seed", "comp_id": "C0003", "status": "absent"},
            {"model_id": "M_seed", "comp_id": "C0005", "status": "causal"},
        ]

        with pytest.raises(StateError, match="explicitly absent"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                seed_claims=claims,
                node_policy="vary",
            )

    def test_exhaustive_finds_seed_in_multiverse(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        # This seed matches a model that exhaustive would generate
        claims = [
            {"model_id": "M_prior", "comp_id": "C0003", "status": "causal"},
        ]
        records = ModelStateExpander.expand(reg, mode="exhaustive", seed_claims=claims)
        df = pd.DataFrame(records)
        assert "M_prior" in df["model_id"].unique()
        prior_rows = df[df["model_id"] == "M_prior"]
        assert all(prior_rows["seeded"])
        # No duplicate: the matched generated model is renamed, not duplicated
        non_seeded = df[~df["seeded"]]
        assert "M_prior" not in non_seeded["model_id"].unique()

    def test_seed_renumbering_avoids_preoccupied_m0001(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        claims = [
            {"model_id": "M0001", "comp_id": "C0001", "status": "causal"},
        ]

        records = ModelStateExpander.expand(reg, mode="exhaustive", seed_claims=claims)
        df = pd.DataFrame(records)

        assert len(df["model_id"].unique()) == 4
        assert len(df[df["model_id"] == "M0001"]) == 1
        assert all(
            df[df["model_id"] != "M0001"].groupby("model_id").size() == len(reg.data)
        )
        assert set(df[df["model_id"] == "M0001"]["seeded"]) == {True}

    def test_seed_renumbering_skips_seed_id_when_seed_matches_other_model(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        claims = [
            {"model_id": "M0002", "comp_id": "C0001", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0002", "status": "causal"},
            {"model_id": "M0002", "comp_id": "C0003", "status": "causal"},
        ]

        records = ModelStateExpander.expand(reg, mode="exhaustive", seed_claims=claims)
        df = pd.DataFrame(records)

        assert len(df["model_id"].unique()) == 3
        assert all(df.groupby("model_id").size() == len(reg.data))
        assert set(df[df["model_id"] == "M0002"]["seeded"]) == {True}

    def test_seed_renumbering_skips_arbitrary_seed_id(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        claims = [
            {"model_id": "M0005", "comp_id": "C0001", "status": "causal"},
        ]

        records = ModelStateExpander.expand(reg, mode="exhaustive", seed_claims=claims)
        df = pd.DataFrame(records)

        assert len(df["model_id"].unique()) == 28
        assert len(df[df["model_id"] == "M0005"]) == 1
        assert all(
            df[df["model_id"] != "M0005"].groupby("model_id").size() == len(reg.data)
        )
        assert set(df[df["model_id"] == "M0005"]["seeded"]) == {True}

    def test_seeded_mode_raises(self):
        reg = self._build_registry([{"name": "X"}])
        with pytest.raises(StateError, match="sampled.*exhaustive"):
            ModelStateExpander.expand(reg, mode="seeded")

    def test_no_seed_claims_adds_seeded_false(self):
        reg = self._build_registry(
            [{"name": "X", "timing": 1}, {"name": "Y", "timing": 2}]
        )
        records = ModelStateExpander.expand(reg, mode="exhaustive")
        assert all(r["seeded"] is False for r in records)

    # --- exhaustive mode -----------------------------------------------------

    def test_exhaustive_two_nodes_one_edge(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        # 1 edge component (C0003 X->Y) => 3 models (causal, unknown, non-causal)
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_timing={"X": 1, "Y": 2},
        )
        df = pd.DataFrame(records)
        assert len(df["model_id"].unique()) == 3
        statuses = set(df[df["comp_id"] == "C0003"]["status"])
        assert statuses == {"causal", "unknown", "non-causal"}
        # Every model should have both nodes as "present"
        for mid in df["model_id"].unique():
            model = df[df["model_id"] == mid]
            assert model[model["comp_id"] == "C0001"]["status"].iloc[0] == "present"
            assert model[model["comp_id"] == "C0002"]["status"].iloc[0] == "present"

    def test_exhaustive_binary_compatibility(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_timing={"X": 1, "Y": 2},
            edge_statuses=["causal", "unknown"],
        )
        df = pd.DataFrame(records)
        assert len(df["model_id"].unique()) == 2
        statuses = set(df[df["comp_id"] == "C0003"]["status"])
        assert statuses == {"causal", "unknown"}

    def test_exhaustive_cycle_is_excluded(self):
        # Two nodes each pointing to the other in timing => cycle impossible
        # But the builder with respect_timing won't generate those edges.
        # Let's force a cycle with directed edges that go both ways.
        nodes = [
            {"name": "A", "timing": 1},
            {"name": "B", "timing": 2},
        ]
        # Build registry with timing -> only A->B generated.
        # Disable timing for edge generation to get both A->B and B->A
        reg = ComponentRegistryBuilder.from_nodes(nodes, respect_timing=False)
        # No node_timing passed -> temporal checks off; only cycle is excluded.
        # 2 edges; 3^2 = 9 combos. Only 8 non-cyclic (both causal excluded).
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
        )
        df = pd.DataFrame(records)
        model_count = len(df["model_id"].unique())
        assert model_count == 8  # 9 combos - 1 cycle = 8

    def test_exhaustive_temporal_invalid_edge_never_causal(self):
        nodes = [
            {"name": "X", "timing": 3},
            {"name": "Y", "timing": 1},
        ]
        reg = ComponentRegistryBuilder.from_nodes(
            nodes, respect_timing=False
        )  # all edges
        # X->Y should never be "causal" because timing(X)=3 >= timing(Y)=1
        # But "non-causal" and "unknown" are allowed.
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_timing={"X": 3, "Y": 1},
        )
        df = pd.DataFrame(records)
        for _, row in reg.data[reg.data["type"] == "edge"].iterrows():
            if row["source"] == "X" and row["target"] == "Y":
                x_to_y_cid = row["comp_id"]
                break
        for mid in df["model_id"].unique():
            status = df[(df["model_id"] == mid) & (df["comp_id"] == x_to_y_cid)][
                "status"
            ].iloc[0]
            assert status != "causal"
        # Ensure "non-causal" does appear for the temporal-invalid edge
        statuses = set(df[df["comp_id"] == x_to_y_cid]["status"])
        assert "non-causal" in statuses

    def test_timing_filter_preserves_binary_status_requests(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing": 3},
                {"name": "Y", "timing": 1},
            ],
            respect_timing=False,
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X": [3], "Y": [1]},
            edge_statuses=["causal", "unknown"],
        )
        df = pd.DataFrame(records)
        invalid_cid = reg.data.loc[
            (reg.data["source"] == "X") & (reg.data["target"] == "Y"),
            "comp_id",
        ].iloc[0]
        assert set(df.loc[df["comp_id"] == invalid_cid, "status"]) == {"unknown"}
        assert records.pruning_report["projected_model_count"] == 2

    def test_timing_filter_reports_empty_status_dimension_explicitly(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing": 3},
                {"name": "Y", "timing": 1},
            ],
            respect_timing=False,
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X": [3], "Y": [1]},
            edge_statuses=["causal"],
        )
        assert records == []
        assert records.pruning_report["temporal_edge_statuses_pruned"] == 1
        assert records.pruning_report["temporal_edge_assignments_pruned"] == 1
        assert records.pruning_report["projected_model_count"] == 0

    def test_exhaustive_bidirected_uses_present_absent_statuses(self):
        reg = RegistryLoader.from_records(
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
                    "direction": "<->",
                    "description": "X <-> Y",
                },
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_timing={"X": 2, "Y": 1},
        )
        statuses = {r["status"] for r in records if r["comp_id"] == "C0003"}
        assert statuses == {"present", "absent"}

    def test_exhaustive_max_models_safeguard(self):
        reg = self._build_registry(
            [{"name": f"X{i}", "timing": i + 1} for i in range(15)]
        )
        # 15 nodes, lots of edges, default max_models=10000
        # This should exceed max_models and raise
        with pytest.raises(StateError, match="exceeding max_models"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                max_models=100,
            )

    def test_edge_statuses_invalid_raises(self):
        reg = self._build_registry([{"name": "X"}])
        with pytest.raises(StateError, match="Invalid edge status"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                edge_statuses=["causal", "invalid"],
            )

    def test_edge_statuses_duplicates_raises(self):
        reg = self._build_registry([{"name": "X"}])
        with pytest.raises(StateError, match="Duplicate"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                edge_statuses=["causal", "causal"],
            )

    def test_edge_statuses_empty_raises(self):
        reg = self._build_registry([{"name": "X"}])
        with pytest.raises(StateError, match="at least one status"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                edge_statuses=[],
            )

    # --- sampled mode --------------------------------------------------------

    def test_sampled_returns_requested_count(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=3,
            seed=42,
            node_timing={"X": 1, "Y": 2, "Z": 3},
        )
        df = pd.DataFrame(records)
        assert len(df["model_id"].unique()) == 3

    def test_sampled_with_non_causal(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
                {"name": "Z", "timing": 3},
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=20,
            seed=7,
            node_timing={"X": 1, "Y": 2, "Z": 3},
        )
        df = pd.DataFrame(records)
        edge_cids = reg.data[reg.data["type"] == "edge"]["comp_id"].tolist()
        edge_statuses = set(df[df["comp_id"].isin(edge_cids)]["status"])
        assert "non-causal" in edge_statuses
        assert "causal" in edge_statuses
        assert "unknown" in edge_statuses

    def test_sampled_binary_compatibility(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=2,
            seed=1,
            node_timing={"X": 1, "Y": 2},
            edge_statuses=["causal", "unknown"],
        )
        df = pd.DataFrame(records)
        edge_cids = reg.data[reg.data["type"] == "edge"]["comp_id"].tolist()
        edge_statuses = set(df[df["comp_id"].isin(edge_cids)]["status"])
        assert "non-causal" not in edge_statuses

    def test_sampled_reproducible(self):
        reg = self._build_registry(
            [
                {"name": "X", "timing": 1},
                {"name": "Y", "timing": 2},
            ]
        )
        r1 = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=2,
            seed=99,
            node_timing={"X": 1, "Y": 2},
        )
        r2 = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=2,
            seed=99,
            node_timing={"X": 1, "Y": 2},
        )
        ids1 = sorted({rec["model_id"] for rec in r1})
        ids2 = sorted({rec["model_id"] for rec in r2})
        assert ids1 == ids2

    def test_fixed_causal_edge_is_not_enumerated_or_dropped(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [{"name": "X"}, {"name": "Z"}, {"name": "Y"}],
            exposure="X",
            outcome="Y",
        )
        fixed_cid = reg.data.loc[reg.data["fixed_status"] == "causal", "comp_id"].iloc[
            0
        ]
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            node_policy="vary",
        )
        df = pd.DataFrame(records)

        assert set(df.loc[df["comp_id"] == fixed_cid, "status"]) == {"causal"}
        node_ids = reg.data[reg.data["type"] == "node"].set_index("source")["comp_id"]
        for _, model in df.groupby("model_id"):
            assert {node_ids["X"], node_ids["Y"]} <= set(model["comp_id"])

        with pytest.raises(StateError, match="fixed causal edge"):
            ModelStateExpander.expand(
                reg,
                mode="sampled",
                n_models=1,
                edge_statuses=["unknown", "non-causal"],
            )

    def test_fixed_causal_edge_normalizes_omitted_seed_and_rejects_conflict(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [{"name": "X"}, {"name": "Y"}], exposure="X", outcome="Y"
        )
        node_cid = reg.data.loc[reg.data["source"] == "X", "comp_id"].iloc[0]
        fixed_cid = reg.data.loc[reg.data["fixed_status"] == "causal", "comp_id"].iloc[
            0
        ]
        records = ModelStateExpander.expand(
            reg,
            mode="sampled",
            n_models=1,
            seed_claims=[
                {"model_id": "seed", "comp_id": node_cid, "status": "present"}
            ],
        )
        seeded = pd.DataFrame(records).query("model_id == 'seed'")
        assert seeded.loc[seeded["comp_id"] == fixed_cid, "status"].iloc[0] == "causal"

        with pytest.raises(StateError, match="fixed edge"):
            ModelStateExpander.expand(
                reg,
                mode="sampled",
                n_models=1,
                seed_claims=[
                    {"model_id": "seed", "comp_id": fixed_cid, "status": "non-causal"}
                ],
            )

    def test_timing_options_retain_mutable_directed_edges_per_model(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing_options": [1, 3]},
                {"name": "Y", "timing_options": [2]},
            ],
            respect_timing=False,
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X": [1, 3], "Y": [2]},
        )
        df = pd.DataFrame(records)
        edge_names = reg.data.loc[reg.data["type"] == "edge"].set_index("comp_id")[
            ["source", "target"]
        ]

        assert records.pruning_report["projected_model_count"] == 12
        assert records.pruning_report["temporal_edge_statuses_pruned"] == 2
        assert records.pruning_report["temporal_edge_assignments_pruned"] == 0
        assert len(df["model_id"].unique()) == 12
        for _, model in df.groupby("model_id"):
            timings = model.set_index("comp_id")["timing"].to_dict()
            for edge_cid in model.loc[
                model["comp_id"].isin(edge_names.index), "comp_id"
            ]:
                source, target = edge_names.loc[edge_cid]
                source_cid = reg.data.loc[
                    (reg.data["type"] == "node") & (reg.data["source"] == source),
                    "comp_id",
                ].iloc[0]
                target_cid = reg.data.loc[
                    (reg.data["type"] == "node") & (reg.data["source"] == target),
                    "comp_id",
                ].iloc[0]
                status = model.loc[model["comp_id"] == edge_cid, "status"].iloc[0]
                if timings[source_cid] < timings[target_cid]:
                    assert status in {"causal", "unknown", "non-causal"}
                else:
                    assert status in {"unknown", "non-causal"}

    def test_flexible_timing_expansion_closes_global_crux_universe(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X1", "timing": 2},
                {"name": "X2", "timing_options": [1, 2]},
                {"name": "Y", "timing": 3},
            ],
            respect_timing=False,
            exposure="X1",
            outcome="Y",
            constraints=[
                {
                    "source": "X2",
                    "target": "X1",
                    "direction": "->",
                    "rule": "allow",
                },
                {
                    "source": "X2",
                    "target": "Y",
                    "direction": "->",
                    "rule": "allow",
                },
            ],
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X1": [2], "X2": [1, 2], "Y": [3]},
        )
        df = pd.DataFrame(records)
        target_cid = reg.data.loc[
            (reg.data["source"] == "X2") & (reg.data["target"] == "X1"),
            "comp_id",
        ].iloc[0]
        x2_cid = reg.data.loc[reg.data["source"] == "X2", "comp_id"].iloc[0]

        assert target_cid == "C0005"
        assert len(df["model_id"].unique()) == 15
        assert records.pruning_report["projected_model_count"] == 15
        for _, model in df.groupby("model_id"):
            assert set(model["comp_id"]) == set(reg.data["comp_id"])

        timing_two_ids = sorted(
            df.loc[(df["comp_id"] == x2_cid) & (df["timing"] == 2), "model_id"].unique()
        )
        assert len(timing_two_ids) == 6
        assert set(
            df.loc[
                df["model_id"].isin(timing_two_ids) & (df["comp_id"] == target_cid),
                "status",
            ]
        ) == {"unknown", "non-causal"}

        state = StateTensor.from_records(reg, list(records))
        dyads = DyadicEngine().compare_pairs(state, reg, mode="basic")
        ranking = DeltaUEngine(crux_mode="global").rank_lynchpins(
            state, dyads, reg, top_k=10
        )
        row = next(item for item in ranking if item["component_id"] == target_cid)

        assert row["mapping_coverage_causal"] == 1.0
        assert row["mapping_coverage_non_causal"] == 1.0
        assert row["timing_pruned_models_causal"] == timing_two_ids
        assert row["models_pruned_causal"] == 6
        assert row["timing_pruned_models_non_causal"] == []
        assert row["models_pruned_non_causal"] == 0
        assert row["post_model_count_causal"] == 9
        assert row["post_model_count_non_causal"] == 15

    def test_timing_options_prune_fixed_edge_assignments_and_report_them(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [
                {"name": "X", "timing_options": [1, 3]},
                {"name": "Y", "timing_options": [2]},
            ],
            exposure="X",
            outcome="Y",
        )
        fixed_cid = reg.data.loc[reg.data["fixed_status"] == "causal", "comp_id"].iloc[
            0
        ]
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X": [1, 3], "Y": [2]},
        )
        df = pd.DataFrame(records)

        assert records.pruning_report["timing_assignments_considered"] == 2
        assert records.pruning_report["required_edge_assignments_pruned"] == 1
        assert records.pruning_report["temporal_edge_statuses_pruned"] == 1
        assert set(df["model_id"]) == {"M0001", "M0002"}
        assert set(df.loc[df["comp_id"] == fixed_cid, "status"]) == {"causal"}
        reverse_cid = reg.data.loc[
            (reg.data["source"] == "Y") & (reg.data["target"] == "X"), "comp_id"
        ].iloc[0]
        assert set(df.loc[df["comp_id"] == reverse_cid, "status"]) == {
            "unknown",
            "non-causal",
        }

    def test_projected_limit_counts_timing_and_direction_specific_states(self):
        reg = RegistryLoader.from_records(
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
                    "description": "X -> Y",
                },
                {
                    "comp_id": "C0004",
                    "type": "edge",
                    "source": "X",
                    "target": "Y",
                    "direction": "<->",
                    "description": "X <-> Y",
                },
            ]
        )
        options = {"X": [1, 3], "Y": [2]}

        with pytest.raises(StateError, match="exceeding max_models"):
            ModelStateExpander.expand(
                reg,
                mode="exhaustive",
                timing_options=options,
                max_models=9,
            )

        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options=options,
            max_models=10,
        )
        assert records.pruning_report["projected_model_count"] == 10
        assert len({record["model_id"] for record in records}) == 10

    def test_optional_nodes_vary_only_named_nodes(self):
        reg = ComponentRegistryBuilder.from_nodes(
            [{"name": "X"}, {"name": "Y"}, {"name": "Z"}],
            exposure="X",
            outcome="Y",
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            optional_nodes=["X", "Z"],
        )
        df = pd.DataFrame(records)
        node_ids = reg.data.loc[reg.data["type"] == "node"].set_index("source")[
            "comp_id"
        ]
        present_sets = {
            frozenset(model.loc[model["comp_id"].isin(node_ids), "comp_id"])
            for _, model in df.groupby("model_id")
        }

        assert present_sets == {
            frozenset({node_ids["X"], node_ids["Y"]}),
            frozenset({node_ids["X"], node_ids["Y"], node_ids["Z"]}),
        }

    def test_seed_matching_uses_bidirected_states_and_claimed_timing(self):
        reg = RegistryLoader.from_records(
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
                    "direction": "<->",
                    "description": "X <-> Y",
                },
            ]
        )
        records = ModelStateExpander.expand(
            reg,
            mode="exhaustive",
            timing_options={"X": [1, 2], "Y": [3]},
            bidirected_statuses=["present"],
            seed_claims=[
                {
                    "model_id": "prior",
                    "comp_id": "C0001",
                    "status": "present",
                    "timing": 2,
                },
                {
                    "model_id": "prior",
                    "comp_id": "C0002",
                    "status": "present",
                    "timing": 3,
                },
                {"model_id": "prior", "comp_id": "C0003", "status": "present"},
            ],
        )
        prior = pd.DataFrame(records).query("model_id == 'prior'")
        assert prior.loc[prior["comp_id"] == "C0001", "timing"].iloc[0] == 2
        assert prior.loc[prior["comp_id"] == "C0003", "status"].iloc[0] == "present"


class TestStateToSparseVectorized:

    def test_vectorized_matches_existing_encoding(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001"])
        state.set_status("M0001", "C0001", "causal")
        state.set_status("M0001", "C0002", "non-causal")
        sparse = state_to_sparse(state).toarray()
        assert sparse[0, 0] == 1
        assert sparse[0, 1] == 2
        assert sparse[0, 2] == 0

    def test_vectorized_shape_is_m_by_n(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        sparse = state_to_sparse(state)
        assert sparse.shape == (2, 3)

    def test_vectorized_after_status_mutation(self):
        reg = _make_registry()
        state = StateTensor.create(reg, ["M0001", "M0002"])
        state.set_status("M0001", "C0001", "causal")
        sparse1 = state_to_sparse(state).toarray()
        state.set_status("M0001", "C0001", "non-causal")
        sparse2 = state_to_sparse(state).toarray()
        assert sparse1[0, 0] == 1
        assert sparse2[0, 0] == 2

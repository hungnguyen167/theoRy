from __future__ import annotations

import pandas as pd
import pytest

from registry.builder import ComponentRegistryBuilder
from symbolic.universe import EdgeVar, SymbolicUniverse, build_symbolic_universe


class TestBuildSymbolicUniverse:
    @pytest.mark.parametrize("invalid_timing", [0, -1])
    def test_timing_inputs_must_be_at_least_one(self, invalid_timing):
        with pytest.raises(ValueError, match="at least 1"):
            build_symbolic_universe(
                nodes=["X", "Y"],
                timing={"X": invalid_timing, "Y": 1},
                exposure="X",
                outcome="Y",
            )

    def test_from_nodes(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        assert u.exposure == "X"
        assert u.outcome == "Y"
        assert len(u.edge_vars) == 2
        assert ("X", "Y") in u.edge_vars
        assert ("A", "Y") in u.edge_vars

    def test_variable_names(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        assert u.variable_names == ["e__X__Y"]
        assert u.edge_count == 1

    def test_edge_var_for(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        ev = u.edge_var_for("X", "Y")
        assert ev is not None
        assert ev.name == "e__X__Y"
        assert ev.source == "X"
        assert ev.target == "Y"

    def test_no_edges_same_timing(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": 1, "Y": 1},
            exposure="X",
            outcome="Y",
        )
        assert u.edge_count == 0

    def test_all_missing_timing_creates_only_fixed_exposure_to_outcome(self):
        u = build_symbolic_universe(
            nodes=["X", "Z", "Y"],
            timing={"X": None, "Z": None, "Y": None},
            exposure="X",
            outcome="Y",
        )
        assert set(u.edge_vars) == {("X", "Y")}
        assert u.fixed_causal_edges == {("X", "Y")}

    def test_registry_fixed_status_is_preserved(self):
        registry = ComponentRegistryBuilder.from_nodes(
            [{"name": "X"}, {"name": "Y"}], exposure="X", outcome="Y"
        )
        u = build_symbolic_universe(
            registry=pd.DataFrame(registry.data), exposure="X", outcome="Y"
        )
        assert u.fixed_causal_edges == {("X", "Y")}

    @pytest.mark.parametrize(
        "exposure, outcome, match",
        [
            ("X", "", "Both or neither"),
            ("X", "X", "distinct"),
            ("X", "Z", "supplied node"),
        ],
    )
    def test_direct_universe_rejects_invalid_causal_target(
        self, exposure, outcome, match
    ):
        with pytest.raises(ValueError, match=match):
            build_symbolic_universe(
                nodes=["X", "Y"],
                timing={"X": None, "Y": None},
                exposure=exposure,
                outcome=outcome,
            )


class TestSymbolicUniverse:
    def test_comp_id_for(self):
        ev = EdgeVar(name="e__A__B", source="A", target="B", comp_id="C0001")
        u = SymbolicUniverse(
            nodes=("A", "B"),
            timing={"A": 1, "B": 2},
            exposure="A",
            outcome="B",
            edge_vars={("A", "B"): ev},
            comp_to_edge={"C0001": ("A", "B")},
        )
        assert u.comp_id_for("A", "B") == "C0001"

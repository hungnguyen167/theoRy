from __future__ import annotations

from symbolic.constraints import (
    constraints_from_dag_spec,
    constraints_from_edge_statuses,
    fixed_edge_constraints,
    sparsity_constraints,
)
from symbolic.backend import BruteForceBackend
from symbolic.universe import build_symbolic_universe


class TestConstraints:
    def test_dag_spec_closed_world(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        dag = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        f = constraints_from_dag_spec(dag, u, unmentioned_edges="non-causal")
        assert f is not None
        vars_set = f.variables()
        assert "e__X__Y" in vars_set
        assert "e__A__Y" in vars_set

    def test_dag_spec_open_world(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        dag = {
            "nodes": ["X", "Y", "A"],
            "edges": [["X", "Y"]],
            "exposure": "X",
            "outcome": "Y",
        }
        f = constraints_from_dag_spec(dag, u, unmentioned_edges="unknown")
        assert f is not None
        vars_set = f.variables()
        assert "e__X__Y" in vars_set
        assert "e__A__Y" not in vars_set

    def test_edge_statuses(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        records = [{"comp_id": "S0001", "status": "causal"}]
        f = constraints_from_edge_statuses(records, u)
        assert f is not None

    def test_sparsity(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A", "B"],
            timing={"X": 1, "A": 1, "B": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        f = sparsity_constraints(u, max_edges=1)
        assert f is not None

    def test_sparsity_max_indegree(self):
        u = build_symbolic_universe(
            nodes=["X", "Y", "A"],
            timing={"X": 1, "A": 1, "Y": 2},
            exposure="X",
            outcome="Y",
        )
        f = sparsity_constraints(u, max_indegree=1)
        assert f is not None

    def test_fixed_edge_constraint_and_closed_world_dag_keep_edge_causal(self):
        u = build_symbolic_universe(
            nodes=["X", "Y"],
            timing={"X": None, "Y": None},
            exposure="X",
            outcome="Y",
        )
        backend = BruteForceBackend(max_vars=20)

        assert backend.count(fixed_edge_constraints(u), u.variable_names) == 1
        closed_world = constraints_from_dag_spec(
            {"nodes": ["X", "Y"], "edges": [], "exposure": "X", "outcome": "Y"},
            u,
            unmentioned_edges="non-causal",
        )
        assert backend.count(closed_world, u.variable_names) == 1

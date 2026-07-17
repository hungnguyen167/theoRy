from __future__ import annotations

from dataclasses import dataclass, field

import pandas as pd


@dataclass(frozen=True)
class EdgeVar:
    name: str
    source: str
    target: str
    comp_id: str | None = None


@dataclass
class SymbolicUniverse:
    nodes: tuple[str, ...]
    timing: dict[str, int | None]
    exposure: str
    outcome: str
    edge_vars: dict[tuple[str, str], EdgeVar]
    comp_to_edge: dict[str, tuple[str, str]] = field(default_factory=dict)
    fixed_causal_edges: set[tuple[str, str]] = field(default_factory=set)
    observed_nodes: frozenset[str] = field(default_factory=frozenset)

    @property
    def variable_names(self) -> list[str]:
        return [ev.name for ev in self.edge_vars.values()]

    @property
    def edge_count(self) -> int:
        return len(self.edge_vars)

    def edge_var_for(self, source: str, target: str) -> EdgeVar | None:
        return self.edge_vars.get((source, target))

    def comp_id_for(self, source: str, target: str) -> str | None:
        ev = self.edge_var_for(source, target)
        return ev.comp_id if ev else None


def build_symbolic_universe(
    registry: pd.DataFrame | None = None,
    *,
    nodes: list[str] | None = None,
    timing: dict[str, int | None] | None = None,
    exposure: str | None = None,
    outcome: str | None = None,
    comp_to_timing: dict[str, int | None] | None = None,
) -> SymbolicUniverse:
    if registry is not None:
        return _from_registry(registry, exposure, outcome)
    if nodes is None or timing is None or exposure is None or outcome is None:
        raise ValueError(
            "Must provide either registry or (nodes + timing + exposure + outcome)"
        )
    return _from_nodes(nodes, timing, exposure, outcome, comp_to_timing)


def _from_registry(
    registry: pd.DataFrame, exposure: str | None, outcome: str | None
) -> SymbolicUniverse:
    node_rows = registry[registry["type"] == "node"]
    edge_rows = registry[
        (registry["type"] == "edge") & (registry.get("direction") == "->")
    ]

    all_nodes = tuple(sorted(node_rows["source"].unique()))
    observed_nodes = frozenset(
        row["source"]
        for _, row in node_rows.iterrows()
        if row.get("observed", True) is not False
    )
    timing: dict[str, int | None] = {}
    for _, row in node_rows.iterrows():
        node = row["source"]
        if node not in timing:
            timing[node] = None

    if exposure is None or outcome is None:
        exposure = exposure or ""
        outcome = outcome or ""

    edge_vars: dict[tuple[str, str], EdgeVar] = {}
    comp_to_edge: dict[str, tuple[str, str]] = {}
    fixed_causal_edges: set[tuple[str, str]] = set()
    for _, row in edge_rows.iterrows():
        src = row["source"]
        tgt = row["target"]
        cid = row["comp_id"]
        name = _edge_var_name(src, tgt)
        ev = EdgeVar(name=name, source=src, target=tgt, comp_id=cid)
        edge_vars[(src, tgt)] = ev
        comp_to_edge[cid] = (src, tgt)
        if "fixed_status" in registry.columns and row.get("fixed_status") == "causal":
            fixed_causal_edges.add((src, tgt))

    return SymbolicUniverse(
        nodes=all_nodes,
        timing=timing,
        exposure=exposure,
        outcome=outcome,
        edge_vars=edge_vars,
        comp_to_edge=comp_to_edge,
        fixed_causal_edges=fixed_causal_edges,
        observed_nodes=observed_nodes,
    )


def _from_nodes(
    nodes: list[str],
    timing: dict[str, int | None],
    exposure: str,
    outcome: str,
    comp_to_timing: dict[str, int | None] | None = None,
) -> SymbolicUniverse:
    if (not exposure) != (not outcome):
        raise ValueError("Both or neither of exposure and outcome must be provided")
    if exposure and outcome:
        if exposure == outcome:
            raise ValueError("Exposure and outcome must be distinct nodes")
        invalid_targets = {exposure, outcome} - set(nodes)
        if invalid_targets:
            raise ValueError(
                "Exposure/outcome must be supplied node names: "
                + ", ".join(sorted(invalid_targets))
            )

    sorted_nodes = sorted(
        nodes, key=lambda n: (timing.get(n) is None, timing.get(n, 999))
    )
    edge_vars: dict[tuple[str, str], EdgeVar] = {}
    comp_to_edge: dict[str, tuple[str, str]] = {}
    fixed_causal_edges: set[tuple[str, str]] = set()
    comp_idx = 0
    all_timing_none = all(timing.get(n) is None for n in nodes)

    if all_timing_none and exposure and outcome and exposure != outcome:
        name = _edge_var_name(exposure, outcome)
        comp_idx += 1
        cid = f"S{comp_idx:04d}"
        ev = EdgeVar(name=name, source=exposure, target=outcome, comp_id=cid)
        edge_vars[(exposure, outcome)] = ev
        comp_to_edge[cid] = (exposure, outcome)
        fixed_causal_edges.add((exposure, outcome))

    if not all_timing_none:
        for i, src in enumerate(sorted_nodes):
            src_t = timing.get(src)
            for j, tgt in enumerate(sorted_nodes):
                if i == j:
                    continue
                tgt_t = timing.get(tgt)
                if src_t is not None and tgt_t is not None and src_t < tgt_t:
                    name = _edge_var_name(src, tgt)
                    comp_idx += 1
                    cid = f"S{comp_idx:04d}"
                    ev = EdgeVar(name=name, source=src, target=tgt, comp_id=cid)
                    edge_vars[(src, tgt)] = ev
                    comp_to_edge[cid] = (src, tgt)

    return SymbolicUniverse(
        nodes=tuple(sorted(nodes)),
        timing=dict(timing),
        exposure=exposure,
        outcome=outcome,
        edge_vars=edge_vars,
        comp_to_edge=comp_to_edge,
        fixed_causal_edges=fixed_causal_edges,
        observed_nodes=frozenset(nodes),
    )


def _edge_var_name(source: str, target: str) -> str:
    s = source.replace("-", "_").replace(" ", "_")
    t = target.replace("-", "_").replace(" ", "_")
    return f"e__{s}__{t}"

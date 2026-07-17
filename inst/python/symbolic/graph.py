from __future__ import annotations

from symbolic.universe import SymbolicUniverse


def assignment_to_edges(
    universe: SymbolicUniverse, assignment: dict[str, bool]
) -> list[tuple[str, str]]:
    edges: list[tuple[str, str]] = []
    for (src, tgt), ev in universe.edge_vars.items():
        if assignment.get(ev.name, False):
            edges.append((src, tgt))
    return edges


def derived_present_nodes(
    universe: SymbolicUniverse, edges: list[tuple[str, str]]
) -> set[str]:
    present: set[str] = {universe.exposure, universe.outcome}
    for src, tgt in edges:
        present.add(src)
        present.add(tgt)
    return present


def dag_spec_from_assignment(
    universe: SymbolicUniverse, assignment: dict[str, bool]
) -> dict:
    edges = assignment_to_edges(universe, assignment)
    return {
        "nodes": list(universe.nodes),
        "edges": [[s, t] for s, t in edges],
        "exposure": universe.exposure,
        "outcome": universe.outcome,
    }

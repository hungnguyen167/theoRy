from __future__ import annotations

from symbolic.formula import And, Formula, Not, Or, Var
from symbolic.universe import SymbolicUniverse


def fixed_edge_constraints(universe: SymbolicUniverse) -> Formula:
    terms: list[Formula] = []
    for src, tgt in universe.fixed_causal_edges:
        ev = universe.edge_var_for(src, tgt)
        if ev:
            terms.append(Var(ev.name))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    return And(*terms)


def constraints_from_edge_statuses(
    records: list[dict], universe: SymbolicUniverse
) -> Formula:
    terms: list[Formula] = []
    for rec in records:
        comp_id = rec.get("comp_id")
        status = rec.get("status")
        if status == "causal":
            edge = universe.comp_to_edge.get(comp_id)
            if edge:
                ev = universe.edge_var_for(*edge)
                if ev:
                    terms.append(Var(ev.name))
        elif status == "non-causal":
            edge = universe.comp_to_edge.get(comp_id)
            if edge:
                ev = universe.edge_var_for(*edge)
                if ev:
                    terms.append(Not(Var(ev.name)))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    return And(*terms)


def constraints_from_dag_spec(
    dag_spec: dict,
    universe: SymbolicUniverse,
    *,
    unmentioned_edges: str = "non-causal",
) -> Formula:
    listed = {tuple(edge) for edge in dag_spec.get("edges", [])}
    listed |= universe.fixed_causal_edges
    terms: list[Formula] = []
    for edge, ev in universe.edge_vars.items():
        if edge in listed:
            terms.append(Var(ev.name))
        elif unmentioned_edges == "non-causal":
            terms.append(Not(Var(ev.name)))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    return And(*terms)


def sparsity_constraints(
    universe: SymbolicUniverse,
    *,
    max_edges: int | None = None,
    max_indegree: int | None = None,
    max_outdegree: int | None = None,
) -> Formula:
    terms: list[Formula] = []
    if max_edges is not None:
        terms.append(_cardinality_le(list(universe.edge_vars.values()), max_edges))
    if max_indegree is not None:
        for tgt in universe.nodes:
            incoming = [ev for (s, t), ev in universe.edge_vars.items() if t == tgt]
            if incoming:
                terms.append(_cardinality_le(incoming, max_indegree))
    if max_outdegree is not None:
        for src in universe.nodes:
            outgoing = [ev for (s, t), ev in universe.edge_vars.items() if s == src]
            if outgoing:
                terms.append(_cardinality_le(outgoing, max_outdegree))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    return And(*terms)


def node_absence_constraints(
    universe: SymbolicUniverse, absent_nodes: set[str]
) -> Formula:
    terms: list[Formula] = []
    for node in absent_nodes:
        for (s, t), ev in universe.edge_vars.items():
            if s == node or t == node:
                terms.append(Not(Var(ev.name)))
    if not terms:
        from symbolic.formula import TRUE

        return TRUE
    return And(*terms)


def _cardinality_le(edge_vars: list, k: int) -> Formula:
    from symbolic.formula import FALSE

    vars_list = [Var(ev.name) for ev in edge_vars]
    n = len(vars_list)
    if k >= n:
        from symbolic.formula import TRUE

        return TRUE
    if k < 0:
        return FALSE
    return _at_most_k(vars_list, k)


def _at_most_k(vars: list[Formula], k: int) -> Formula:
    n = len(vars)
    if k >= n:
        from symbolic.formula import TRUE

        return TRUE
    if k < 0:
        from symbolic.formula import FALSE

        return FALSE

    clauses: list[Formula] = []
    for combo in _combinations(range(n), k + 1):
        clause = Or(*[Not(vars[i]) for i in combo])
        clauses.append(clause)
    return And(*clauses)


def _combinations(items: list, r: int):
    if r == 0:
        yield []
        return
    for i in range(len(items)):
        for rest in _combinations(items[i + 1 :], r - 1):
            yield [items[i]] + rest

from __future__ import annotations

import itertools

from symbolic.formula import And, Formula, Not, Or, Var, FALSE, TRUE
from symbolic.reachability import ReachabilityIndex
from symbolic.universe import SymbolicUniverse


def candidate_adjustment_sets(
    universe: SymbolicUniverse,
    policy: str = "all_non_endpoints",
) -> list[frozenset[str]]:
    observed = universe.observed_nodes or frozenset(universe.nodes)
    candidates: list[str] = [
        n
        for n in universe.nodes
        if n in observed and n not in (universe.exposure, universe.outcome)
    ]
    if policy == "all_non_endpoints":
        result: list[frozenset[str]] = []
        for r in range(len(candidates) + 1):
            for combo in itertools.combinations(candidates, r):
                result.append(frozenset(combo))
        return result
    return [frozenset()]


def enumerate_simple_paths(
    universe: SymbolicUniverse,
    source: str,
    target: str,
    max_len: int = 8,
    max_paths: int | None = None,
) -> list[list[str]]:
    """Enumerate undirected simple paths in the skeleton from source to target."""
    paths: list[list[str]] = []

    def _dfs(current: str, path: list[str], visited: set[str]):
        if max_paths is not None and len(paths) >= max_paths:
            return
        if len(path) > max_len:
            return
        if current == target and len(path) > 1:
            paths.append(list(path))
            return
        for s, t in universe.edge_vars:
            if s == current and t not in visited:
                visited.add(t)
                path.append(t)
                _dfs(t, path, visited)
                path.pop()
                visited.remove(t)
            elif t == current and s not in visited:
                visited.add(s)
                path.append(s)
                _dfs(s, path, visited)
                path.pop()
                visited.remove(s)

    _dfs(source, [source], {source})
    return paths


def is_backdoor_path(universe: SymbolicUniverse, path: list[str]) -> bool:
    """A backdoor path starts with an edge pointing INTO the exposure X (path[0])."""
    if len(path) < 2:
        return False
    x = path[0]
    adjacent = path[1]
    return (adjacent, x) in universe.edge_vars


def edge_var_for_segment(universe: SymbolicUniverse, u: str, v: str) -> Formula:
    """Return the edge variable Formula for the segment u-v (either direction)."""
    if (u, v) in universe.edge_vars:
        return Var(universe.edge_vars[(u, v)].name)
    if (v, u) in universe.edge_vars:
        return Var(universe.edge_vars[(v, u)].name)
    return FALSE


def path_exists_formula(universe: SymbolicUniverse, path: list[str]) -> Formula:
    """Formula that is true iff all edges along the path are present."""
    terms: list[Formula] = []
    for i in range(len(path) - 1):
        seg = edge_var_for_segment(universe, path[i], path[i + 1])
        if seg is FALSE:
            return FALSE
        terms.append(seg)
    return And(*terms)


def is_collider_on_path(universe: SymbolicUniverse, path: list[str], idx: int) -> bool:
    """Check if path[idx] is a collider on the path (both adjacent edges point into it).

    In a temporal DAG, edge (a,b) in edge_vars means timing[a]<timing[b], so the
    edge (if present) goes a->b (into b). A collider at B requires both neighbours
    to have edges pointing INTO B.
    """
    node = path[idx]
    before = path[idx - 1]
    after = path[idx + 1]
    return (before, node) in universe.edge_vars and (after, node) in universe.edge_vars


def collider_nodes_on_path(
    universe: SymbolicUniverse, path: list[str]
) -> tuple[str, ...]:
    """Return the collider interior nodes on a path."""
    return tuple(
        path[i]
        for i in range(1, len(path) - 1)
        if is_collider_on_path(universe, path, i)
    )


def noncollider_nodes_on_path(
    universe: SymbolicUniverse, path: list[str]
) -> tuple[str, ...]:
    """Return the non-collider interior nodes on a path."""
    return tuple(
        path[i]
        for i in range(1, len(path) - 1)
        if not is_collider_on_path(universe, path, i)
    )


def path_open_formula(
    universe: SymbolicUniverse,
    reachability: ReachabilityIndex,
    path: list[str],
    Z: frozenset[str],
) -> Formula:
    """Return a Formula that is true iff the path is d-connecting given Z.

    For each interior node:
      - non-collider: open iff node NOT in Z (boolean; blocks if conditioned)
      - collider: open iff node in Z OR some descendant of node is in Z
        (descendant reachability is symbolic when edges are uncertain)
    """
    terms: list[Formula] = []
    for i in range(1, len(path) - 1):
        node = path[i]
        if is_collider_on_path(universe, path, i):
            if node in Z:
                continue
            desc_terms: list[Formula] = []
            for z in Z:
                rf = reachability.is_descendant_formula(node, z)
                if rf is not FALSE:
                    desc_terms.append(rf)
            if not desc_terms:
                return FALSE
            terms.append(Or(*desc_terms))
        else:
            if node in Z:
                return FALSE
    if not terms:
        return TRUE
    return And(*terms)


def forbidden_node_formula(
    universe: SymbolicUniverse,
    reachability: ReachabilityIndex,
    Z: frozenset[str],
) -> Formula:
    """Formula ensuring Z contains no descendants of X (forbidden for adjustment)."""
    X = universe.exposure
    desc_formulas: list[Formula] = []
    for z in Z:
        rf = reachability.is_descendant_formula(X, z)
        if rf is not FALSE:
            desc_formulas.append(Not(rf))
    if not desc_formulas:
        return TRUE
    return And(*desc_formulas)


def backdoor_path_formulas(
    universe: SymbolicUniverse,
    reachability: ReachabilityIndex | None = None,
    max_path_len: int = 8,
) -> list[tuple[list[str], Formula]]:
    """Return (path, path_exists_formula) for each backdoor path from X to Y."""
    X = universe.exposure
    Y = universe.outcome
    paths = enumerate_simple_paths(universe, X, Y, max_path_len)
    formulas: list[tuple[list[str], Formula]] = []
    for path in paths:
        if not is_backdoor_path(universe, path):
            continue
        pe = path_exists_formula(universe, path)
        if pe is FALSE:
            continue
        formulas.append((path, pe))
    return formulas


def valid_adjustment_formula(
    universe: SymbolicUniverse,
    reachability: ReachabilityIndex,
    Z: frozenset[str],
    max_path_len: int = 8,
) -> Formula:
    """Formula true iff Z is a valid backdoor adjustment set for X -> Y."""
    X = universe.exposure
    Y = universe.outcome

    if X in Z or Y in Z:
        return FALSE

    forbidden = forbidden_node_formula(universe, reachability, Z)
    if forbidden is FALSE:
        return FALSE

    bd_paths = backdoor_path_formulas(universe, reachability, max_path_len)
    if not bd_paths:
        return forbidden

    open_path_formulas: list[Formula] = []
    for path, pe in bd_paths:
        po = path_open_formula(universe, reachability, path, Z)
        if po is FALSE:
            continue
        open_path_formulas.append(And(pe, po))

    if not open_path_formulas:
        return forbidden

    return And(Not(Or(*open_path_formulas)), forbidden)


def valid_adjustment_formula_from_paths(
    universe: SymbolicUniverse,
    reachability: ReachabilityIndex,
    backdoor_paths: list[tuple[list[str], Formula]],
    Z: frozenset[str],
) -> Formula:
    """Formula true iff Z is a valid backdoor adjustment set, using precomputed paths.

    This is the efficient version that reuses cached backdoor path formulas
    instead of re-enumerating paths on every call.
    """
    X = universe.exposure
    Y = universe.outcome

    if X in Z or Y in Z:
        return FALSE

    forbidden = forbidden_node_formula(universe, reachability, Z)
    if forbidden is FALSE:
        return FALSE

    if not backdoor_paths:
        return forbidden

    open_path_formulas: list[Formula] = []
    for path, pe in backdoor_paths:
        po = path_open_formula(universe, reachability, path, Z)
        if po is FALSE:
            continue
        open_path_formulas.append(And(pe, po))

    if not open_path_formulas:
        return forbidden

    return And(Not(Or(*open_path_formulas)), forbidden)


def adjustment_identifiable_formula(
    universe: SymbolicUniverse,
    candidate_sets: list[frozenset[str]] | None = None,
    reachability: ReachabilityIndex | None = None,
    max_path_len: int = 8,
) -> Formula:
    """Formula true iff there exists a valid adjustment set for X -> Y."""
    if candidate_sets is None:
        candidate_sets = candidate_adjustment_sets(universe)
    if reachability is None:
        reachability = ReachabilityIndex(universe)

    formulas: list[Formula] = []
    for Z in candidate_sets:
        formulas.append(
            valid_adjustment_formula(universe, reachability, Z, max_path_len)
        )
    return Or(*formulas)


# Backward-compatible aliases for private names used in tests
_simple_paths = enumerate_simple_paths
_is_backdoor = is_backdoor_path
_edge_var_for_segment = edge_var_for_segment
_path_exists_formula = path_exists_formula
_is_collider_on_path = is_collider_on_path
_path_open_formula = path_open_formula
_forbidden_node_formula = forbidden_node_formula

from __future__ import annotations

from dataclasses import dataclass

from symbolic.dsep import (
    candidate_adjustment_sets,
    collider_nodes_on_path,
    forbidden_node_formula,
    noncollider_nodes_on_path,
    path_exists_formula,
    valid_adjustment_formula_from_paths,
)
from symbolic.formula import Formula, Not, Or, FALSE, TRUE
from symbolic.reachability import ReachabilityIndex, build_reachability
from symbolic.universe import SymbolicUniverse


@dataclass(frozen=True)
class PathRecord:
    """Cached record for one path from X to Y."""

    path_id: str
    nodes: tuple[str, ...]
    is_backdoor: bool
    path_exists: Formula
    collider_nodes: tuple[str, ...]
    noncollider_nodes: tuple[str, ...]


@dataclass
class QueryFormulaIndex:
    """Precomputed formula index for all query-relevant formulas.

    Build once, reuse for class partitioning, delta-U, and simulations.
    """

    universe: SymbolicUniverse
    reachability: ReachabilityIndex
    candidate_sets: tuple[frozenset[str], ...]
    paths: tuple[PathRecord, ...]
    backdoor_paths: tuple[PathRecord, ...]
    backdoor_path_tuples: tuple[tuple[list[str], Formula], ...]
    valid_z: dict[frozenset[str], Formula]
    adjustment_identifiable: Formula
    empty_adjustment_valid: Formula
    open_backdoor_empty: Formula
    forbidden_by_z: dict[frozenset[str], Formula]


def build_query_formula_index(
    universe: SymbolicUniverse,
    *,
    candidate_policy: str = "all_non_endpoints",
    max_path_len: int = 8,
    max_paths: int | None = None,
) -> QueryFormulaIndex:
    """Build a QueryFormulaIndex that caches all query-relevant formulas.

    This is the primary entry point for Phase 1. It enumerates paths once
    and caches valid_adjustment_formula for every candidate Z set.
    """
    reachability = build_reachability(universe)
    X = universe.exposure
    Y = universe.outcome

    raw_paths = _enumerate_and_record(universe, X, Y, max_path_len, max_paths)

    all_path_records: list[PathRecord] = []
    backdoor_records: list[PathRecord] = []
    backdoor_tuples: list[tuple[list[str], Formula]] = []

    for i, (nodes_tuple, is_bd, pe, colliders, noncolliders) in enumerate(raw_paths):
        record = PathRecord(
            path_id=f"P{i:04d}",
            nodes=nodes_tuple,
            is_backdoor=is_bd,
            path_exists=pe,
            collider_nodes=colliders,
            noncollider_nodes=noncolliders,
        )
        all_path_records.append(record)
        if is_bd and pe is not FALSE:
            backdoor_records.append(record)
            backdoor_tuples.append((list(nodes_tuple), pe))

    candidate_sets = tuple(candidate_adjustment_sets(universe, candidate_policy))

    valid_z: dict[frozenset[str], Formula] = {}
    forbidden_by_z: dict[frozenset[str], Formula] = {}

    for Z in candidate_sets:
        forbidden_by_z[Z] = forbidden_node_formula(universe, reachability, Z)
        valid_z[Z] = valid_adjustment_formula_from_paths(
            universe, reachability, backdoor_tuples, Z
        )

    adj_id = Or(*valid_z.values()) if valid_z else FALSE
    empty_valid = valid_z.get(frozenset(), FALSE)
    open_backdoor_empty = Not(empty_valid) if empty_valid is not TRUE else FALSE

    return QueryFormulaIndex(
        universe=universe,
        reachability=reachability,
        candidate_sets=candidate_sets,
        paths=tuple(all_path_records),
        backdoor_paths=tuple(backdoor_records),
        backdoor_path_tuples=tuple(backdoor_tuples),
        valid_z=valid_z,
        adjustment_identifiable=adj_id,
        empty_adjustment_valid=empty_valid,
        open_backdoor_empty=open_backdoor_empty,
        forbidden_by_z=forbidden_by_z,
    )


def _enumerate_and_record(
    universe: SymbolicUniverse,
    source: str,
    target: str,
    max_path_len: int,
    max_paths: int | None,
) -> list[tuple[tuple[str, ...], bool, Formula, tuple[str, ...], tuple[str, ...]]]:
    """Enumerate paths and compute their properties in one pass.

    Returns list of (nodes_tuple, is_backdoor, path_exists_formula, collider_nodes, noncollider_nodes).
    """
    from symbolic.dsep import enumerate_simple_paths, is_backdoor_path

    raw = enumerate_simple_paths(universe, source, target, max_path_len, max_paths)

    results = []
    for path in raw:
        nodes_tuple = tuple(path)
        is_bd = is_backdoor_path(universe, path)
        pe = path_exists_formula(universe, path)
        colliders = collider_nodes_on_path(universe, path)
        noncolliders = noncollider_nodes_on_path(universe, path)
        results.append((nodes_tuple, is_bd, pe, colliders, noncolliders))

    return results

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class QuerySignature:
    adjustment_identifiable: bool | None = None
    possibly_adjustment_identifiable: bool | None = None
    necessarily_adjustment_identifiable: bool | None = None
    adjustment_identifiable_mass: int | float | None = None
    empty_adjustment_valid: bool | None = None
    projected_edges: frozenset[tuple[str, str]] | None = None
    derived_present_nodes: frozenset[str] | None = None


@dataclass(frozen=True)
class QueryClassSignature:
    class_id: str = ""
    mass: int | float = 0
    adjustment_identifiable: bool = False
    possibly_adjustment_identifiable: bool | None = None
    necessarily_adjustment_identifiable: bool | None = None
    adjustment_identifiable_mass: int | float | None = None
    valid_adjustment_exists: bool = False
    empty_adjustment_valid: bool = False
    confounding_signature: tuple[str, ...] = ()
    collider_relevance_signature: tuple[str, ...] = ()
    endpoint_relevant_nodes: tuple[str, ...] = ()
    projected_similarity_to_preferred: float | None = None


def query_signature_from_formula(
    adjustment_id_formula,
    empty_adjustment_formula,
    universe,
    backend,
    variables: list[str],
    constraints=None,
) -> QuerySignature:
    adj_id_count = backend.count(adjustment_id_formula, variables, constraints)
    total_count = (
        backend.count(_true_formula(), variables, constraints)
        if constraints
        else (1 << len(variables))
    )
    empty_valid = backend.satisfiable(empty_adjustment_formula, variables, constraints)

    return QuerySignature(
        adjustment_identifiable=adj_id_count > 0,
        necessarily_adjustment_identifiable=(
            adj_id_count == total_count if total_count > 0 else None
        ),
        possibly_adjustment_identifiable=adj_id_count > 0,
        adjustment_identifiable_mass=adj_id_count,
        empty_adjustment_valid=empty_valid,
    )


def _true_formula():
    from symbolic.formula import TRUE

    return TRUE

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from registry.schema import ComponentRegistry
from state.tensor import StateTensor


@dataclass(frozen=True)
class DyadContext:
    """Latest successful dyad computation for follow-on API calls."""

    registry: ComponentRegistry
    state: StateTensor
    dyads: list[dict[str, Any]]
    mode: str
    exposure: str | None = None
    outcome: str | None = None


_latest_dyad_context: DyadContext | None = None


def update_latest_dyad_context(
    *,
    registry: ComponentRegistry,
    state: StateTensor,
    dyads: list[dict[str, Any]],
    mode: str,
    exposure: str | None = None,
    outcome: str | None = None,
) -> None:
    """Store the latest dyad result for Epic 3 Delta-U workflows."""
    global _latest_dyad_context
    _latest_dyad_context = DyadContext(
        registry=registry,
        state=state,
        dyads=dyads,
        mode=mode,
        exposure=exposure,
        outcome=outcome,
    )


def get_latest_dyad_context() -> DyadContext | None:
    """Return the latest successful dyad computation, if one exists."""
    return _latest_dyad_context


def clear_latest_dyad_context() -> None:
    """Clear cached dyad context for isolated tests or server reset."""
    global _latest_dyad_context
    _latest_dyad_context = None


# --- Symbolic context ----------------------------------------------------------


@dataclass(frozen=True)
class SymbolicContext:
    universe: Any
    constraints: Any | None = None


_symbolic_context: SymbolicContext | None = None


def update_latest_symbolic_context(*, universe, constraints=None) -> None:
    global _symbolic_context
    _symbolic_context = SymbolicContext(universe=universe, constraints=constraints)


def get_latest_symbolic_context() -> SymbolicContext | None:
    return _symbolic_context


def clear_latest_symbolic_context() -> None:
    global _symbolic_context
    _symbolic_context = None

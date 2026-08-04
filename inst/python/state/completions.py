from __future__ import annotations

import itertools
import math
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from typing import Any

from registry.schema import ComponentRegistry


@dataclass(frozen=True)
class SemanticSignature:
    """Canonical, model-ID-independent state semantics."""

    node_presence: tuple[tuple[str, bool], ...]
    edge_statuses: tuple[tuple[str, str], ...]
    node_timing: tuple[tuple[str, int | None], ...]
    constraints: Any = ()


@dataclass(frozen=True)
class CompletionDiagnostics:
    model_id: str
    is_resolved: bool
    unknown_edge_ids: tuple[str, ...]
    completion_ids: tuple[str, ...]
    completion_count: int
    expected_completion_count: int
    completion_coverage_complete: bool
    completion_source: str
    missing_signatures: tuple[SemanticSignature, ...] = ()
    duplicate_resolved_state_count: int = 0


def _freeze(value: Any) -> Any:
    """Convert model constraints to a stable, hashable representation."""
    if isinstance(value, Mapping):
        return tuple(sorted((str(key), _freeze(item)) for key, item in value.items()))
    if isinstance(value, (list, tuple)):
        return tuple(_freeze(item) for item in value)
    if isinstance(value, (set, frozenset)):
        return tuple(sorted((_freeze(item) for item in value), key=repr))
    if isinstance(value, float) and math.isnan(value):
        return None
    try:
        hash(value)
    except TypeError:
        return repr(value)
    return value


class CompletionIndex:
    """Index existing resolved states and prove partial-state completion coverage."""

    def __init__(
        self,
        state,
        registry: ComponentRegistry,
        *,
        model_constraints: Mapping[str, Any] | Callable[[str], Any] | None = None,
    ) -> None:
        self.state = state
        self.registry = registry
        self._model_constraints = model_constraints

        rows = registry.data
        node_rows = rows[rows["type"] == "node"]
        edge_rows = rows[rows["type"] == "edge"]

        self._node_ids = tuple(sorted(node_rows["comp_id"].tolist()))
        self._node_id_by_name = {
            row["source"]: row["comp_id"] for _, row in node_rows.iterrows()
        }
        self._edges = {
            row["comp_id"]: {
                "source": row["source"],
                "target": row["target"],
                "direction": row["direction"],
                "fixed_status": row.get("fixed_status"),
            }
            for _, row in edge_rows.iterrows()
        }

        self._signature_by_model = {
            model_id: self._signature_for_model(model_id)
            for model_id in state.model_ids
        }
        self._resolved_by_signature: dict[SemanticSignature, list[str]] = {}
        self.models_by_signature: dict[SemanticSignature, list[str]] = {}
        for model_id in sorted(state.model_ids):
            signature = self._signature_by_model[model_id]
            self.models_by_signature.setdefault(signature, []).append(model_id)
            if self.is_resolved_signature(signature) and self.is_valid_signature(
                signature
            ):
                self._resolved_by_signature.setdefault(signature, []).append(model_id)

        self._expected_cache: dict[str, tuple[SemanticSignature, ...]] = {}
        self._diagnostics_cache: dict[str, CompletionDiagnostics] = {}

    def _constraints_for_model(self, model_id: str) -> Any:
        source = self._model_constraints
        if callable(source):
            return _freeze(source(model_id))
        if source is not None:
            return _freeze(source.get(model_id, ()))

        getter = getattr(self.state, "get_model_constraints", None)
        if callable(getter):
            return _freeze(getter(model_id))

        constraints = getattr(self.state, "model_constraints", None)
        if isinstance(constraints, Mapping):
            return _freeze(constraints.get(model_id, ()))
        return ()

    def _node_present(self, model_id: str, comp_id: str) -> bool:
        if hasattr(self.state, "node_present"):
            return bool(self.state.node_present(model_id, comp_id))
        return self.state.get_status(model_id, comp_id) in ("present", "causal")

    def _edge_applicable(self, model_id: str, edge_id: str) -> bool:
        if hasattr(self.state, "edge_applicable"):
            return bool(self.state.edge_applicable(model_id, edge_id))
        edge = self._edges[edge_id]
        source_id = self._node_id_by_name.get(edge["source"])
        target_id = self._node_id_by_name.get(edge["target"])
        return bool(
            source_id
            and target_id
            and self._node_present(model_id, source_id)
            and self._node_present(model_id, target_id)
        )

    def _signature_for_model(self, model_id: str) -> SemanticSignature:
        node_presence = tuple(
            (comp_id, self._node_present(model_id, comp_id))
            for comp_id in self._node_ids
        )
        edge_statuses = tuple(
            (edge_id, self.state.get_status(model_id, edge_id))
            for edge_id in sorted(self._edges)
            if self._edge_applicable(model_id, edge_id)
        )
        node_timing = tuple(
            (
                comp_id,
                (
                    self.state.get_timing(model_id, comp_id)
                    if hasattr(self.state, "get_timing")
                    else None
                ),
            )
            for comp_id in self._node_ids
        )
        return SemanticSignature(
            node_presence=node_presence,
            edge_statuses=edge_statuses,
            node_timing=node_timing,
            constraints=self._constraints_for_model(model_id),
        )

    def semantic_signature(self, model_id: str) -> SemanticSignature:
        return self._signature_by_model[model_id]

    # A short alias is convenient for cache consumers.
    signature = semantic_signature

    def signature_after_resolution(
        self,
        model_id: str,
        assignments: dict[str, str],
    ) -> SemanticSignature:
        """Return the semantic signature with hypothetical edge assignments.

        Only edges that are applicable in the model are overridden; node
        presence, node timing, and model constraints are carried over
        unchanged.
        """
        base = self.semantic_signature(model_id)
        statuses = dict(base.edge_statuses)
        for edge_id, status in assignments.items():
            if edge_id in statuses:
                statuses[edge_id] = status
        return SemanticSignature(
            node_presence=base.node_presence,
            edge_statuses=tuple(sorted(statuses.items())),
            node_timing=base.node_timing,
            constraints=base.constraints,
        )

    def matching_model(
        self,
        model_id: str,
        assignments: dict[str, str],
        allowed_model_ids: set[str] | None = None,
    ) -> str | None:
        """Find an existing model matching a hypothetical resolution.

        Returns the lexicographically first model with the target semantic
        signature (including still-partial models). When ``allowed_model_ids``
        is supplied, only those models are considered. Returns ``None`` when
        no matching model exists.
        """
        target = self.signature_after_resolution(model_id, assignments)
        candidates = self.models_by_signature.get(target)
        if not candidates:
            return None
        if allowed_model_ids is not None:
            candidates = [m for m in candidates if m in allowed_model_ids]
        return candidates[0] if candidates else None

    @staticmethod
    def is_resolved_signature(signature: SemanticSignature) -> bool:
        return all(
            status in ("causal", "non-causal") for _, status in signature.edge_statuses
        )

    def is_resolved(self, model_id: str) -> bool:
        return self.is_resolved_signature(self.semantic_signature(model_id))

    def is_valid_signature(self, signature: SemanticSignature) -> bool:
        present = {comp_id for comp_id, value in signature.node_presence if value}
        statuses = dict(signature.edge_statuses)
        timing = dict(signature.node_timing)
        directed_edges: set[tuple[str, str]] = set()

        for edge_id, edge in self._edges.items():
            source_id = self._node_id_by_name.get(edge["source"])
            target_id = self._node_id_by_name.get(edge["target"])
            applicable = source_id in present and target_id in present
            status = statuses.get(edge_id)

            if edge["fixed_status"] == "causal":
                if not applicable or status != "causal":
                    return False

            if not applicable or status != "causal" or edge["direction"] != "->":
                continue

            source_timing = timing.get(source_id)
            target_timing = timing.get(target_id)
            if (
                source_timing is not None
                and target_timing is not None
                and source_timing >= target_timing
            ):
                return False
            directed_edges.add((edge["source"], edge["target"]))

        return not self._has_cycle(directed_edges)

    @staticmethod
    def _has_cycle(edges: set[tuple[str, str]]) -> bool:
        adjacency: dict[str, list[str]] = {}
        for source, target in edges:
            adjacency.setdefault(source, []).append(target)
            adjacency.setdefault(target, [])

        visiting: set[str] = set()
        visited: set[str] = set()

        def visit(node: str) -> bool:
            if node in visiting:
                return True
            if node in visited:
                return False
            visiting.add(node)
            for target in adjacency.get(node, []):
                if visit(target):
                    return True
            visiting.remove(node)
            visited.add(node)
            return False

        return any(visit(node) for node in adjacency)

    def expected_signatures(self, model_id: str) -> tuple[SemanticSignature, ...]:
        cached = self._expected_cache.get(model_id)
        if cached is not None:
            return cached

        base = self.semantic_signature(model_id)
        statuses = dict(base.edge_statuses)
        unknown_edges = tuple(
            edge_id for edge_id, status in base.edge_statuses if status == "unknown"
        )
        expected: set[SemanticSignature] = set()

        for values in itertools.product(
            ("causal", "non-causal"), repeat=len(unknown_edges)
        ):
            resolved = dict(statuses)
            resolved.update(zip(unknown_edges, values))
            signature = SemanticSignature(
                node_presence=base.node_presence,
                edge_statuses=tuple(sorted(resolved.items())),
                node_timing=base.node_timing,
                constraints=base.constraints,
            )
            if self.is_resolved_signature(signature) and self.is_valid_signature(
                signature
            ):
                expected.add(signature)

        result = tuple(sorted(expected, key=repr))
        self._expected_cache[model_id] = result
        return result

    expected_completion_signatures = expected_signatures

    def resolved_descendants(self, partial_model_id: str) -> list[str]:
        if self.is_resolved(partial_model_id):
            signature = self.semantic_signature(partial_model_id)
            return [partial_model_id] if self.is_valid_signature(signature) else []

        descendants: list[str] = []
        for signature in self.expected_signatures(partial_model_id):
            model_ids = self._resolved_by_signature.get(signature)
            if model_ids:
                descendants.append(model_ids[0])
        return descendants

    lookup = resolved_descendants

    def diagnostics(self, model_id: str) -> CompletionDiagnostics:
        cached = self._diagnostics_cache.get(model_id)
        if cached is not None:
            return cached

        signature = self.semantic_signature(model_id)
        is_resolved = self.is_resolved_signature(signature)
        unknown_edges = tuple(
            edge_id
            for edge_id, status in signature.edge_statuses
            if status == "unknown"
        )
        expected = self.expected_signatures(model_id)

        if is_resolved:
            completion_ids = tuple(self.resolved_descendants(model_id))
            represented = {signature} if completion_ids else set()
            duplicate_count = 0
        else:
            completion_ids = tuple(self.resolved_descendants(model_id))
            represented = {
                expected_signature
                for expected_signature in expected
                if expected_signature in self._resolved_by_signature
            }
            duplicate_count = sum(
                max(0, len(self._resolved_by_signature[item]) - 1)
                for item in represented
            )

        expected_set = set(expected)
        missing = tuple(sorted(expected_set - represented, key=repr))
        coverage_complete = represented == expected_set
        if is_resolved and not represented:
            coverage_complete = False
        result = CompletionDiagnostics(
            model_id=model_id,
            is_resolved=is_resolved,
            unknown_edge_ids=unknown_edges,
            completion_ids=completion_ids,
            completion_count=len(represented),
            expected_completion_count=len(expected_set),
            completion_coverage_complete=coverage_complete,
            completion_source="direct" if is_resolved else "multiverse_lookup",
            missing_signatures=missing,
            duplicate_resolved_state_count=duplicate_count,
        )
        self._diagnostics_cache[model_id] = result
        return result

    completion_diagnostics = diagnostics


def materialize_missing_completions(
    state,
    registry: ComponentRegistry,
    model_ids: list[str],
    *,
    id_prefix: str = "__support_",
) -> list[dict]:
    """Return resolved support records needed to close the selected models.

    Existing resolved states are reused by semantic signature. Returned states
    are support artifacts only; callers decide whether they enter analysis.
    """
    index = CompletionIndex(state, registry)
    existing = set(index._resolved_by_signature)
    pending: list[SemanticSignature] = []
    seen = set(existing)

    for model_id in model_ids:
        for signature in index.expected_signatures(model_id):
            if signature not in seen:
                seen.add(signature)
                pending.append(signature)

    records: list[dict] = []
    for number, signature in enumerate(sorted(pending, key=repr), start=1):
        support_id = f"{id_prefix}{number:06d}"
        present = {comp_id for comp_id, value in signature.node_presence if value}
        timing = dict(signature.node_timing)

        for comp_id in sorted(present):
            records.append(
                {
                    "model_id": support_id,
                    "comp_id": comp_id,
                    "status": "present",
                    "timing": timing.get(comp_id),
                    "completion_support": True,
                }
            )
        for comp_id, status in signature.edge_statuses:
            records.append(
                {
                    "model_id": support_id,
                    "comp_id": comp_id,
                    "status": status,
                    "timing": None,
                    "completion_support": True,
                }
            )
    return records

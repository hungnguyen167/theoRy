from __future__ import annotations

import inspect
from dataclasses import dataclass, replace
from typing import Any

from dyadic.causal import CausalError
from dyadic.identification import IdentificationError
from state.completions import CompletionDiagnostics, CompletionIndex, SemanticSignature

NormalizedMAS = frozenset[frozenset[str]]


def normalize_mas(mas: list[str] | list[list[str]] | None) -> NormalizedMAS | None:
    """Normalize MAS without conflating ``[[]]``, ``[]``, and ``None``."""
    if mas is None:
        return None
    if mas and all(isinstance(item, str) for item in mas):
        return frozenset({frozenset(item for item in mas if item)})
    return frozenset(
        frozenset(item)
        for item in mas
        if isinstance(item, (list, tuple, set, frozenset))
    )


def _serialize_mas(mas: NormalizedMAS | None) -> list[list[str]] | None:
    if mas is None:
        return None
    return [
        sorted(item)
        for item in sorted(mas, key=lambda value: (len(value), tuple(sorted(value))))
    ]


def mas_compatible(
    profile_a: CausalQueryProfile, profile_b: CausalQueryProfile
) -> bool | None:
    mas_a = profile_a.robust_mas
    mas_b = profile_b.robust_mas
    if mas_a is None or mas_b is None:
        return None
    if not mas_a or not mas_b:
        return False
    return bool(mas_a & mas_b)


def identified_compatible(
    profile_a: CausalQueryProfile, profile_b: CausalQueryProfile
) -> bool | None:
    if profile_a.identified is None or profile_b.identified is None:
        return None
    return profile_a.identified and profile_b.identified


@dataclass(frozen=True)
class CausalQueryProfile:
    model_id: str
    mas: list[list[str]] | None
    robust_mas: NormalizedMAS | None
    identified: bool | None
    identification_formula: str | None
    is_resolved: bool
    completion_ids: tuple[str, ...]
    completion_count: int
    expected_completion_count: int | None
    completion_coverage_complete: bool
    completion_source: str = "direct"
    identification_method: str | None = None
    completion_diagnostics: CompletionDiagnostics | None = None

    @property
    def diagnostics(self) -> CompletionDiagnostics | None:
        return self.completion_diagnostics


@dataclass(frozen=True)
class _ProfilePayload:
    mas: list[list[str]] | None
    robust_mas: NormalizedMAS | None
    identified: bool | None
    identification_formula: str | None
    is_resolved: bool
    completion_ids: tuple[str, ...]
    completion_count: int
    expected_completion_count: int | None
    completion_coverage_complete: bool
    completion_source: str
    identification_method: str | None
    completion_diagnostics: CompletionDiagnostics


class CausalProfileBuilder:
    """Build and cache one causal-query profile per semantic state and query."""

    def __init__(
        self,
        state=None,
        registry=None,
        dag_spec_builder=None,
        causal_wrapper=None,
        identification_wrapper=None,
        *,
        completion_index: CompletionIndex | None = None,
    ) -> None:
        if completion_index is None:
            if state is None or registry is None:
                raise ValueError(
                    "state and registry are required without completion_index"
                )
            completion_index = CompletionIndex(state, registry)
        self.completion_index = completion_index
        self.state = completion_index.state
        self.registry = completion_index.registry
        if dag_spec_builder is None:
            raise ValueError("dag_spec_builder is required")
        if causal_wrapper is None:
            raise ValueError("causal_wrapper is required")
        self.dag_spec_builder = dag_spec_builder
        self.causal_wrapper = causal_wrapper
        self.identification_wrapper = identification_wrapper
        self._cache: dict[
            tuple[SemanticSignature, str | None, str | None, str], _ProfilePayload
        ] = {}

    def build(
        self,
        model_id: str,
        exposure: str | None,
        outcome: str | None,
    ) -> CausalQueryProfile:
        if (exposure is None) != (outcome is None):
            raise ValueError("Both or neither of exposure and outcome must be provided")
        if exposure is not None and exposure == outcome:
            raise ValueError("Exposure and outcome must be distinct")

        signature = self.completion_index.semantic_signature(model_id)
        key = (signature, exposure, outcome, "total")
        payload = self._cache.get(key)
        if payload is None:
            if self.completion_index.is_resolved(model_id):
                payload = self._build_resolved(model_id, exposure, outcome)
            else:
                payload = self._build_partial(model_id, exposure, outcome)
            self._cache[key] = payload
        diagnostics = self.completion_index.diagnostics(model_id)
        payload = replace(
            payload,
            completion_ids=diagnostics.completion_ids,
            completion_count=diagnostics.completion_count,
            expected_completion_count=diagnostics.expected_completion_count,
            completion_coverage_complete=diagnostics.completion_coverage_complete,
            completion_source=diagnostics.completion_source,
            completion_diagnostics=diagnostics,
        )
        return CausalQueryProfile(model_id=model_id, **payload.__dict__)

    build_profile = build

    def build_all(
        self,
        model_ids: list[str] | tuple[str, ...] | None,
        exposure: str | None,
        outcome: str | None,
    ) -> dict[str, CausalQueryProfile]:
        if model_ids is None:
            model_ids = self.state.model_ids
        return {
            model_id: self.build(model_id, exposure, outcome) for model_id in model_ids
        }

    @staticmethod
    def compare(
        profile_a: CausalQueryProfile, profile_b: CausalQueryProfile
    ) -> dict[str, Any]:
        return {
            "mas_ego": profile_a.mas,
            "mas_alter": profile_b.mas,
            "mas_compatible": mas_compatible(profile_a, profile_b),
            "identified_ego": profile_a.identified,
            "identified_alter": profile_b.identified,
            "identified_compatible": identified_compatible(profile_a, profile_b),
        }

    def _build_resolved(
        self,
        model_id: str,
        exposure: str | None,
        outcome: str | None,
    ) -> _ProfilePayload:
        diagnostics = self.completion_index.diagnostics(model_id)
        signature = self.completion_index.semantic_signature(model_id)
        if (
            not diagnostics.completion_coverage_complete
            or not self.completion_index.is_valid_signature(signature)
        ):
            return self._unavailable_payload(diagnostics)

        try:
            dag_spec = self._make_dag_spec(model_id, exposure, outcome)
        except Exception:
            return self._unavailable_payload(diagnostics)

        if dag_spec is None or dag_spec.get("query_nodes_missing", False):
            return self._unavailable_payload(diagnostics)

        try:
            mas = self.causal_wrapper.compute_adjustment_sets(dag_spec)
        except CausalError:
            raise
        robust_mas = normalize_mas(mas)

        identified: bool | None = None
        formula: str | None = None
        method: str | None = None
        if self.identification_wrapper is not None:
            try:
                identified, formula = self._identify(dag_spec)
                method = "general_id"
            except IdentificationError:
                identified = None
                formula = None

        return _ProfilePayload(
            mas=mas,
            robust_mas=robust_mas,
            identified=identified,
            identification_formula=formula,
            is_resolved=True,
            completion_ids=diagnostics.completion_ids,
            completion_count=diagnostics.completion_count,
            expected_completion_count=diagnostics.expected_completion_count,
            completion_coverage_complete=True,
            completion_source="direct",
            identification_method=method,
            completion_diagnostics=diagnostics,
        )

    def _build_partial(
        self,
        model_id: str,
        exposure: str | None,
        outcome: str | None,
    ) -> _ProfilePayload:
        diagnostics = self.completion_index.diagnostics(model_id)
        descendant_profiles = [
            self.build(descendant_id, exposure, outcome)
            for descendant_id in diagnostics.completion_ids
        ]

        robust_mas: NormalizedMAS | None = None
        if diagnostics.completion_coverage_complete:
            if not descendant_profiles:
                robust_mas = frozenset()
            elif all(profile.robust_mas is not None for profile in descendant_profiles):
                robust_mas = descendant_profiles[0].robust_mas
                for profile in descendant_profiles[1:]:
                    robust_mas = robust_mas & profile.robust_mas  # type: ignore[operator]

        represented_identified = [profile.identified for profile in descendant_profiles]
        if any(value is False for value in represented_identified):
            identified: bool | None = False
        elif (
            diagnostics.completion_coverage_complete
            and descendant_profiles
            and all(value is True for value in represented_identified)
        ):
            identified = True
        else:
            identified = None

        return _ProfilePayload(
            mas=_serialize_mas(robust_mas),
            robust_mas=robust_mas,
            identified=identified,
            identification_formula=None,
            is_resolved=False,
            completion_ids=diagnostics.completion_ids,
            completion_count=diagnostics.completion_count,
            expected_completion_count=diagnostics.expected_completion_count,
            completion_coverage_complete=diagnostics.completion_coverage_complete,
            completion_source="multiverse_lookup",
            identification_method="general_id" if self.identification_wrapper else None,
            completion_diagnostics=diagnostics,
        )

    def _unavailable_payload(
        self, diagnostics: CompletionDiagnostics
    ) -> _ProfilePayload:
        return _ProfilePayload(
            mas=None,
            robust_mas=None,
            identified=None,
            identification_formula=None,
            is_resolved=diagnostics.is_resolved,
            completion_ids=diagnostics.completion_ids,
            completion_count=diagnostics.completion_count,
            expected_completion_count=diagnostics.expected_completion_count,
            completion_coverage_complete=diagnostics.completion_coverage_complete,
            completion_source=diagnostics.completion_source,
            identification_method=None,
            completion_diagnostics=diagnostics,
        )

    def _make_dag_spec(
        self, model_id: str, exposure: str | None, outcome: str | None
    ) -> dict:
        parameters = inspect.signature(self.dag_spec_builder).parameters
        if "state" in parameters and "registry" in parameters:
            return self.dag_spec_builder(
                model_id,
                self.state,
                self.registry,
                exposure=exposure,
                outcome=outcome,
            )
        return self.dag_spec_builder(model_id, exposure=exposure, outcome=outcome)

    def _identify(self, dag_spec: dict) -> tuple[bool, str | None]:
        method = self.identification_wrapper.identify_total_effect
        parameters = inspect.signature(method).parameters
        if len(parameters) == 1 and "dag_spec" in parameters:
            result = method(dag_spec)
        else:
            result = method(
                nodes=dag_spec["nodes"],
                directed_edges=dag_spec.get("edges", []),
                bidirected_edges=dag_spec.get("bidirected_edges", []),
                exposure=dag_spec["exposure"],
                outcome=dag_spec["outcome"],
            )
        if isinstance(result, tuple):
            return result
        return bool(result), None


# Service is an equivalent name for callers that prefer orchestration terminology.
CausalProfileService = CausalProfileBuilder

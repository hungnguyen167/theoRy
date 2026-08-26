from __future__ import annotations

import builtins
from unittest.mock import Mock

import pytest

import dyadic.causal as causal_module
from dyadic.causal import (
    CausalBackendUnavailableError,
    CausalWrapper,
    NativeCausalUnsupportedError,
)
from dyadic.identification import (
    IdentificationBackendUnavailableError,
    IdentificationError,
    IdentificationWrapper,
    NativeIdentificationUnsupportedError,
)


def _reject_rpy2_imports(monkeypatch):
    original_import = builtins.__import__

    def reject_rpy2(name, *args, **kwargs):
        if name == "rpy2" or name.startswith("rpy2."):
            raise AssertionError("native backend must not import rpy2")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", reject_rpy2)


@pytest.mark.parametrize("causal_backend", ["native", "auto"])
def test_native_backdoor_adjustment_and_identification_do_not_import_rpy2(
    monkeypatch, causal_backend
):
    _reject_rpy2_imports(monkeypatch)
    dag_spec = {
        "nodes": ["X", "Z", "Y"],
        "edges": [("Z", "X"), ("Z", "Y"), ("X", "Y")],
        "exposure": "X",
        "outcome": "Y",
    }

    adjustment = CausalWrapper(causal_backend=causal_backend)
    assert adjustment.compute_adjustment_sets(dag_spec) == [["Z"]]

    identified, formula = IdentificationWrapper(
        causal_backend=causal_backend
    ).identify_total_effect(
        nodes=dag_spec["nodes"],
        directed_edges=dag_spec["edges"],
        bidirected_edges=[],
        exposure="X",
        outcome="Y",
    )
    assert identified is True
    assert formula is not None
    assert "Z" in formula


def test_native_expands_bidirected_edge_as_latent_common_cause():
    dag_spec = {
        "nodes": ["X", "Z", "Y"],
        "edges": [("Z", "Y"), ("X", "Y")],
        "bidirected_edges": [("X", "Z")],
        "exposure": "X",
        "outcome": "Y",
    }

    assert CausalWrapper(causal_backend="native").compute_adjustment_sets(dag_spec) == [
        ["Z"]
    ]


def test_auto_falls_back_to_r_only_for_native_unsupported_queries(monkeypatch):
    def unsupported(_dag_spec):
        raise NativeCausalUnsupportedError("native scope exceeded")

    r_adjustment = Mock(return_value=[["R"]])
    monkeypatch.setattr(causal_module, "native_backdoor_adjustment_sets", unsupported)

    adjustment = CausalWrapper(causal_backend="auto")
    monkeypatch.setattr(adjustment, "_compute_adjustment_sets_r", r_adjustment)
    assert adjustment.compute_adjustment_sets({"nodes": ["X", "Y"]}) == [["R"]]
    r_adjustment.assert_called_once()

    identification = IdentificationWrapper(causal_backend="auto")
    r_identification = Mock(return_value=(False, None))
    monkeypatch.setattr(
        identification,
        "_identify_total_effect_native",
        Mock(side_effect=NativeIdentificationUnsupportedError("native scope exceeded")),
    )
    monkeypatch.setattr(
        identification,
        "_identify_total_effect_r",
        r_identification,
    )
    assert identification.identify_total_effect(
        nodes=["X", "Y"],
        directed_edges=[("X", "Y")],
        bidirected_edges=[],
        exposure="X",
        outcome="Y",
    ) == (False, None)
    r_identification.assert_called_once()


def test_auto_reports_r_only_queries_as_unavailable_without_r_backend(monkeypatch):
    def unsupported(*_args, **_kwargs):
        raise NativeCausalUnsupportedError("native scope exceeded")

    monkeypatch.setattr(causal_module, "native_backdoor_adjustment_sets", unsupported)
    adjustment = CausalWrapper(causal_backend="auto")
    monkeypatch.setattr(
        adjustment,
        "_compute_adjustment_sets_r",
        Mock(side_effect=CausalBackendUnavailableError("R backend unavailable")),
    )
    assert adjustment.compute_adjustment_sets({"nodes": ["X", "Y"]}) is None

    identification = IdentificationWrapper(causal_backend="auto")
    monkeypatch.setattr(
        identification,
        "_identify_total_effect_native",
        Mock(side_effect=NativeIdentificationUnsupportedError("native scope exceeded")),
    )
    monkeypatch.setattr(
        identification,
        "_identify_total_effect_r",
        Mock(
            side_effect=IdentificationBackendUnavailableError("R backend unavailable")
        ),
    )
    assert identification.identify_total_effect(
        nodes=["X", "Y"],
        directed_edges=[("X", "Y")],
        bidirected_edges=[],
        exposure="X",
        outcome="Y",
    ) == (None, None)


def test_native_identification_rejects_non_backdoor_cases():
    with pytest.raises(IdentificationError, match="valid backdoor adjustment set"):
        IdentificationWrapper(causal_backend="native").identify_total_effect(
            nodes=["X", "M", "Y"],
            directed_edges=[("X", "M"), ("M", "Y")],
            bidirected_edges=[("X", "Y")],
            exposure="X",
            outcome="Y",
        )

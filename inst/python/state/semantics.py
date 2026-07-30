from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from registry.schema import ComponentRegistry
from state.tensor import StateError

# ---------------------------------------------------------------------------
# Canonical status constants
# ---------------------------------------------------------------------------

NODE_PRESENT = "present"
NODE_ABSENT = "absent"
EDGE_UNKNOWN = "unknown"
EDGE_CAUSAL = "causal"
EDGE_NON_CAUSAL = "non-causal"

VALID_NODE_STATUSES = frozenset({NODE_PRESENT, NODE_ABSENT})
VALID_EDGE_STATUSES = frozenset({EDGE_UNKNOWN, EDGE_CAUSAL, EDGE_NON_CAUSAL})
VALID_BIDIRECTED_STATUSES = frozenset({NODE_PRESENT, NODE_ABSENT})
VALID_BIDIRECTED_INPUT_STATUSES = VALID_BIDIRECTED_STATUSES | VALID_EDGE_STATUSES


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class NormalizedState:
    """Normalized sparse state for a single model."""

    present_nodes: set[str] = field(default_factory=set)
    edge_statuses: dict[str, str] = field(default_factory=dict)
    timing: dict[str, int] = field(default_factory=dict)


@dataclass
class StructuralCounts:
    """Result of comparing structural claims between two models."""

    shared_claims: int = 0
    union_claims: int = 0
    repair_cost: int = 0
    node_conflicts: int = 0
    edge_conflicts: int = 0
    inapplicable_components: int = 0
    compared_components: int = 0


# ---------------------------------------------------------------------------
# Registry helpers
# ---------------------------------------------------------------------------


def node_component_map(registry: ComponentRegistry) -> dict[str, str]:
    """Return {node_name: comp_id} for all node components."""
    df = registry.data
    node_comps = df[df["type"] == "node"]
    return dict(zip(node_comps["source"], node_comps["comp_id"]))


def edge_endpoint_components(
    registry: ComponentRegistry,
) -> dict[str, tuple[str, str]]:
    """Return {edge_comp_id: (source_name, target_name)} for all edge components."""
    df = registry.data
    edge_comps = df[df["type"] == "edge"]
    return {
        row["comp_id"]: (row["source"], row["target"])
        for _, row in edge_comps.iterrows()
    }


def is_node_component(registry: ComponentRegistry, comp_id: str) -> bool:
    """Check if a component ID is a node component."""
    df = registry.data
    row = df[df["comp_id"] == comp_id]
    if row.empty:
        return False
    return row.iloc[0]["type"] == "node"


def is_edge_component(registry: ComponentRegistry, comp_id: str) -> bool:
    """Check if a component ID is an edge component."""
    df = registry.data
    row = df[df["comp_id"] == comp_id]
    if row.empty:
        return False
    return row.iloc[0]["type"] == "edge"


# ---------------------------------------------------------------------------
# Normalization
# ---------------------------------------------------------------------------


def normalize_sparse_records(
    registry: ComponentRegistry,
    records: list[dict[str, Any]],
    *,
    infer_edge_endpoints: bool = False,
) -> dict[str, NormalizedState]:
    """Normalize state records into sparse semantics.

    Parameters
    ----------
    registry:
        Component registry defining the universe.
    records:
        List of ``{model_id, comp_id, status, timing}`` dicts.
    infer_edge_endpoints:
        If ``True``, edge claims infer endpoint node presence (seed-claim
        mode).  If ``False``, edge claims require explicit node presence
        (strict mode).

    Returns
    -------
    Dictionary mapping ``model_id -> NormalizedState``.
    """
    edge_endpoints = edge_endpoint_components(registry)
    edge_directions = {
        row["comp_id"]: row["direction"]
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows()
    }
    node_map = node_component_map(registry)
    node_comp_ids = set(node_map.values())
    valid_comp_ids = set(registry.data["comp_id"].tolist())

    grouped: dict[str, list[dict[str, Any]]] = {}
    for r in records:
        grouped.setdefault(r["model_id"], []).append(r)

    result: dict[str, NormalizedState] = {}

    for model_id, model_records in grouped.items():
        ns = NormalizedState()
        explicit_absent_nodes: set[str] = set()
        edge_records: list[dict[str, Any]] = []

        for r in model_records:
            comp_id = r["comp_id"]
            status = r["status"]

            if comp_id not in valid_comp_ids:
                raise StateError(f"Unknown component ID: {comp_id}")

            if comp_id in node_comp_ids:
                if status in {NODE_PRESENT, EDGE_CAUSAL}:
                    ns.present_nodes.add(comp_id)
                elif status in {NODE_ABSENT, EDGE_UNKNOWN, EDGE_NON_CAUSAL}:
                    if status == NODE_ABSENT:
                        explicit_absent_nodes.add(comp_id)
                else:
                    raise StateError(f"Invalid status for node {comp_id}: {status!r}")
                timing_val = r.get("timing")
                if timing_val is not None:
                    ns.timing[comp_id] = timing_val
            else:
                if comp_id in edge_endpoints:
                    valid_statuses = (
                        VALID_BIDIRECTED_INPUT_STATUSES
                        if edge_directions.get(comp_id) == "<->"
                        else VALID_EDGE_STATUSES
                    )
                    if status not in valid_statuses:
                        raise StateError(
                            f"Invalid status for edge {comp_id}: {status!r}"
                        )
                    if edge_directions.get(comp_id) == "<->":
                        r = dict(r)
                        if status == NODE_PRESENT:
                            r["status"] = EDGE_CAUSAL
                        elif status == NODE_ABSENT:
                            r["status"] = EDGE_NON_CAUSAL
                    edge_records.append(r)

        for r in edge_records:
            comp_id = r["comp_id"]
            status = r["status"]
            src_name, tgt_name = edge_endpoints[comp_id]
            src_cid = node_map.get(src_name)
            tgt_cid = node_map.get(tgt_name)

            if src_cid is None or tgt_cid is None:
                raise StateError(
                    f"Edge {comp_id} references unknown endpoint in model {model_id}"
                )

            if infer_edge_endpoints:
                for node_cid in (src_cid, tgt_cid):
                    if node_cid in explicit_absent_nodes:
                        raise StateError(
                            f"Edge {comp_id} conflicts with explicitly absent "
                            f"endpoint node {node_cid} in model {model_id}"
                        )
                    ns.present_nodes.add(node_cid)
            else:
                if src_cid not in ns.present_nodes:
                    raise StateError(
                        f"Edge {comp_id} requires node {src_name} "
                        f"to be present in model {model_id}"
                    )
                if tgt_cid not in ns.present_nodes:
                    raise StateError(
                        f"Edge {comp_id} requires node {tgt_name} "
                        f"to be present in model {model_id}"
                    )

            ns.edge_statuses[comp_id] = status

        result[model_id] = ns

    return result


# ---------------------------------------------------------------------------
# Applicability helpers
# ---------------------------------------------------------------------------


def edge_applicable(
    state,
    model_id: str,
    edge_comp_id: str,
    registry: ComponentRegistry,
) -> bool:
    """Check if an edge is applicable in a model (both endpoints present).

    Works with both legacy StateTensor (dense) and sparse-aware StateTensor.
    """
    if hasattr(state, "is_edge_component") and not state.is_edge_component(
        edge_comp_id
    ):
        return False
    if hasattr(state, "edge_applicable"):
        return state.edge_applicable(model_id, edge_comp_id)

    edge_endpoints = edge_endpoint_components(registry)
    if edge_comp_id not in edge_endpoints:
        return False

    src_name, tgt_name = edge_endpoints[edge_comp_id]
    node_map = node_component_map(registry)

    src_cid = node_map.get(src_name)
    tgt_cid = node_map.get(tgt_name)

    if src_cid is None or tgt_cid is None:
        return False

    if hasattr(state, "node_present"):
        return state.node_present(model_id, src_cid) and state.node_present(
            model_id, tgt_cid
        )
    else:
        src_status = state.get_status(model_id, src_cid)
        tgt_status = state.get_status(model_id, tgt_cid)
        return src_status in ("causal", "present") and tgt_status in (
            "causal",
            "present",
        )


def effective_edge_status(
    state,
    model_id: str,
    edge_comp_id: str,
    registry: ComponentRegistry,
) -> str | None:
    """Return the effective edge status, or ``None`` if inapplicable."""
    if not edge_applicable(state, model_id, edge_comp_id, registry):
        return None
    raw = state.get_status(model_id, edge_comp_id)
    if raw in VALID_EDGE_STATUSES:
        return raw
    if raw == "unknown":
        return EDGE_UNKNOWN
    return EDGE_UNKNOWN


# ---------------------------------------------------------------------------
# Structural comparison
# ---------------------------------------------------------------------------


def compare_structural_claims(
    state,
    registry: ComponentRegistry,
    ego_id: str,
    alter_id: str,
) -> StructuralCounts:
    """Compare structural claims between two models under sparse semantics.

    Node contribution:
      present/present -> shared+1, union+1
      present/absent or absent/present -> union+1, repair+1
      absent/absent -> nothing

    Edge contribution (both-inapplicable edges are ignored):
      inapplicable/inapplicable -> nothing

    Edge contribution (one-sided applicable counts as disagreement):
      applicable/inapplicable -> union+1, repair+1
      inapplicable/applicable -> union+1, repair+1

    Edge contribution (when applicable in BOTH models):
      causal/causal -> shared+1, union+1
      non-causal/non-causal -> shared+1, union+1
      causal/unknown or unknown/causal -> union+1, repair+1
      non-causal/unknown or unknown/non-causal -> union+1, repair+1
      causal/non-causal or non-causal/causal -> union+2, repair+1, conflict+1
      unknown/unknown -> nothing

    Returns a :class:`StructuralCounts` named tuple.
    """
    counts = StructuralCounts()
    node_map = node_component_map(registry)
    edge_endpoints = edge_endpoint_components(registry)

    # --- Node comparison ---
    for node_name, node_cid in node_map.items():
        ego_present = _node_is_present(state, ego_id, node_cid)
        alter_present = _node_is_present(state, alter_id, node_cid)

        if ego_present and alter_present:
            counts.shared_claims += 1
            counts.union_claims += 1
            counts.compared_components += 1
        elif ego_present != alter_present:
            counts.union_claims += 1
            counts.repair_cost += 1
            counts.node_conflicts += 1
            counts.compared_components += 1

    # --- Edge comparison ---
    for edge_cid in edge_endpoints:
        ego_app = edge_applicable(state, ego_id, edge_cid, registry)
        alter_app = edge_applicable(state, alter_id, edge_cid, registry)

        if not ego_app and not alter_app:
            counts.inapplicable_components += 1
            continue

        if not ego_app or not alter_app:
            counts.inapplicable_components += 1
            counts.union_claims += 1
            counts.repair_cost += 1
            counts.compared_components += 1
            continue

        ego_status = state.get_status(ego_id, edge_cid)
        alter_status = state.get_status(alter_id, edge_cid)

        ego_resolved = ego_status in (EDGE_CAUSAL, EDGE_NON_CAUSAL)
        alter_resolved = alter_status in (EDGE_CAUSAL, EDGE_NON_CAUSAL)

        if ego_status == alter_status and ego_resolved:
            counts.shared_claims += 1
            counts.union_claims += 1
            counts.compared_components += 1
        elif ego_resolved or alter_resolved:
            if ego_resolved and alter_resolved and ego_status != alter_status:
                counts.union_claims += 2
                counts.repair_cost += 1
                counts.edge_conflicts += 1
            else:
                counts.union_claims += 1
                counts.repair_cost += 1
            counts.compared_components += 1

    return counts


def _node_is_present(state, model_id: str, node_cid: str) -> bool:
    """Check if a node is present in a model."""
    if hasattr(state, "node_present"):
        return state.node_present(model_id, node_cid)
    status = state.get_status(model_id, node_cid)
    return status in ("causal", "present")

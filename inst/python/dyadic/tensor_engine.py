from __future__ import annotations

import torch

from registry.schema import ComponentRegistry
from state.tensor import StateTensor


def resolve_device(device: str = "auto") -> torch.device:
    if device == "auto":
        return torch.device("cuda" if torch.cuda.is_available() else "cpu")
    if device == "cuda" and not torch.cuda.is_available():
        raise ValueError("CUDA requested but not available")
    return torch.device(device)


def status_codes(
    state: StateTensor,
    device: str | torch.device = "cpu",
) -> torch.Tensor:
    M = len(state.model_ids)
    C = len(state.component_ids)
    codes = torch.zeros((M, C), dtype=torch.int8, device=device)
    t = state.tensor.to(device)

    causal = t[:, :, 0] == 1
    non_causal = t[:, :, 1] == 1

    codes[causal] = 1
    codes[non_causal] = 2
    return codes


def causal_mask(
    state: StateTensor,
    device: str | torch.device = "cpu",
) -> torch.Tensor:
    M = len(state.model_ids)
    C = len(state.component_ids)
    mask = torch.zeros((M, C), dtype=torch.bool, device=device)
    t = state.tensor.to(device)
    mask[t[:, :, 0] == 1] = True
    return mask


def _node_present_mask(
    state: StateTensor,
    device: str | torch.device = "cpu",
) -> torch.Tensor:
    if hasattr(state, "node_present_mask") and state.node_present_mask is not None:
        return state.node_present_mask.to(device)

    mask = causal_mask(state, device=device)
    node_mask = torch.zeros_like(mask)
    for cid in state._node_comp_ids:
        if cid in state.component_index:
            j = state.component_index[cid]
            node_mask[:, j] = mask[:, j]
    return node_mask


def _edge_applicable_mask(
    state: StateTensor,
    device: str | torch.device = "cpu",
) -> torch.Tensor:
    if (
        hasattr(state, "edge_applicable_mask")
        and state.edge_applicable_mask is not None
    ):
        return state.edge_applicable_mask.to(device)

    M = len(state.model_ids)
    C = len(state.component_ids)
    applicable = torch.zeros((M, C), dtype=torch.bool, device=device)

    for mid in state.model_ids:
        i = state.model_index[mid]
        for cid in state._edge_comp_ids:
            if cid not in state.component_index:
                continue
            j = state.component_index[cid]
            if state.edge_applicable(mid, cid):
                applicable[i, j] = True

    return applicable


def _edge_component_mask(state: StateTensor) -> torch.Tensor:
    """Build a boolean mask for edge component indices."""
    C = len(state.component_ids)
    edge_mask = torch.zeros(C, dtype=torch.bool)

    for cid in state._edge_comp_ids:
        if cid in state.component_index:
            j = state.component_index[cid]
            edge_mask[j] = True

    return edge_mask


def structural_similarity_matrix(
    state: StateTensor,
    registry: ComponentRegistry,
    model_ids: list[str] | None = None,
    device: str | torch.device = "cpu",
) -> tuple[torch.Tensor, list[str]]:
    if model_ids is None:
        model_ids = state.model_ids
    model_ids = sorted(model_ids)
    indices = [state.model_index[mid] for mid in model_ids]

    device = torch.device(device)

    np_mask = _node_present_mask(state, device=device)[indices]

    edge_app_mask = _edge_applicable_mask(state, device=device)[indices]

    t = state.tensor.to(device)
    causal = (t[:, :, 0] == 1)[indices]
    non_causal = (t[:, :, 1] == 1)[indices]

    edge_mask = _edge_component_mask(state)
    edge_mask_d = edge_mask.to(device)

    # --- Node contribution ---
    # shared nodes: present in both
    node_shared = (np_mask[:, None, :] & np_mask[None, :, :]).sum(dim=2)
    # union nodes: present in either
    node_union = (np_mask[:, None, :] | np_mask[None, :, :]).sum(dim=2)

    # --- Edge contribution ---
    # Only compare edges applicable in both models
    pair_applicable = edge_app_mask[:, None, :] & edge_app_mask[None, :, :]

    # One-sided applicable edges: applicable in exactly one model (XOR)
    one_sided_applicable = edge_app_mask[:, None, :] ^ edge_app_mask[None, :, :]

    # Resolved status for each model on edge components only
    resolved_any = (causal | non_causal) & edge_mask_d.unsqueeze(0)  # (M, C)

    # Same resolved status
    same_causal = causal.unsqueeze(1) & causal.unsqueeze(0) & pair_applicable
    same_noncausal = non_causal.unsqueeze(1) & non_causal.unsqueeze(0) & pair_applicable
    shared_edges = (same_causal | same_noncausal).sum(dim=2)

    # Conflicting resolved claims (causal vs non-causal)
    conflicting = (
        (
            (causal.unsqueeze(1) & non_causal.unsqueeze(0))
            | (non_causal.unsqueeze(1) & causal.unsqueeze(0))
        )
        & pair_applicable
    ).sum(dim=2)

    # Union edges: resolved in either model (OR), plus conflicting gets counted twice,
    # plus one-sided applicable edges (each adds one union claim)
    resolved_or = (
        resolved_any.unsqueeze(1) | resolved_any.unsqueeze(0)
    ) & pair_applicable
    union_edges = resolved_or.sum(dim=2) + conflicting + one_sided_applicable.sum(dim=2)

    shared = node_shared + shared_edges
    union = node_union + union_edges

    similarity = torch.where(
        union == 0,
        torch.ones_like(union, dtype=torch.float32),
        shared.to(torch.float32) / union.to(torch.float32),
    )

    return similarity.cpu(), model_ids


def structural_dyad_scores(
    state: StateTensor,
    registry: ComponentRegistry,
    model_ids: list[str] | None = None,
    device: str | torch.device = "cpu",
) -> tuple[torch.Tensor, list[str]]:
    matrix, ordered_ids = structural_similarity_matrix(
        state,
        registry,
        model_ids=model_ids,
        device=device,
    )

    M = matrix.shape[0]
    scores: list[float] = []
    dyad_ids: list[str] = []

    for i in range(M):
        for j in range(M):
            if i == j:
                continue
            scores.append(float(matrix[i, j]))
            dyad_ids.append(f"{ordered_ids[i]}__{ordered_ids[j]}")

    return torch.tensor(scores, dtype=torch.float32), dyad_ids

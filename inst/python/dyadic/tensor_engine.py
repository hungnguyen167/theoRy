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


def _temporal_valid_mask(
    state: StateTensor,
    registry: ComponentRegistry,
    device: str | torch.device = "cpu",
) -> torch.Tensor:
    M = len(state.model_ids)
    C = len(state.component_ids)
    valid = torch.ones((M, C), dtype=torch.bool, device=device)

    df = registry.data
    edge_df = df[df["type"] == "edge"]

    if edge_df.empty:
        return valid

    node_to_comp = {
        row["source"]: row["comp_id"]
        for _, row in df[df["type"] == "node"].iterrows()
    }

    for _, row in edge_df.iterrows():
        cid = row["comp_id"]
        if cid not in state.component_index:
            continue
        j = state.component_index[cid]

        source_cid = node_to_comp.get(row["source"])
        target_cid = node_to_comp.get(row["target"])
        if source_cid is None or target_cid is None:
            continue

        for mid in state.model_ids:
            i = state.model_index[mid]
            status = state.get_status(mid, cid)
            if status != "causal":
                continue

            source_t = state.get_timing(mid, source_cid)
            target_t = state.get_timing(mid, target_cid)

            if (
                source_t is not None
                and target_t is not None
                and source_t >= target_t
            ):
                valid[i, j] = False

    return valid


def structural_similarity_matrix(
    state: StateTensor,
    registry: ComponentRegistry,
    model_ids: list[str] | None = None,
    device: str | torch.device = "cpu",
    exclude_temporally_invalid: bool = True,
) -> tuple[torch.Tensor, list[str]]:
    if model_ids is None:
        model_ids = state.model_ids
    model_ids = sorted(model_ids)
    indices = [state.model_index[mid] for mid in model_ids]

    device = torch.device(device)

    causal = causal_mask(state, device=device)[indices]

    if exclude_temporally_invalid:
        valid = _temporal_valid_mask(state, registry, device=device)[indices]
    else:
        valid = torch.ones_like(causal, dtype=torch.bool, device=device)

    M = causal.shape[0]
    similarity = torch.zeros((M, M), dtype=torch.float32, device=device)

    causal_i = causal[:, None, :]
    causal_j = causal[None, :, :]
    pair_mask = valid[:, None, :] & valid[None, :, :]

    shared = (causal_i & causal_j & pair_mask).sum(dim=2)
    union = ((causal_i | causal_j) & pair_mask).sum(dim=2)

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
        state, registry, model_ids=model_ids, device=device,
        exclude_temporally_invalid=True,
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

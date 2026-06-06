from __future__ import annotations

import pandas as pd
import scipy.sparse
import torch

from state.tensor import StateTensor

SPARSE_ENCODING = {"unknown": 0, "causal": 1, "non-causal": 2}


def state_to_sparse(state: StateTensor) -> scipy.sparse.csr_matrix:
    """Convert a StateTensor to a CSR matrix.

    CSR encoding:
      unknown    = 0
      causal     = 1
      non-causal = 2
    """
    codes = torch.zeros(
        state.tensor.shape[:2],
        dtype=torch.int8,
        device=state.tensor.device,
    )
    codes[state.tensor[:, :, 0] == 1] = 1
    codes[state.tensor[:, :, 1] == 1] = 2

    return scipy.sparse.csr_matrix(codes.cpu().numpy())


def state_to_dataframe(state: StateTensor) -> pd.DataFrame:
    """Export a StateTensor to a tidy DataFrame for inspection/R export."""
    return pd.DataFrame(
        [
            {
                "model_id": mid,
                "comp_id": cid,
                "status": state.get_status(mid, cid),
            }
            for mid in state.model_ids
            for cid in state.component_ids
        ]
    )

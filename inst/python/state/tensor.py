from __future__ import annotations

import torch

from registry.schema import ComponentRegistry

STATUS_ENCODING = {
    "unknown": [0, 0],
    "causal": [1, 0],
    "non-causal": [0, 1],
}

STATUS_DECODING = {(0, 0): "unknown", (1, 0): "causal", (0, 1): "non-causal"}
STATUS_CODE = {"unknown": 0, "causal": 1, "non-causal": 2}

VALID_STATUSES = set(STATUS_ENCODING.keys())


class StateError(Exception):
    pass


class StateTensor:
    """Represents the theoretical state of multiple models using a uint8 tensor.

    Internal tensor shape is (model_count, component_count, 2) with dtype uint8.
    Status encoding:
      unknown    = [0, 0]
      causal     = [1, 0]
      non-causal = [0, 1]
    """

    def __init__(
        self,
        tensor: torch.Tensor,
        model_index: dict[str, int],
        component_index: dict[str, int],
        component_ids: list[str],
        model_ids: list[str],
        timing: dict[tuple[str, str], int],
    ):
        self.tensor = tensor
        self.model_index = model_index
        self.component_index = component_index
        self.component_ids = component_ids
        self.model_ids = model_ids
        self.timing = timing
        self.packed_tensor = self._pack_status_tensor()

    def _pack_status_tensor(self) -> torch.Tensor:
        """Pack four 2-bit status codes into each uint8 cell."""
        model_count = len(self.model_ids)
        component_count = len(self.component_ids)
        packed_width = (component_count + 3) // 4
        packed = torch.zeros((model_count, packed_width), dtype=torch.uint8)

        for mid in self.model_ids:
            i = self.model_index[mid]
            for cid in self.component_ids:
                j = self.component_index[cid]
                status = STATUS_DECODING.get(
                    tuple(self.tensor[i, j].tolist()), "unknown"
                )
                packed[i, j // 4] |= STATUS_CODE[status] << ((j % 4) * 2)

        return packed

    @classmethod
    def create(
        cls,
        registry: ComponentRegistry,
        model_ids: list[str],
    ) -> StateTensor:
        """Create a StateTensor with all statuses defaulting to "unknown"."""
        component_ids = sorted(registry.data["comp_id"].tolist())
        model_ids_sorted = sorted(model_ids)

        model_index = {mid: i for i, mid in enumerate(model_ids_sorted)}
        component_index = {cid: j for j, cid in enumerate(component_ids)}

        tensor = torch.zeros(
            (len(model_ids_sorted), len(component_ids), 2),
            dtype=torch.uint8,
        )
        timing: dict[tuple[str, str], int] = {}

        return cls(
            tensor=tensor,
            model_index=model_index,
            component_index=component_index,
            component_ids=component_ids,
            model_ids=model_ids_sorted,
            timing=timing,
        )

    @classmethod
    def from_records(
        cls,
        registry: ComponentRegistry,
        records: list[dict],
        model_ids: list[str] | None = None,
    ) -> StateTensor:
        """Create and populate a StateTensor from API-format records.

        If model_ids is omitted, infer sorted unique values from records.
        """
        if model_ids is None:
            model_ids = sorted({r["model_id"] for r in records})

        state = cls.create(registry, model_ids)

        updates: list[tuple[str, str, str]] = []
        for record in records:
            updates.append((record["model_id"], record["comp_id"], record["status"]))
            timing_val = record.get("timing")
            if timing_val is not None:
                state.set_timing(record["model_id"], record["comp_id"], timing_val)

        state.set_status_batch(updates)
        return state

    def set_status(self, model_id: str, comp_id: str, status: str) -> None:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")
        if status not in VALID_STATUSES:
            raise StateError(
                f"Invalid status: {status!r}. Must be one of {sorted(VALID_STATUSES)}"
            )

        i = self.model_index[model_id]
        j = self.component_index[comp_id]
        self.tensor[i, j] = torch.tensor(STATUS_ENCODING[status], dtype=torch.uint8)
        self.packed_tensor = self._pack_status_tensor()

    def set_status_batch(self, updates: list[tuple[str, str, str]]) -> None:
        """Batch update multiple status cells, rebuild packed_tensor once."""
        for model_id, comp_id, status in updates:
            if model_id not in self.model_index:
                raise StateError(f"Unknown model ID: {model_id}")
            if comp_id not in self.component_index:
                raise StateError(f"Unknown component ID: {comp_id}")
            if status not in VALID_STATUSES:
                raise StateError(
                    f"Invalid status: {status!r}. Must be one of {sorted(VALID_STATUSES)}"
                )

            i = self.model_index[model_id]
            j = self.component_index[comp_id]
            self.tensor[i, j] = torch.tensor(STATUS_ENCODING[status], dtype=torch.uint8)

        self.packed_tensor = self._pack_status_tensor()

    def get_status(self, model_id: str, comp_id: str) -> str:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")

        i = self.model_index[model_id]
        j = self.component_index[comp_id]
        key = tuple(self.tensor[i, j].tolist())
        return STATUS_DECODING.get(key, "unknown")

    def set_timing(self, model_id: str, comp_id: str, timing_value: int) -> None:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")
        if not isinstance(timing_value, int):
            raise StateError(
                f"Timing must be integer, got {type(timing_value).__name__}"
            )

        self.timing[(model_id, comp_id)] = timing_value

    def get_timing(self, model_id: str, comp_id: str) -> int | None:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")

        return self.timing.get((model_id, comp_id))

    def hash_model(self, model_id: str, *, include_timing: bool = False) -> int:
        """Return a hash of a specific model's state for cache invalidation."""
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        i = self.model_index[model_id]
        model_row = self.packed_tensor[i]
        if not include_timing:
            return hash(model_row.numpy().tobytes())

        timing = tuple(
            (cid, self.timing.get((model_id, cid)))
            for cid in self.component_ids
        )
        return hash((model_row.numpy().tobytes(), timing))

    def to_records(self) -> list[dict]:
        """Serialize state to a list of record dicts."""
        records: list[dict] = []
        for mid in self.model_ids:
            for cid in self.component_ids:
                status = self.get_status(mid, cid)
                t = self.get_timing(mid, cid)
                records.append(
                    {
                        "model_id": mid,
                        "comp_id": cid,
                        "status": status,
                        "timing": t,
                    }
                )
        return records

    def to_sparse(self):
        """Convert to scipy.sparse.csr_matrix."""
        from state.sparse import state_to_sparse

        return state_to_sparse(self)

    def to_dataframe(self):
        """Export to a tidy pandas DataFrame with model, component, and status."""
        from state.sparse import state_to_dataframe

        return state_to_dataframe(self)

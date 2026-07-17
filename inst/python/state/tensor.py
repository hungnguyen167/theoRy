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

VALID_NODE_STATUSES = frozenset({"present", "absent"})
VALID_EDGE_STATUSES = frozenset({"causal", "unknown", "non-causal"})


class StateError(Exception):
    pass


class StateTensor:
    """Represents the theoretical state of multiple models using a uint8 tensor.

    Internal tensor shape is (model_count, component_count, 2) with dtype uint8.
    Status encoding:
      unknown    = [0, 0]
      causal     = [1, 0]
      non-causal = [0, 1]

    Sparse-aware extensions:
      - ``node_present`` mask: bool[M, C] indicating which nodes are present
      - ``edge_applicable`` mask: bool[M, C] indicating which edges are applicable
      - Component type cache: ``_node_comp_ids``, ``_edge_comp_ids``
    """

    def __init__(
        self,
        tensor: torch.Tensor,
        model_index: dict[str, int],
        component_index: dict[str, int],
        component_ids: list[str],
        model_ids: list[str],
        timing: dict[tuple[str, str], int],
        node_present: torch.Tensor | None = None,
        edge_applicable: torch.Tensor | None = None,
        node_comp_ids: set[str] | None = None,
        edge_comp_ids: set[str] | None = None,
        edge_to_nodes: dict[str, tuple[str, str]] | None = None,
    ):
        self.tensor = tensor
        self.model_index = model_index
        self.component_index = component_index
        self.component_ids = component_ids
        self.model_ids = model_ids
        self.timing = timing
        self.packed_tensor = self._pack_status_tensor()

        M = len(model_ids)
        C = len(component_ids)

        self._node_comp_ids = node_comp_ids or set()
        self._edge_comp_ids = edge_comp_ids or set()
        self._edge_to_nodes = edge_to_nodes or {}

        if node_present is not None:
            self.node_present_mask = node_present
        else:
            self.node_present_mask = torch.zeros((M, C), dtype=torch.bool)

        if edge_applicable is not None:
            self.edge_applicable_mask = edge_applicable
        else:
            self.edge_applicable_mask = torch.zeros((M, C), dtype=torch.bool)

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

    # ------------------------------------------------------------------
    # Factory methods
    # ------------------------------------------------------------------

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

        node_comp_ids = set(
            registry.data[registry.data["type"] == "node"]["comp_id"].tolist()
        )
        edge_comp_ids = set(
            registry.data[registry.data["type"] == "edge"]["comp_id"].tolist()
        )

        node_map = {}
        for _, row in registry.data[registry.data["type"] == "node"].iterrows():
            node_map[row["source"]] = row["comp_id"]

        edge_to_nodes = {}
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows():
            src_cid = node_map.get(row["source"])
            tgt_cid = node_map.get(row["target"])
            if src_cid and tgt_cid:
                edge_to_nodes[row["comp_id"]] = (src_cid, tgt_cid)

        return cls(
            tensor=tensor,
            model_index=model_index,
            component_index=component_index,
            component_ids=component_ids,
            model_ids=model_ids_sorted,
            timing=timing,
            node_comp_ids=node_comp_ids,
            edge_comp_ids=edge_comp_ids,
            edge_to_nodes=edge_to_nodes,
        )

    @classmethod
    def from_records(
        cls,
        registry: ComponentRegistry,
        records: list[dict],
        model_ids: list[str] | None = None,
    ) -> StateTensor:
        """Create and populate a StateTensor from API-format records.

        Supports sparse node semantics:
        - Node statuses: "present", "absent" (or legacy "causal" for present)
        - Edge statuses: "causal", "unknown", "non-causal"
        - Missing node records mean absent
        - Edge records require both endpoints present

        If model_ids is omitted, infer sorted unique values from records.
        """
        if model_ids is None:
            model_ids = sorted({r["model_id"] for r in records})

        state = cls.create(registry, model_ids)

        node_map = {}
        for _, row in registry.data[registry.data["type"] == "node"].iterrows():
            node_map[row["source"]] = row["comp_id"]

        edge_endpoints = {}
        for _, row in registry.data[registry.data["type"] == "edge"].iterrows():
            edge_endpoints[row["comp_id"]] = (row["source"], row["target"])

        present_nodes: dict[str, set[str]] = {mid: set() for mid in model_ids}
        explicit_absent_nodes: dict[str, set[str]] = {mid: set() for mid in model_ids}

        updates: list[tuple[str, str, str]] = []
        edge_records: list[tuple[str, str, str]] = []

        for record in records:
            mid = record["model_id"]
            cid = record["comp_id"]
            status = record["status"]
            timing_val = record.get("timing")

            if mid not in state.model_index:
                raise StateError(f"Unknown model ID: {mid}")

            if cid in state._node_comp_ids:
                if status == "present" or status == "causal":
                    present_nodes[mid].add(cid)
                    updates.append((mid, cid, "causal"))
                elif status == "absent":
                    explicit_absent_nodes[mid].add(cid)
                    updates.append((mid, cid, "unknown"))
                elif status in {"unknown", "non-causal"}:
                    updates.append((mid, cid, status))
                else:
                    raise StateError(
                        f"Invalid status for node {cid}: {status!r}. "
                        "Use 'present' or 'absent' for sparse records. "
                        "Legacy node statuses 'causal', 'unknown', and "
                        "'non-causal' are accepted for dense inputs."
                    )
            elif cid in state._edge_comp_ids:
                if status not in VALID_EDGE_STATUSES:
                    raise StateError(
                        f"Invalid status for edge {cid}: {status!r}. "
                        f"Must be one of {sorted(VALID_EDGE_STATUSES)}"
                    )
                edge_records.append((mid, cid, status))
                updates.append((mid, cid, status))
            else:
                updates.append((mid, cid, status))

            if timing_val is not None:
                state.set_timing(mid, cid, timing_val)

        for mid, edge_cid, _status in edge_records:
            if edge_cid not in state._edge_to_nodes:
                continue
            src_cid, tgt_cid = state._edge_to_nodes[edge_cid]
            for node_cid in (src_cid, tgt_cid):
                if node_cid in explicit_absent_nodes[mid]:
                    raise StateError(
                        f"Edge {edge_cid} in model {mid} is inconsistent with "
                        f"explicitly absent endpoint node {node_cid}"
                    )

        state.set_status_batch(updates)

        node_present = torch.zeros(
            (len(model_ids), len(state.component_ids)), dtype=torch.bool
        )
        for mid in model_ids:
            i = state.model_index[mid]
            for node_cid in present_nodes[mid]:
                j = state.component_index[node_cid]
                node_present[i, j] = True

        state.node_present_mask = node_present

        edge_applicable = torch.zeros_like(node_present)
        for mid in model_ids:
            i = state.model_index[mid]
            for edge_cid, (src, tgt) in edge_endpoints.items():
                src_cid = node_map.get(src)
                tgt_cid = node_map.get(tgt)
                if src_cid is not None and tgt_cid is not None:
                    src_j = state.component_index.get(src_cid)
                    tgt_j = state.component_index.get(tgt_cid)
                    if src_j is not None and tgt_j is not None:
                        if node_present[i, src_j] and node_present[i, tgt_j]:
                            ej = state.component_index[edge_cid]
                            edge_applicable[i, ej] = True

        state.edge_applicable_mask = edge_applicable
        state._edge_to_nodes = state._edge_to_nodes  # already set by create

        return state

    # ------------------------------------------------------------------
    # Mutation
    # ------------------------------------------------------------------

    def set_status(self, model_id: str, comp_id: str, status: str) -> None:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")

        if comp_id in self._node_comp_ids:
            if status not in VALID_NODE_STATUSES and status not in VALID_STATUSES:
                raise StateError(
                    f"Invalid status for node {comp_id}: {status!r}. "
                    f"Must be one of {sorted(VALID_NODE_STATUSES)} or legacy {sorted(VALID_STATUSES)}"
                )
            if status == "present":
                status = "causal"
            elif status == "absent":
                status = "unknown"
        else:
            if status not in VALID_STATUSES:
                raise StateError(
                    f"Invalid status: {status!r}. Must be one of {sorted(VALID_STATUSES)}"
                )

        i = self.model_index[model_id]
        j = self.component_index[comp_id]
        self.tensor[i, j] = torch.tensor(STATUS_ENCODING[status], dtype=torch.uint8)

        if comp_id in self._node_comp_ids:
            self.node_present_mask[i, j] = status == "causal"

        self.packed_tensor = self._pack_status_tensor()
        self._recompute_edge_applicable(model_id)

    def set_status_batch(self, updates: list[tuple[str, str, str]]) -> None:
        """Batch update multiple status cells, rebuild packed_tensor once."""
        affected_models: set[str] = set()

        for model_id, comp_id, status in updates:
            if model_id not in self.model_index:
                raise StateError(f"Unknown model ID: {model_id}")
            if comp_id not in self.component_index:
                raise StateError(f"Unknown component ID: {comp_id}")

            resolve_status = status
            if comp_id in self._node_comp_ids:
                if status == "present":
                    resolve_status = "causal"
                elif status == "absent":
                    resolve_status = "unknown"
                elif status not in VALID_STATUSES:
                    raise StateError(
                        f"Invalid status: {status!r}. Must be one of {sorted(VALID_STATUSES)}"
                    )
            else:
                if status not in VALID_STATUSES:
                    raise StateError(
                        f"Invalid status: {status!r}. Must be one of {sorted(VALID_STATUSES)}"
                    )

            i = self.model_index[model_id]
            j = self.component_index[comp_id]
            self.tensor[i, j] = torch.tensor(
                STATUS_ENCODING[resolve_status], dtype=torch.uint8
            )

            if comp_id in self._node_comp_ids:
                self.node_present_mask[i, j] = resolve_status == "causal"

            affected_models.add(model_id)

        for mid in affected_models:
            self._recompute_edge_applicable(mid)

        self.packed_tensor = self._pack_status_tensor()

    def _recompute_edge_applicable(self, model_id: str) -> None:
        """Recompute edge applicability for a single model using stored edge->nodes mapping."""
        if model_id not in self.model_index:
            return
        i = self.model_index[model_id]

        for edge_cid, (src_cid, tgt_cid) in self._edge_to_nodes.items():
            if edge_cid not in self.component_index:
                continue
            ej = self.component_index[edge_cid]
            src_j = self.component_index.get(src_cid)
            tgt_j = self.component_index.get(tgt_cid)
            if src_j is not None and tgt_j is not None:
                self.edge_applicable_mask[i, ej] = bool(
                    self.node_present_mask[i, src_j].item()
                ) and bool(self.node_present_mask[i, tgt_j].item())

    # ------------------------------------------------------------------
    # Accessors
    # ------------------------------------------------------------------

    def get_status(self, model_id: str, comp_id: str) -> str:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {comp_id}")

        i = self.model_index[model_id]
        j = self.component_index[comp_id]
        key = tuple(self.tensor[i, j].tolist())
        return STATUS_DECODING.get(key, "unknown")

    def is_node_component(self, comp_id: str) -> bool:
        return comp_id in self._node_comp_ids

    def is_edge_component(self, comp_id: str) -> bool:
        return comp_id in self._edge_comp_ids

    def node_present(self, model_id: str, node_comp_id: str) -> bool:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if node_comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {node_comp_id}")
        i = self.model_index[model_id]
        j = self.component_index[node_comp_id]
        return bool(self.node_present_mask[i, j].item())

    def edge_applicable(self, model_id: str, edge_comp_id: str) -> bool:
        if model_id not in self.model_index:
            raise StateError(f"Unknown model ID: {model_id}")
        if edge_comp_id not in self.component_index:
            raise StateError(f"Unknown component ID: {edge_comp_id}")
        i = self.model_index[model_id]
        j = self.component_index[edge_comp_id]
        return bool(self.edge_applicable_mask[i, j].item())

    def edge_status(self, model_id: str, edge_comp_id: str) -> str | None:
        if not self.edge_applicable(model_id, edge_comp_id):
            return None
        return self.get_status(model_id, edge_comp_id)

    def get_effective_status(self, model_id: str, comp_id: str) -> str | None:
        if comp_id in self._node_comp_ids:
            return "present" if self.node_present(model_id, comp_id) else "absent"
        elif comp_id in self._edge_comp_ids:
            return self.edge_status(model_id, comp_id)
        return self.get_status(model_id, comp_id)

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
        np_row = self.node_present_mask[i]

        combined = (model_row.numpy().tobytes(), np_row.numpy().tobytes())
        if not include_timing:
            return hash(combined)

        timing = tuple(
            (cid, self.timing.get((model_id, cid))) for cid in self.component_ids
        )
        return hash((combined, timing))

    # ------------------------------------------------------------------
    # Serialization
    # ------------------------------------------------------------------

    def to_records(self) -> list[dict]:
        """Serialize state to a list of record dicts.

        Uses sparse semantics:
        - Only present nodes are emitted
        - Only applicable edges with resolved status are emitted
        - Unknown applicable edges may be omitted
        """
        records: list[dict] = []
        for mid in self.model_ids:
            for cid in self.component_ids:
                if cid in self._node_comp_ids:
                    if self.node_present(mid, cid):
                        t = self.get_timing(mid, cid)
                        records.append(
                            {
                                "model_id": mid,
                                "comp_id": cid,
                                "status": "present",
                                "timing": t,
                            }
                        )
                elif cid in self._edge_comp_ids:
                    if self.edge_applicable(mid, cid):
                        status = self.get_status(mid, cid)
                        records.append(
                            {
                                "model_id": mid,
                                "comp_id": cid,
                                "status": status,
                                "timing": None,
                            }
                        )
                else:
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

    def to_dense_records(self) -> list[dict]:
        """Serialize all model-component combinations (legacy dense format)."""
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

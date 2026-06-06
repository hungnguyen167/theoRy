from __future__ import annotations

import re

import pandas as pd
from pydantic import BaseModel, field_validator
from typing import Literal


class RegistryError(Exception):
    pass


class ComponentSchema(BaseModel):
    comp_id: str
    type: Literal["node", "edge"]
    source: str
    target: str | None = None
    direction: Literal["->", "<->"] | None = None
    description: str

    @field_validator("comp_id")
    @classmethod
    def validate_comp_id(cls, v: str) -> str:
        if not re.match(r"^C\d{4}$", v):
            raise ValueError(f"comp_id must match C{{NNNN}}, got {v!r}")
        return v

    @field_validator("source")
    @classmethod
    def source_nonempty(cls, v: str) -> str:
        if not v:
            raise ValueError("source must be non-empty")
        return v

    @field_validator("description")
    @classmethod
    def description_nonempty(cls, v: str) -> str:
        if not v:
            raise ValueError("description must be non-empty")
        return v

    @field_validator("target")
    @classmethod
    def validate_target(cls, v: str | None, info) -> str | None:
        if info.data.get("type") == "node" and v is not None:
            raise ValueError("node type must have target=None")
        if info.data.get("type") == "edge" and v is None:
            raise ValueError("edge type must have target set")
        return v

    @field_validator("direction")
    @classmethod
    def validate_direction(cls, v: str | None, info) -> str | None:
        if info.data.get("type") == "node" and v is not None:
            raise ValueError("node type must have direction=None")
        if info.data.get("type") == "edge" and v not in {"->", "<->"}:
            raise ValueError("edge type must have direction in {'->', '<->'}")
        return v


class ComponentRegistry:
    """Stores and validates a component registry."""

    REQUIRED_COLUMNS = {
        "comp_id",
        "type",
        "source",
        "target",
        "direction",
        "description",
    }

    def __init__(self, data: pd.DataFrame):
        missing = self.REQUIRED_COLUMNS - set(data.columns)
        if missing:
            raise RegistryError(f"Missing required columns: {missing}")
        self._data = data.copy()

    @property
    def data(self) -> pd.DataFrame:
        return self._data

    def summary(self) -> dict:
        n_total = len(self._data)
        n_nodes = int((self._data["type"] == "node").sum())
        n_edges = int((self._data["type"] == "edge").sum())
        n_directed = int(
            ((self._data["type"] == "edge") & (self._data["direction"] == "->")).sum()
        )
        n_bidirectional = int(
            ((self._data["type"] == "edge") & (self._data["direction"] == "<->")).sum()
        )
        return {
            "total_components": n_total,
            "nodes": n_nodes,
            "edges": n_edges,
            "directed_edges": n_directed,
            "bidirectional_edges": n_bidirectional,
        }

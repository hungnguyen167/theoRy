from __future__ import annotations

from pathlib import Path

import pandas as pd

from registry.schema import ComponentRegistry, ComponentSchema, RegistryError
from pydantic import ValidationError


def _normalize_nulls(df: pd.DataFrame) -> pd.DataFrame:
    for col in df.columns:
        if not pd.api.types.is_object_dtype(
            df[col]
        ) and not pd.api.types.is_string_dtype(df[col]):
            continue
        mask = df[col].isna()
        if mask.any():
            df[col] = df[col].astype(object)
            df.loc[mask, col] = None
    return df


class RegistryLoader:
    """Load and validate component registries."""

    @staticmethod
    def load(path: str | Path) -> ComponentRegistry:
        path = Path(path)
        if not path.exists():
            raise RegistryError(f"Registry file not found: {path}")
        if path.stat().st_size == 0:
            raise RegistryError(f"Registry file is empty: {path}")
        df = pd.read_parquet(path)
        if df.empty:
            raise RegistryError("Registry file contains no data")
        df = _normalize_nulls(df)
        records = df.to_dict(orient="records")
        try:
            validated = [ComponentSchema(**r) for r in records]
        except ValidationError as e:
            raise RegistryError(f"Registry validation failed: {e}") from e
        data = pd.DataFrame([v.model_dump() for v in validated])
        data = _normalize_nulls(data)
        return ComponentRegistry(data)

    @staticmethod
    def from_records(records: list[dict]) -> ComponentRegistry:
        if not records:
            raise RegistryError("No records provided")
        try:
            validated = [ComponentSchema(**r) for r in records]
        except ValidationError as e:
            raise RegistryError(f"Registry validation failed: {e}") from e
        data = pd.DataFrame([v.model_dump() for v in validated])
        data = _normalize_nulls(data)
        return ComponentRegistry(data)

from __future__ import annotations

import warnings
from pathlib import Path

import pandas as pd

from registry.schema import ComponentRegistry, RegistryError


class ComponentRegistryBuilder:
    """Generate a ComponentRegistry from a compact node / timing specification."""

    @staticmethod
    def from_nodes(
        nodes: list[dict],
        *,
        respect_timing: bool = True,
        include_bidirectional: bool = False,
        constraints: list[dict] | None = None,
    ) -> ComponentRegistry:
        """Build the full component registry.

        Parameters
        ----------
        nodes:
            List of dicts with keys ``name`` (required), ``timing`` (int or
            None), and ``description`` (str, optional).
        respect_timing:
            When ``True``, only generate directed edges where
            ``timing(source) < timing(target)``.
        include_bidirectional:
            When ``True``, also generate ``<->`` edge components for each
            unordered node pair.
        constraints:
            Optional list of dicts with ``source``, ``target``, ``direction``,
            and ``rule`` (one of ``allow``, ``forbid``, ``require``).
        """
        if not nodes:
            raise RegistryError("At least one node is required to build a registry")

        node_names = {n["name"] for n in nodes}
        if len(node_names) != len(nodes):
            raise RegistryError("Duplicate node names are not allowed")

        forbidden_set: set[tuple[str, str, str]] = set()
        required_set: set[tuple[str, str, str]] = set()
        allowed_set: set[tuple[str, str, str]] | None = None

        if constraints:
            allowed_pairs: set[tuple[str, str, str]] = set()
            for c in constraints:
                src = c["source"]
                tgt = c["target"]
                direction = c.get("direction", "->")
                rule = c["rule"]
                if src not in node_names or tgt not in node_names:
                    raise RegistryError(
                        f"Constraint references unknown node: {src!r} or {tgt!r}"
                    )
                if src == tgt:
                    raise RegistryError(
                        f"Self-edge constraint not allowed: {src!r} -> {tgt!r}"
                    )
                # Normalize bidirectional pair ordering
                if direction == "<->":
                    src, tgt = min(src, tgt), max(src, tgt)
                triplet = (src, tgt, direction)
                if rule == "forbid":
                    if triplet in required_set:
                        raise RegistryError(
                            f"Contradictory constraint: {src!r} {direction} {tgt!r} "
                            f"is both required and forbidden"
                        )
                    forbidden_set.add(triplet)
                elif rule == "require":
                    if triplet in forbidden_set:
                        raise RegistryError(
                            f"Contradictory constraint: {src!r} {direction} {tgt!r} "
                            f"is both required and forbidden"
                        )
                    required_set.add(triplet)
                elif rule == "allow":
                    allowed_pairs.add(triplet)
            if allowed_pairs:
                allowed_set = allowed_pairs

        # --- node components -------------------------------------------------
        registry_records: list[dict] = []
        next_id = 1

        for n in nodes:
            registry_records.append(
                {
                    "comp_id": f"C{next_id:04d}",
                    "type": "node",
                    "source": n["name"],
                    "target": None,
                    "direction": None,
                    "description": n.get("description") or n["name"],
                }
            )
            next_id += 1

        # --- directed edges --------------------------------------------------
        for source_node in nodes:
            for target_node in nodes:
                if source_node["name"] == target_node["name"]:
                    continue

                if respect_timing:
                    s_t = source_node.get("timing")
                    t_t = target_node.get("timing")
                    if s_t is None or t_t is None:
                        warnings.warn(
                            "Temporal integrity cannot be fully enforced: "
                            "some nodes have missing timing values"
                        )
                    elif s_t >= t_t:
                        continue

                triplet = (source_node["name"], target_node["name"], "->")

                if triplet in forbidden_set:
                    continue

                if allowed_set is not None and triplet not in allowed_set:
                    continue

                registry_records.append(
                    {
                        "comp_id": f"C{next_id:04d}",
                        "type": "edge",
                        "source": source_node["name"],
                        "target": target_node["name"],
                        "direction": "->",
                        "description": f"{source_node['name']} -> {target_node['name']}",
                    }
                )
                next_id += 1

        # --- bidirectional edges ---------------------------------------------
        if include_bidirectional:
            seen: set[tuple[str, str]] = set()
            for source_node in nodes:
                for target_node in nodes:
                    if source_node["name"] == target_node["name"]:
                        continue
                    pair = (
                        min(source_node["name"], target_node["name"]),
                        max(source_node["name"], target_node["name"]),
                    )
                    if pair in seen:
                        continue
                    seen.add(pair)

                    triplet = (source_node["name"], target_node["name"], "<->")
                    if triplet in forbidden_set:
                        continue
                    if allowed_set is not None and triplet not in allowed_set:
                        continue

                    registry_records.append(
                        {
                            "comp_id": f"C{next_id:04d}",
                            "type": "edge",
                            "source": pair[0],
                            "target": pair[1],
                            "direction": "<->",
                            "description": f"{pair[0]} <-> {pair[1]}",
                        }
                    )
                    next_id += 1

        # --- required edges that were not generated -------------------------
        node_timing_map = {n["name"]: n.get("timing") for n in nodes}
        for req in required_set:
            if req in {
                (r["source"], r["target"], r["direction"]) for r in registry_records
            }:
                continue
            # Check temporal validity for required edges
            if respect_timing:
                s_t = node_timing_map.get(req[0])
                t_t = node_timing_map.get(req[1])
                if s_t is not None and t_t is not None and s_t >= t_t:
                    raise RegistryError(
                        f"Required edge {req[0]} {req[1]} violates timing: "
                        f"timing({req[0]})={s_t} >= timing({req[1]})={t_t}"
                    )
            registry_records.append(
                {
                    "comp_id": f"C{next_id:04d}",
                    "type": "edge",
                    "source": req[0],
                    "target": req[1],
                    "direction": req[2],
                    "description": f"{req[0]} {req[2]} {req[1]} (required)",
                }
            )
            next_id += 1

        data = pd.DataFrame(registry_records)
        data = data.astype(object)
        for col in ("target", "direction"):
            data[col] = data[col].where(pd.notna(data[col]), None)

        return ComponentRegistry(data)

    @staticmethod
    def to_parquet(registry: ComponentRegistry, path: str | Path) -> None:
        """Write a ComponentRegistry to a Parquet file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        registry.data.to_parquet(path, index=False)

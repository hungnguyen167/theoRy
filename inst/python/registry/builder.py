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
        exposure: str | None = None,
        outcome: str | None = None,
    ) -> ComponentRegistry:
        """Build the full component registry.

        Parameters
        ----------
        nodes:
            List of dicts with keys ``name`` (required), ``timing`` (int or
            None), ``timing_options`` (list[int], optional), and
            ``description`` (str, optional).
        respect_timing:
            When ``True``, only generate directed edges where
            ``timing(source) < timing(target)``.
        include_bidirectional:
            When ``True``, also generate ``<->`` edge components for unordered
            node pairs that can occupy the same time when timing is respected.
        constraints:
            Optional list of dicts with ``source``, ``target``, ``direction``,
            and ``rule`` (one of ``allow``, ``forbid``, ``require``).
        exposure, outcome:
            Optional causal target nodes. When both are provided,
            ``exposure -> outcome`` is created with ``fixed_status = "causal"``.
        """
        if not nodes:
            raise RegistryError("At least one node is required to build a registry")

        for node in nodes:
            timing = node.get("timing")
            if timing is not None:
                if isinstance(timing, bool) or not isinstance(timing, int):
                    raise RegistryError(
                        f"Timing for node {node.get('name')!r} must be an integer"
                    )
                if timing < 1:
                    raise RegistryError(
                        f"Timing for node {node.get('name')!r} must be at least 1"
                    )

            timing_options = node.get("timing_options")
            if timing_options is not None:
                for value in timing_options:
                    if isinstance(value, bool) or not isinstance(value, int):
                        raise RegistryError(
                            f"Timing options for node {node.get('name')!r} "
                            "must contain integers"
                        )
                    if value < 1:
                        raise RegistryError(
                            f"Timing options for node {node.get('name')!r} "
                            "must contain values at least 1"
                        )

        node_names = {n["name"] for n in nodes}
        if len(node_names) != len(nodes):
            raise RegistryError("Duplicate node names are not allowed")

        if (exposure is None) != (outcome is None):
            raise RegistryError(
                "Both or neither of exposure and outcome must be provided"
            )

        implicit_exposure_outcome_order = False
        if exposure is not None and outcome is not None:
            if exposure not in node_names:
                raise RegistryError(f"Exposure '{exposure}' is not in the node list")
            if outcome not in node_names:
                raise RegistryError(f"Outcome '{outcome}' is not in the node list")
            if exposure == outcome:
                raise RegistryError("Exposure and outcome must be distinct nodes")

            all_timing_none = all(
                n.get("timing") is None and n.get("timing_options") is None
                for n in nodes
            )
            if all_timing_none:
                implicit_exposure_outcome_order = True

        forbidden_set: set[tuple[str, str, str]] = set()
        required_set: set[tuple[str, str, str]] = set()
        allowed_directed: set[tuple[str, str, str]] = set()
        allowed_bidirectional: set[tuple[str, str, str]] = set()

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
            allowed_directed = {
                triplet for triplet in allowed_pairs if triplet[2] == "->"
            }
            allowed_bidirectional = {
                triplet for triplet in allowed_pairs if triplet[2] == "<->"
            }

        if exposure is not None and outcome is not None:
            exp_out = (exposure, outcome, "->")
            if exp_out in forbidden_set:
                raise RegistryError(
                    f"Cannot forbid {exposure} -> {outcome}: "
                    "exposure -> outcome is fixed as causal"
                )
        if implicit_exposure_outcome_order:
            out_exp = (outcome, exposure, "->")
            exp_out_bi = (min(exposure, outcome), max(exposure, outcome), "<->")
            if out_exp in required_set:
                raise RegistryError(
                    f"Cannot require {outcome} -> {exposure}: "
                    f"exposure must precede outcome when no timing is supplied"
                )
            if exp_out_bi in required_set:
                raise RegistryError(
                    f"Cannot require {exposure} <-> {outcome}: "
                    f"exposure must precede outcome when no timing is supplied"
                )

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
                    "fixed_status": None,
                    "observed": n.get("observed", True),
                }
            )
            next_id += 1

        # --- directed edges --------------------------------------------------
        for source_node in nodes:
            for target_node in nodes:
                if source_node["name"] == target_node["name"]:
                    continue

                if implicit_exposure_outcome_order:
                    if (
                        source_node["name"] == outcome
                        and target_node["name"] == exposure
                    ):
                        continue

                triplet = (source_node["name"], target_node["name"], "->")
                is_exposure_outcome = (
                    source_node["name"] == exposure and target_node["name"] == outcome
                )
                if respect_timing:
                    s_options = source_node.get("timing_options")
                    t_options = target_node.get("timing_options")
                    if s_options is None:
                        s_t = source_node.get("timing")
                        s_options = [s_t] if s_t is not None else None
                    if t_options is None:
                        t_t = target_node.get("timing")
                        t_options = [t_t] if t_t is not None else None
                    if s_options is None or t_options is None:
                        if not implicit_exposure_outcome_order:
                            warnings.warn(
                                "Temporal integrity cannot be fully enforced: "
                                "some nodes have missing timing values"
                            )
                    elif (
                        not any(s_t < t_t for s_t in s_options for t_t in t_options)
                        and not is_exposure_outcome
                    ):
                        continue

                if triplet in forbidden_set:
                    continue

                if (
                    allowed_directed
                    and triplet not in allowed_directed
                    and not is_exposure_outcome
                ):
                    continue

                is_fixed = (is_exposure_outcome) or triplet in required_set

                registry_records.append(
                    {
                        "comp_id": f"C{next_id:04d}",
                        "type": "edge",
                        "source": source_node["name"],
                        "target": target_node["name"],
                        "direction": "->",
                        "description": f"{source_node['name']} -> {target_node['name']}",
                        "fixed_status": "causal" if is_fixed else None,
                        "observed": True,
                    }
                )
                next_id += 1

        # --- bidirectional edges ---------------------------------------------
        if include_bidirectional or allowed_bidirectional:
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

                    if implicit_exposure_outcome_order and set(pair) == {
                        exposure,
                        outcome,
                    }:
                        continue

                    triplet = (pair[0], pair[1], "<->")
                    if (
                        not include_bidirectional
                        and triplet not in allowed_bidirectional
                    ):
                        continue
                    if triplet in forbidden_set:
                        continue

                    is_explicit = (
                        triplet in allowed_bidirectional or triplet in required_set
                    )
                    if respect_timing and not is_explicit:
                        source_options = source_node.get("timing_options")
                        target_options = target_node.get("timing_options")
                        if source_options is None:
                            source_time = source_node.get("timing")
                            source_options = (
                                [source_time] if source_time is not None else None
                            )
                        if target_options is None:
                            target_time = target_node.get("timing")
                            target_options = (
                                [target_time] if target_time is not None else None
                            )
                        if (
                            source_options is not None
                            and target_options is not None
                            and set(source_options).isdisjoint(target_options)
                        ):
                            continue

                    is_fixed = triplet in required_set
                    registry_records.append(
                        {
                            "comp_id": f"C{next_id:04d}",
                            "type": "edge",
                            "source": pair[0],
                            "target": pair[1],
                            "direction": "<->",
                            "description": f"{pair[0]} <-> {pair[1]}",
                            "fixed_status": "causal" if is_fixed else None,
                            "observed": True,
                        }
                    )
                    next_id += 1

        # --- required edges that were not generated -------------------------
        node_timing_map = {n["name"]: n.get("timing") for n in nodes}
        for req in sorted(required_set):
            if req in {
                (r["source"], r["target"], r["direction"]) for r in registry_records
            }:
                continue
            if implicit_exposure_outcome_order:
                if req[0] == outcome and req[1] == exposure and req[2] == "->":
                    raise RegistryError(
                        f"Required edge {req[0]} {req[2]} {req[1]} contradicts "
                        f"implicit exposure-before-outcome ordering"
                    )
                if req[2] == "<->" and set(req[:2]) == {exposure, outcome}:
                    raise RegistryError(
                        f"Required edge {req[0]} {req[2]} {req[1]} contradicts "
                        f"implicit exposure-before-outcome ordering"
                    )

            # Legacy fixed timings fail at build time. Flexible timing options
            # are deferred to state expansion, where invalid assignments are pruned.
            if respect_timing and req[2] == "->":
                s_t = node_timing_map.get(req[0])
                t_t = node_timing_map.get(req[1])
                has_timing_options = any(
                    n.get("timing_options") is not None
                    for n in nodes
                    if n["name"] in {req[0], req[1]}
                )
                if (
                    not has_timing_options
                    and s_t is not None
                    and t_t is not None
                    and s_t >= t_t
                ):
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
                    "fixed_status": "causal",
                    "observed": True,
                }
            )
            next_id += 1

        data = pd.DataFrame(registry_records)
        data = data.astype(object)
        # Keep nullable registry fields as JSON-compatible ``None`` values.
        # Pandas otherwise materializes mixed ``None``/string columns as
        # floating-point NaN, which leaks into simulation artifacts.
        for col in ("target", "direction", "fixed_status"):
            data[col] = data[col].where(pd.notna(data[col]), None)

        return ComponentRegistry(data)

    @staticmethod
    def to_parquet(registry: ComponentRegistry, path: str | Path) -> None:
        """Write a ComponentRegistry to a Parquet file."""
        path = Path(path)
        path.parent.mkdir(parents=True, exist_ok=True)
        registry.data.to_parquet(path, index=False)

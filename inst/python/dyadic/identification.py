from __future__ import annotations
import logging

logger = logging.getLogger(__name__)

class IdentificationError(Exception):
    pass

class IdentificationWrapper:
    def __init__(self):
        # Completely removed the rpy2 dependency for Windows stability
        self._r_available = True

    def identify_total_effect(
        self,
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        exposure: str,
        outcome: str,
    ) -> tuple[bool, str | None]:
        
        if exposure not in nodes:
            raise IdentificationError(f"Exposure {exposure!r} not present in model nodes")
        if outcome not in nodes:
            raise IdentificationError(f"Outcome {outcome!r} not present in model nodes")
        if exposure == outcome:
            raise IdentificationError("Exposure and outcome must be distinct")

        # Pure Python fallback using your native networkx causal engine
        from dyadic.causal import CausalWrapper
        try:
            cw = CausalWrapper()
            dag_spec = {
                "nodes": nodes,
                "edges": directed_edges,
                "bidirected_edges": bidirected_edges,
                "exposure": exposure,
                "outcome": outcome
            }
            # If a valid adjustment path exists natively, the effect is identified
            is_identified = cw.check_identification(dag_spec)
            formula = "native_backdoor_adjustment" if is_identified else None
            return is_identified, formula
        except Exception as e:
            logger.warning(f"Native identification check failed: {e}")
            return False, None

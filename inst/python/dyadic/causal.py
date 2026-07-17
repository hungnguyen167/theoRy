from __future__ import annotations

import logging

logger = logging.getLogger(__name__)


class CausalError(Exception):
    pass


class CausalWrapper:
    def __init__(self):
        self._r_available: bool | None = None

    def _ensure_r(self) -> bool:
        if self._r_available is True:
            return True
        if self._r_available is False:
            raise CausalError("R/dagitty not available (already checked this session)")
        try:
            import rpy2.robjects as ro  # noqa: F401
        except ImportError as e:
            self._r_available = False
            raise CausalError(
                "rpy2 is not installed. Install with: pip install rpy2"
            ) from e
        except OSError as e:
            self._r_available = False
            raise CausalError(
                "Cannot load R shared library via rpy2 (likely a "
                "libstdc++ version mismatch between your Python "
                "environment and system R). "
                "Try: LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libstdc++.so.6\n"
                f"Underlying error: {e}"
            ) from e
        except Exception as e:
            self._r_available = False
            raise CausalError(f"Unexpected error importing rpy2: {e}") from e

        try:
            from rpy2.robjects.packages import importr
            from rpy2.robjects import conversion, default_converter

            with conversion.localconverter(default_converter):
                importr("dagitty")

            self._r_available = True
            return True
        except OSError as e:
            self._r_available = False
            raise CausalError(
                "Cannot load R shared library when loading dagitty "
                "(likely a libstdc++ version mismatch). "
                "Try: LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libstdc++.so.6\n"
                f"Underlying error: {e}"
            ) from e
        except Exception as e:
            self._r_available = False
            raise CausalError(
                "Cannot load R package dagitty. "
                "Install in R with: install.packages('dagitty')\n"
                f"Underlying error: {e}"
            ) from e

    @staticmethod
    def _dag_spec_to_dagitty(dag_spec: dict) -> str:
        nodes = dag_spec.get("nodes", [])
        edges = dag_spec.get("edges", [])
        bidirected_edges = dag_spec.get("bidirected_edges", [])
        latent_nodes = CausalWrapper._latent_nodes(dag_spec)

        if not nodes:
            raise CausalError("DAG spec must contain at least one node")

        lines = ["dag {"]
        for node in nodes:
            suffix = " [unobserved]" if node in latent_nodes else ""
            lines.append(f"  {node}{suffix}")
        for edge in edges:
            if isinstance(edge, (list, tuple)) and len(edge) == 2:
                lines.append(f"  {edge[0]} -> {edge[1]}")
        for edge in bidirected_edges:
            if isinstance(edge, (list, tuple)) and len(edge) == 2:
                lines.append(f"  {edge[0]} <-> {edge[1]}")
        lines.append("}")
        return "\n".join(lines)

    @staticmethod
    def _latent_nodes(dag_spec: dict) -> set[str]:
        nodes = set(dag_spec.get("nodes", []))
        latent = set(dag_spec.get("latent_nodes", []))
        observed = dag_spec.get("observed_nodes")
        if observed is not None:
            latent.update(nodes - set(observed))
        return latent & nodes

    def _resolve_endpoints(self, dag_spec: dict) -> tuple[str | None, str | None]:
        nodes = dag_spec.get("nodes", [])
        exposure = dag_spec.get("exposure")
        outcome = dag_spec.get("outcome")

        if exposure is None and len(nodes) >= 2:
            exposure = nodes[0]
        if outcome is None and len(nodes) >= 2:
            outcome = nodes[-1]

        return exposure, outcome

    def compute_adjustment_sets(self, dag_spec: dict) -> list[list[str]]:
        self._ensure_r()

        try:
            from rpy2.robjects import conversion, default_converter
            from rpy2.robjects.packages import importr
            from rpy2.robjects.vectors import StrVector

            with conversion.localconverter(default_converter):
                dagitty = importr("dagitty")

                dag_syntax = self._dag_spec_to_dagitty(dag_spec)
                dag_obj = dagitty.dagitty(dag_syntax)

                exposure, outcome = self._resolve_endpoints(dag_spec)
                if exposure is None or outcome is None:
                    raise CausalError(
                        "At least two nodes and exposure/outcome required "
                        "for adjustment sets"
                    )

                exposure_vec = StrVector([exposure])
                outcome_vec = StrVector([outcome])

                adj_sets = dagitty.adjustmentSets(
                    dag_obj,
                    exposure=exposure_vec,
                    outcome=outcome_vec,
                    effect="total",
                )

                result: list[list[str]] = []
                latent_nodes = self._latent_nodes(dag_spec)
                for s in adj_sets:
                    adjustment = list(s)
                    if not latent_nodes.intersection(adjustment):
                        result.append(adjustment)

            return result
        except Exception as e:
            if isinstance(e, CausalError):
                raise
            raise CausalError(f"Failed to compute adjustment sets: {e}")

    def check_identification(self, dag_spec: dict) -> bool:
        try:
            self._ensure_r()
        except CausalError:
            logger.warning("R/dagitty unavailable; assuming model not identified")
            return False

        try:
            from rpy2.robjects import conversion, default_converter
            from rpy2.robjects.packages import importr

            with conversion.localconverter(default_converter):
                dagitty = importr("dagitty")

                dag_syntax = self._dag_spec_to_dagitty(dag_spec)
                dag_obj = dagitty.dagitty(dag_syntax)

                exposure, outcome = self._resolve_endpoints(dag_spec)
                if exposure is None or outcome is None:
                    return False

                from rpy2.robjects.vectors import StrVector

                adj_sets = dagitty.adjustmentSets(
                    dag_obj,
                    exposure=StrVector([exposure]),
                    outcome=StrVector([outcome]),
                )
                return len(list(adj_sets)) > 0
        except CausalError:
            raise
        except Exception as e:
            logger.warning(f"Identification check failed: {e}")
            return False

    def compare_mas(
        self,
        mas_a: list[str] | list[list[str]] | None,
        mas_b: list[str] | list[list[str]] | None,
    ) -> dict:
        if mas_a is None or mas_b is None:
            return {"compatible": None}

        def _normalize(sets):
            if not sets:
                return set()
            if all(isinstance(s, str) for s in sets):
                return {frozenset(filter(None, sets))}
            result = set()
            for s in sets:
                if isinstance(s, (list, tuple)):
                    result.add(frozenset(s))
                elif isinstance(s, str):
                    result.add(frozenset({s}))
            return result

        sets_a = _normalize(mas_a)
        sets_b = _normalize(mas_b)

        if not sets_a and not sets_b:
            compatible = False
        elif not sets_a or not sets_b:
            compatible = False
        else:
            compatible = bool(sets_a & sets_b)

        return {"compatible": compatible}

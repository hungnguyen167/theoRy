from __future__ import annotations

import logging
from typing import Literal

from dyadic.causal import (
    CausalError,
    NativeCausalUnsupportedError,
    native_backdoor_adjustment_sets,
)

logger = logging.getLogger(__name__)


class IdentificationError(Exception):
    pass


class NativeIdentificationUnsupportedError(IdentificationError):
    """Raised when general identification exceeds the native backdoor scope."""


class IdentificationBackendUnavailableError(IdentificationError):
    """Raised when the optional R/CausalEffect backend cannot be used."""


class IdentificationWrapper:
    def __init__(self, causal_backend: Literal["auto", "native", "r"] = "r"):
        if causal_backend not in {"auto", "native", "r"}:
            raise ValueError("causal_backend must be one of: auto, native, r")
        self.causal_backend = causal_backend
        self._r_available: bool | None = None

    def _ensure_r(self) -> bool:
        if self._r_available is True:
            return True
        if self._r_available is False:
            raise IdentificationBackendUnavailableError(
                "R/causaleffect not available (already checked this session)"
            )
        try:
            import rpy2.robjects as ro  # noqa: F401
        except ImportError as e:
            self._r_available = False
            raise IdentificationBackendUnavailableError(
                "rpy2 is not installed. Install with: pip install rpy2"
            ) from e
        except OSError as e:
            self._r_available = False
            raise IdentificationBackendUnavailableError(
                "Cannot load the R shared library via rpy2. Ensure R is "
                "installed and R_HOME is configured. On Linux with Conda, try: "
                "LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libstdc++.so.6\n"
                f"Underlying error: {e}"
            ) from e
        except Exception as e:
            self._r_available = False
            raise IdentificationBackendUnavailableError(
                f"Unexpected error importing rpy2: {e}"
            ) from e

        try:
            import rpy2.robjects as ro
            from rpy2.robjects import conversion, default_converter

            with conversion.localconverter(default_converter):
                ro.r("suppressMessages(library(causaleffect))")

            self._r_available = True
            return True
        except Exception as e:
            self._r_available = False
            raise IdentificationBackendUnavailableError(
                "Cannot load R package causaleffect. "
                "Install in R with: install.packages('causaleffect')\n"
                f"Underlying error: {e}"
            ) from e

    def identify_total_effect(
        self,
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        exposure: str,
        outcome: str,
    ) -> tuple[bool | None, str | None]:
        if exposure not in nodes:
            raise IdentificationError(
                f"Exposure {exposure!r} not present in model nodes {nodes}"
            )
        if outcome not in nodes:
            raise IdentificationError(
                f"Outcome {outcome!r} not present in model nodes {nodes}"
            )
        if exposure == outcome:
            raise IdentificationError(
                f"Exposure and outcome must be distinct, got both {exposure!r}"
            )

        if self.causal_backend == "native":
            return self._identify_total_effect_native(
                nodes, directed_edges, bidirected_edges, exposure, outcome
            )
        if self.causal_backend == "auto":
            try:
                return self._identify_total_effect_native(
                    nodes, directed_edges, bidirected_edges, exposure, outcome
                )
            except NativeIdentificationUnsupportedError:
                try:
                    return self._identify_total_effect_r(
                        nodes, directed_edges, bidirected_edges, exposure, outcome
                    )
                except IdentificationBackendUnavailableError as e:
                    logger.warning(
                        "R/CausalEffect is unavailable; general identification "
                        "will be reported as unavailable: %s",
                        e,
                    )
                    return None, None
        return self._identify_total_effect_r(
            nodes, directed_edges, bidirected_edges, exposure, outcome
        )

    def _identify_total_effect_native(
        self,
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        exposure: str,
        outcome: str,
    ) -> tuple[bool, str | None]:
        try:
            adjustment_sets = native_backdoor_adjustment_sets(
                {
                    "nodes": nodes,
                    "edges": directed_edges,
                    "bidirected_edges": bidirected_edges,
                    "exposure": exposure,
                    "outcome": outcome,
                }
            )
        except NativeCausalUnsupportedError as e:
            raise NativeIdentificationUnsupportedError(str(e)) from e
        except CausalError as e:
            raise IdentificationError(
                f"Native backdoor identification failed: {e}"
            ) from e

        if not adjustment_sets:
            raise NativeIdentificationUnsupportedError(
                "Native identification supports total effects with a valid "
                "backdoor adjustment set; use causal_backend='r' for general ID"
            )

        adjustment = adjustment_sets[0]
        if adjustment:
            covariates = ", ".join(adjustment)
            formula = (
                f"P({outcome} | do({exposure})) = sum_{{{covariates}}} "
                f"P({outcome} | {exposure}, {covariates}) P({covariates})"
            )
        else:
            formula = f"P({outcome} | do({exposure})) = P({outcome} | {exposure})"
        return True, formula

    def _identify_total_effect_r(
        self,
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        exposure: str,
        outcome: str,
    ) -> tuple[bool, str | None]:
        self._ensure_r()

        import rpy2.robjects as ro
        from rpy2.robjects import conversion, default_converter

        with conversion.localconverter(default_converter):
            r_code = self._build_r_code(
                nodes, directed_edges, bidirected_edges, exposure, outcome
            )
            try:
                result = ro.r(r_code)
            except Exception as e:
                msg = str(e)
                if "Set 'x' contains variables not present" in msg:
                    raise IdentificationError(
                        f"Exposure {exposure!r} not present in graph"
                    )
                if "Set 'y' contains variables not present" in msg:
                    raise IdentificationError(
                        f"Outcome {outcome!r} not present in graph"
                    )
                raise IdentificationError(f"Identification failed: {e}")

            if result is None or str(result) == "":
                return False, None

            result_str = (
                str(result[0])
                if hasattr(result, "__len__") and len(result) > 0
                else str(result)
            )

            if result_str.startswith("not_identified"):
                return False, None

            return True, result_str

    @staticmethod
    def _build_r_code(
        nodes: list[str],
        directed_edges: list[tuple[str, str]],
        bidirected_edges: list[tuple[str, str]],
        exposure: str,
        outcome: str,
    ) -> str:
        def r_string(value: str) -> str:
            escaped = value.replace("\\", "\\\\").replace('"', '\\"')
            escaped = escaped.replace("\n", "\\n").replace("\r", "\\r")
            return f'"{escaped}"'

        valid_nodes = set(nodes)
        directed = [
            (source, target)
            for source, target in directed_edges
            if source in valid_nodes and target in valid_nodes
        ]
        bidirected = []
        seen_bidirected = set()
        for source, target in bidirected_edges:
            if source not in valid_nodes or target not in valid_nodes:
                continue
            key = tuple(sorted((source, target)))
            if key not in seen_bidirected:
                seen_bidirected.add(key)
                bidirected.append((source, target))

        graph_edges = directed + [
            edge
            for source, target in bidirected
            for edge in ((source, target), (target, source))
        ]
        node_vector = "c(" + ", ".join(r_string(node) for node in nodes) + ")"

        lines = [
            "suppressMessages(library(igraph))",
            "suppressMessages(library(causaleffect))",
            f"node_names <- {node_vector}",
        ]

        if graph_edges:
            edge_vector = (
                "c("
                + ", ".join(r_string(value) for edge in graph_edges for value in edge)
                + ")"
            )
            lines.append(
                f"edge_matrix <- matrix({edge_vector}, ncol = 2, byrow = TRUE)"
            )
            lines.append("g <- graph_from_edgelist(edge_matrix, directed = TRUE)")
            lines.append("missing_nodes <- setdiff(node_names, V(g)$name)")
            lines.append(
                "if (length(missing_nodes)) g <- add_vertices(g, "
                "length(missing_nodes), name = missing_nodes)"
            )
        else:
            lines.append(
                "g <- make_empty_graph(n = length(node_names), directed = TRUE)"
            )
            lines.append("V(g)$name <- node_names")

        if bidirected:
            bidirected_offset = len(directed)
            edge_indices = [
                bidirected_offset + 2 * i + offset
                for i in range(len(bidirected))
                for offset in (1, 2)
            ]
            indices_str = "c(" + ", ".join(str(x) for x in edge_indices) + ")"
            lines.append(f'g <- set_edge_attr(g, "description", {indices_str}, "U")')

        lines.append(
            "result <- tryCatch({"
            f"  causal.effect(y = {r_string(outcome)}, x = {r_string(exposure)},"
            "                      z = NULL, G = g, expr = TRUE)"
            "}, error = function(e) {"
            '  paste("not_identified:", e$message)'
            "})"
        )
        lines.append("result")

        return "\n".join(lines)

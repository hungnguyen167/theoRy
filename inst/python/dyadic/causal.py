from __future__ import annotations

import logging
from itertools import combinations
from typing import Literal

import networkx as nx

logger = logging.getLogger(__name__)


class CausalError(Exception):
    pass


class NativeCausalUnsupportedError(CausalError):
    """Raised when a query falls outside the native backdoor solver's scope."""


_NATIVE_MAX_ADJUSTMENT_CANDIDATES = 12


def _latent_nodes(dag_spec: dict) -> set[str]:
    nodes = set(dag_spec.get("nodes", []))
    latent = set(dag_spec.get("latent_nodes", []))
    observed = dag_spec.get("observed_nodes")
    if observed is not None:
        latent.update(nodes - set(observed))
    return latent & nodes


def _resolve_endpoints(dag_spec: dict) -> tuple[str | None, str | None]:
    nodes = dag_spec.get("nodes", [])
    exposure = dag_spec.get("exposure")
    outcome = dag_spec.get("outcome")

    if exposure is None and len(nodes) >= 2:
        exposure = nodes[0]
    if outcome is None and len(nodes) >= 2:
        outcome = nodes[-1]

    return exposure, outcome


def _native_graph(dag_spec: dict) -> tuple[nx.DiGraph, set[str], str, str]:
    nodes = dag_spec.get("nodes", [])
    directed_edges = dag_spec.get("edges", [])
    bidirected_edges = dag_spec.get("bidirected_edges", [])

    if not isinstance(nodes, list) or not nodes:
        raise CausalError("DAG spec must contain at least one node")
    if not all(isinstance(node, str) and node for node in nodes):
        raise CausalError("DAG spec nodes must be non-empty strings")
    if len(nodes) != len(set(nodes)):
        raise CausalError("DAG spec nodes must be unique")
    if not isinstance(directed_edges, list) or not isinstance(bidirected_edges, list):
        raise CausalError("DAG spec edges and bidirected_edges must be lists")

    exposure, outcome = _resolve_endpoints(dag_spec)
    if exposure is None or outcome is None:
        raise CausalError(
            "At least two nodes and exposure/outcome required for adjustment sets"
        )
    if exposure not in nodes or outcome not in nodes:
        raise CausalError("Exposure and outcome must be present in DAG spec nodes")
    if exposure == outcome:
        raise CausalError("Exposure and outcome must be distinct")

    latent_nodes = _latent_nodes(dag_spec)
    if exposure in latent_nodes or outcome in latent_nodes:
        raise CausalError("Exposure and outcome must be observed nodes")

    graph = nx.DiGraph()
    graph.add_nodes_from(nodes)

    def add_directed_edges(edges: list, *, bidirected: bool = False) -> None:
        for edge in edges:
            if not isinstance(edge, (list, tuple)) or len(edge) != 2:
                raise CausalError("Each DAG edge must be a source-target pair")
            source, target = edge
            if source not in graph or target not in graph:
                raise CausalError(
                    f"DAG edge ({source!r}, {target!r}) references an unknown node"
                )
            if source == target:
                raise CausalError("DAG edges must not be self-loops")
            if bidirected:
                latent_name = f"__theory_bidirected_latent_{len(graph)}"
                while latent_name in graph:
                    latent_name += "_"
                graph.add_edge(latent_name, source)
                graph.add_edge(latent_name, target)
            else:
                graph.add_edge(source, target)

    add_directed_edges(directed_edges)
    add_directed_edges(bidirected_edges, bidirected=True)

    if not nx.is_directed_acyclic_graph(graph):
        raise CausalError(
            "Native backdoor adjustment requires a directed acyclic graph"
        )

    return graph, set(nodes) - latent_nodes, exposure, outcome


def _is_d_separated(
    graph: nx.DiGraph, exposure: str, outcome: str, adjustment: set[str]
) -> bool:
    """Use ancestral moralization to test d-separation in a DAG."""
    ancestors = {exposure, outcome, *adjustment}
    for node in tuple(ancestors):
        ancestors.update(nx.ancestors(graph, node))

    ancestral_graph = graph.subgraph(ancestors)
    moral_graph = nx.Graph()
    moral_graph.add_nodes_from(ancestral_graph)
    moral_graph.add_edges_from(ancestral_graph.edges)
    for node in ancestral_graph:
        parents = list(ancestral_graph.predecessors(node))
        moral_graph.add_edges_from(combinations(parents, 2))

    moral_graph.remove_nodes_from(adjustment)
    return not nx.has_path(moral_graph, exposure, outcome)


def native_backdoor_adjustment_sets(dag_spec: dict) -> list[list[str]]:
    """Return all minimal observed adjustment sets for a total-effect query.

    The native backend supports single-treatment, single-outcome DAG queries.
    Bidirected edges are represented as a fresh latent common cause before
    applying the backdoor criterion. General ID cases, such as front-door
    identification, are deliberately outside this solver's scope.
    """
    graph, observed_nodes, exposure, outcome = _native_graph(dag_spec)
    treatment_descendants = nx.descendants(graph, exposure)
    graph.remove_edges_from(list(graph.out_edges(exposure)))

    eligible = sorted(observed_nodes - {exposure, outcome} - treatment_descendants)
    if len(eligible) > _NATIVE_MAX_ADJUSTMENT_CANDIDATES:
        raise NativeCausalUnsupportedError(
            "Native backdoor adjustment supports at most "
            f"{_NATIVE_MAX_ADJUSTMENT_CANDIDATES} eligible observed covariates"
        )

    adjustment_sets: list[list[str]] = []
    for size in range(len(eligible) + 1):
        for candidate in combinations(eligible, size):
            candidate_set = set(candidate)
            if any(
                set(existing).issubset(candidate_set) for existing in adjustment_sets
            ):
                continue
            if _is_d_separated(graph, exposure, outcome, candidate_set):
                adjustment_sets.append(list(candidate))

    return adjustment_sets


class CausalWrapper:
    def __init__(self, causal_backend: Literal["auto", "native", "r"] = "r"):
        if causal_backend not in {"auto", "native", "r"}:
            raise ValueError("causal_backend must be one of: auto, native, r")
        self.causal_backend = causal_backend
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
        return _latent_nodes(dag_spec)

    def _resolve_endpoints(self, dag_spec: dict) -> tuple[str | None, str | None]:
        return _resolve_endpoints(dag_spec)

    def compute_adjustment_sets(self, dag_spec: dict) -> list[list[str]]:
        if self.causal_backend == "native":
            return native_backdoor_adjustment_sets(dag_spec)
        if self.causal_backend == "auto":
            try:
                return native_backdoor_adjustment_sets(dag_spec)
            except NativeCausalUnsupportedError:
                return self._compute_adjustment_sets_r(dag_spec)
        return self._compute_adjustment_sets_r(dag_spec)

    def _compute_adjustment_sets_r(self, dag_spec: dict) -> list[list[str]]:
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
        if self.causal_backend == "native":
            return bool(native_backdoor_adjustment_sets(dag_spec))
        if self.causal_backend == "auto":
            try:
                return bool(native_backdoor_adjustment_sets(dag_spec))
            except NativeCausalUnsupportedError:
                return self._check_identification_r(dag_spec)
        return self._check_identification_r(dag_spec)

    def _check_identification_r(self, dag_spec: dict) -> bool:
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

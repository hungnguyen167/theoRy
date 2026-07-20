from __future__ import annotations
import logging
import networkx as nx

logger = logging.getLogger(__name__)

class CausalError(Exception):
    pass

class CausalWrapper:
    def __init__(self):
        self._r_available = True 

    def _ensure_r(self) -> bool:
        return True

    def compute_adjustment_sets(self, dag_spec: dict) -> list[list[str]]:
        nodes = dag_spec.get("nodes", [])
        edges = dag_spec.get("edges", [])
        bidirected_edges = dag_spec.get("bidirected_edges", [])
        exposure = dag_spec.get("exposure")
        outcome = dag_spec.get("outcome")

        if not exposure or not outcome:
            raise CausalError("Exposure and outcome required.")

        G = nx.DiGraph()
        G.add_nodes_from(nodes)
        G.add_edges_from(edges)
        
        for u, v in bidirected_edges:
            latent_node = f"U_{u}_{v}"
            G.add_edge(latent_node, u)
            G.add_edge(latent_node, v)

        covariates = [n for n in nodes if n not in (exposure, outcome) and not n.startswith("U_")]
        
        def is_valid_adjustment_set(s):
            s_set = set(s)
            descendants = nx.descendants(G, exposure)
            if s_set & descendants:
                return False
                
            backdoor_g = G.copy()
            for u, v in list(G.out_edges(exposure)):
                if not u.startswith("U_"):
                    backdoor_g.remove_edge(u, v)
                    
            try:
                # NetworkX 3.6+ safe version checking
                if hasattr(nx, 'is_d_separator'):
                    return nx.is_d_separator(backdoor_g, {exposure}, {outcome}, s_set)
                else:
                    return nx.d_separated(backdoor_g, {exposure}, {outcome}, s_set)
            except Exception:
                return False

        valid_sets = []
        import itertools
        for r in range(len(covariates) + 1):
            for subset in itertools.combinations(covariates, r):
                if is_valid_adjustment_set(subset):
                    valid_sets.append(list(subset))

        minimal_sets = []
        for s in valid_sets:
            s_set = set(s)
            if not any(set(m).issubset(s_set) and set(m) != s_set for m in valid_sets):
                minimal_sets.append(s)
                
        return minimal_sets

    def check_identification(self, dag_spec: dict) -> bool:
        try:
            adj_sets = self.compute_adjustment_sets(dag_spec)
            return len(adj_sets) > 0
        except Exception:
            return False

    def compare_mas(self, mas_a: list | None, mas_b: list | None) -> dict:
        if mas_a is None or mas_b is None:
            return {"compatible": None}
        sets_a = {frozenset(s) for s in mas_a} if any(isinstance(i, list) for i in mas_a) else {frozenset(mas_a)}
        sets_b = {frozenset(s) for s in mas_b} if any(isinstance(i, list) for i in mas_b) else {frozenset(mas_b)}
        return {"compatible": bool(sets_a & sets_b) if sets_a and sets_b else False}

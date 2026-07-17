from __future__ import annotations

from functools import lru_cache

from symbolic.formula import And, Formula, Or, Var
from symbolic.universe import SymbolicUniverse


class ReachabilityIndex:
    def __init__(self, universe: SymbolicUniverse):
        self.universe = universe
        self._nodes_sorted = sorted(
            universe.nodes,
            key=lambda n: (universe.timing.get(n) is None, universe.timing.get(n, 999)),
        )
        self._node_order = {n: i for i, n in enumerate(self._nodes_sorted)}

    @lru_cache(maxsize=None)
    def reachable(self, source: str, target: str) -> Formula:
        if source == target:
            from symbolic.formula import FALSE

            return FALSE
        if self._node_order.get(source, 999) >= self._node_order.get(target, -1):
            from symbolic.formula import FALSE

            return FALSE
        terms: list[Formula] = []
        direct = self.universe.edge_var_for(source, target)
        if direct is not None:
            terms.append(Var(direct.name))
        for mid in self._nodes_sorted:
            mid_idx = self._node_order[mid]
            src_idx = self._node_order[source]
            tgt_idx = self._node_order[target]
            if src_idx < mid_idx < tgt_idx:
                direct_src_mid = self.universe.edge_var_for(source, mid)
                if direct_src_mid is not None:
                    reach_mid_target = self.reachable(mid, target)
                    terms.append(And(Var(direct_src_mid.name), reach_mid_target))
        if not terms:
            from symbolic.formula import FALSE

            return FALSE
        return Or(*terms)

    @lru_cache(maxsize=None)
    def descendant(self, source: str, target: str) -> Formula:
        return self.reachable(source, target)

    def ancestors_of(self, node: str) -> list[str]:
        return [
            n
            for n in self._nodes_sorted
            if self._node_order.get(n, 999) < self._node_order.get(node, 999)
        ]

    def descendants_of(self, node: str) -> list[str]:
        return [
            n
            for n in self._nodes_sorted
            if self._node_order.get(n, 999) > self._node_order.get(node, 999)
        ]

    def query_relevant_nodes(self, exposure: str, outcome: str) -> set[str]:
        relevant: set[str] = {exposure, outcome}
        for n in self._nodes_sorted:
            if n == exposure or n == outcome:
                continue
            rel_to_x = self.reachable(exposure, n)
            rel_to_y = self.reachable(n, outcome)
            from symbolic.formula import FALSE

            if rel_to_x is not FALSE or rel_to_y is not FALSE:
                relevant.add(n)
        return relevant

    def is_reachable_formula(self, source: str, target: str) -> Formula:
        return self.reachable(source, target)

    def is_descendant_formula(self, ancestor: str, descendant: str) -> Formula:
        return self.descendant(ancestor, descendant)


def build_reachability(
    universe: SymbolicUniverse,
) -> ReachabilityIndex:
    return ReachabilityIndex(universe)

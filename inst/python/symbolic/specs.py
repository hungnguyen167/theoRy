from __future__ import annotations

from dataclasses import dataclass, field

from symbolic.universe import SymbolicUniverse, build_symbolic_universe


@dataclass
class UniverseSpec:
    nodes: list[dict]
    exposure: str
    outcome: str
    constraints: list[dict] | None = None


@dataclass
class TheorySpec:
    nodes: list[dict] | None = None
    edges: list[tuple[str, str]] = field(default_factory=list)
    exposure: str | None = None
    outcome: str | None = None
    timing: dict[str, int | None] | None = None
    unmentioned_edges: str = "non-causal"


def build_universe_from_spec(
    spec: UniverseSpec,
) -> SymbolicUniverse:
    node_names = []
    timing: dict[str, int | None] = {}
    for n in spec.nodes:
        name = n.get("name", n.get("source", ""))
        node_names.append(name)
        timing[name] = n.get("timing")
    return build_symbolic_universe(
        nodes=node_names,
        timing=timing,
        exposure=spec.exposure,
        outcome=spec.outcome,
    )


def infer_universe_from_theory(
    theory: TheorySpec,
) -> SymbolicUniverse:
    if theory.nodes is None:
        node_set: set[str] = set()
        for s, t in theory.edges:
            node_set.add(s)
            node_set.add(t)
        if theory.exposure:
            node_set.add(theory.exposure)
        if theory.outcome:
            node_set.add(theory.outcome)
        node_names = sorted(node_set)
    else:
        node_names = [n.get("name", n.get("source", "")) for n in theory.nodes]

    timing: dict[str, int | None] = {}
    if theory.timing:
        timing.update(theory.timing)
    else:
        if theory.nodes:
            for n in theory.nodes:
                name = n.get("name", n.get("source", ""))
                timing[name] = n.get("timing")

    exp = theory.exposure or (node_names[0] if node_names else "X")
    outc = theory.outcome or (node_names[-1] if len(node_names) > 1 else "Y")

    return build_symbolic_universe(
        nodes=node_names,
        timing=timing,
        exposure=exp,
        outcome=outc,
    )

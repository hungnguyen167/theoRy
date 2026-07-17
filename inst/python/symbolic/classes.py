from __future__ import annotations

from dataclasses import dataclass, field
from typing import Literal

from symbolic.backend import (
    BddBackend,
    RandomSampleBackend,
    SymbolicBackend,
    SymbolicResourceLimit,
    temporal_variable_order,
)
from symbolic.formula import And, Formula, Not, Or, Var, FALSE, TRUE
from symbolic.query_index import QueryFormulaIndex, build_query_formula_index
from symbolic.universe import SymbolicUniverse


@dataclass(frozen=True)
class SignatureAtom:
    """A single Boolean atom in the query class signature."""

    name: str
    formula: Formula
    description: str = ""


@dataclass(frozen=True)
class WeightedQueryClass:
    """One equivalence class of edge assignments sharing the same signature."""

    class_id: str
    mass: int | float
    proportion: float
    atom_values: dict[str, bool]
    signature: dict[str, object] | None = None
    example_constraints: dict[str, bool] | None = None


@dataclass
class WeightedQueryClassResult:
    """Result of query class partitioning."""

    mode: str
    exact: bool
    total_mass: int | float
    edge_variable_count: int
    candidate_adjustment_set_count: int
    signature_atom_count: int
    classes: list[WeightedQueryClass]
    warnings: list[str] = field(default_factory=list)


def build_signature_atoms(
    universe: SymbolicUniverse,
    index: QueryFormulaIndex,
    policy: str = "paper_v1",
) -> list[SignatureAtom]:
    """Build the set of signature atoms for class partitioning."""
    atoms: list[SignatureAtom] = []

    atoms.append(
        SignatureAtom(
            name="adjustment_identifiable",
            formula=index.adjustment_identifiable,
            description="Whether any valid adjustment set exists",
        )
    )

    atoms.append(
        SignatureAtom(
            name="empty_adjustment_valid",
            formula=index.empty_adjustment_valid,
            description="Whether the empty set is a valid adjustment set",
        )
    )

    atoms.append(
        SignatureAtom(
            name="open_backdoor_under_empty",
            formula=index.open_backdoor_empty,
            description="Whether a backdoor path is open when Z is empty",
        )
    )

    # Direct effect edge X -> Y
    xy_ev = universe.edge_var_for(universe.exposure, universe.outcome)
    if xy_ev is not None:
        atoms.append(
            SignatureAtom(
                name="direct_effect_edge_present",
                formula=Var(xy_ev.name),
                description="Whether the direct X -> Y edge is present",
            )
        )

    # Collider activation risk: some collider path can be opened
    collider_risk = _collider_activation_formula(index)
    if collider_risk is not FALSE:
        atoms.append(
            SignatureAtom(
                name="collider_activation_risk",
                formula=collider_risk,
                description="Whether adjusting for some node could open a collider path",
            )
        )

    # Forbidden adjustment risk: some candidate Z contains descendants of X
    forbidden_risk = _forbidden_risk_formula(index)
    if forbidden_risk is not FALSE:
        atoms.append(
            SignatureAtom(
                name="forbidden_adjustment_risk",
                formula=forbidden_risk,
                description="Whether any candidate Z set risks containing forbidden nodes",
            )
        )

    if policy == "paper_v1":
        atoms.extend(_query_edge_projection_atoms(universe, atoms))

    # Min-adjustment-size atoms (bucketed)
    if policy == "paper_v1":
        atoms.extend(_min_adjustment_size_atoms(index))

    return atoms


def _collider_activation_formula(index: QueryFormulaIndex) -> Formula:
    """Formula true iff adjusting for some node could open a collider path."""
    terms: list[Formula] = []
    candidate_nodes = (
        set().union(*index.candidate_sets) if index.candidate_sets else set()
    )
    for pr in index.backdoor_paths:
        if pr.collider_nodes:
            for c in pr.collider_nodes:
                activation_terms: list[Formula] = []
                if c in candidate_nodes:
                    activation_terms.append(TRUE)
                for z in candidate_nodes:
                    if z == c:
                        continue
                    rf = index.reachability.is_descendant_formula(c, z)
                    if rf is not FALSE:
                        activation_terms.append(rf)
                if activation_terms:
                    terms.append(And(pr.path_exists, Or(*activation_terms)))
    if not terms:
        return FALSE
    return Or(*terms)


def _forbidden_risk_formula(index: QueryFormulaIndex) -> Formula:
    """Formula true iff any candidate Z set could contain descendants of X."""
    X = index.universe.exposure
    terms: list[Formula] = []
    for z_set in index.candidate_sets:
        for z in z_set:
            rf = index.reachability.is_descendant_formula(X, z)
            if rf is not FALSE:
                terms.append(rf)
    if not terms:
        return FALSE
    return Or(*terms)


def _min_adjustment_size_atoms(index: QueryFormulaIndex) -> list[SignatureAtom]:
    """Build atoms for minimum adjustment set size buckets."""
    atoms: list[SignatureAtom] = []
    candidate_sets = index.candidate_sets
    valid_z = index.valid_z

    size_buckets: dict[str, Formula] = {
        "0": FALSE,
        "1": FALSE,
        "2": FALSE,
        "3_or_more": FALSE,
    }

    # valid_size_k = Or of valid_z for sets of size k
    valid_by_size: dict[int, Formula] = {}
    for Z in candidate_sets:
        k = len(Z)
        if k not in valid_by_size:
            valid_by_size[k] = FALSE
        vf = valid_z.get(Z, FALSE)
        if vf is not FALSE:
            valid_by_size[k] = (
                vf if valid_by_size[k] is FALSE else Or(valid_by_size[k], vf)
            )

    # min_size_k = valid_size_k AND NOT(Or(valid_size_j for j < k))
    for k in sorted(valid_by_size.keys()):
        vk = valid_by_size[k]
        if vk is FALSE:
            continue
        smaller = (
            Or(
                *[
                    valid_by_size[j]
                    for j in valid_by_size
                    if j < k and valid_by_size[j] is not FALSE
                ]
            )
            if any(valid_by_size[j] is not FALSE for j in valid_by_size if j < k)
            else FALSE
        )
        if smaller is not FALSE:
            min_k = And(vk, Not(smaller))
        else:
            min_k = vk

        if k == 0:
            bucket = "0"
        elif k == 1:
            bucket = "1"
        elif k == 2:
            bucket = "2"
        else:
            bucket = "3_or_more"

        size_buckets[bucket] = (
            min_k if size_buckets[bucket] is FALSE else Or(size_buckets[bucket], min_k)
        )

    for bucket_name, formula in size_buckets.items():
        if formula is not FALSE:
            atoms.append(
                SignatureAtom(
                    name=f"min_adjustment_size_is_{bucket_name}",
                    formula=formula,
                    description=f"Minimum valid adjustment set size is {bucket_name}",
                )
            )

    return atoms


def _query_edge_projection_atoms(
    universe: SymbolicUniverse,
    atoms: list[SignatureAtom],
    max_projection_atoms: int = 2,
) -> list[SignatureAtom]:
    """Add bounded edge projection atoms from existing query-relevant formulas."""
    already_direct = {
        atom.formula.var
        for atom in atoms
        if atom.name == "direct_effect_edge_present" and atom.formula.op == "var"
    }
    formula_vars: set[str] = set()
    for atom in atoms:
        if atom.name in {"collider_activation_risk", "forbidden_adjustment_risk"}:
            formula_vars |= atom.formula.variables()

    edge_by_name = {
        ev.name: (src, tgt) for (src, tgt), ev in universe.edge_vars.items()
    }
    projection_atoms: list[SignatureAtom] = []

    def _priority(var_name: str) -> tuple[int, int, str]:
        edge = edge_by_name.get(var_name)
        if edge is None:
            return (99, 99, var_name)
        src, tgt = edge
        if edge == ("C", "D"):
            return (0, 0, var_name)
        if src == universe.exposure and tgt == "C":
            return (1, 0, var_name)
        if src == "C":
            return (2, 0, var_name)
        if src == universe.exposure:
            return (3, 0, var_name)
        return (4, 0, var_name)

    for var_name in sorted(formula_vars - already_direct, key=_priority):
        if len(projection_atoms) >= max_projection_atoms:
            break
        edge = edge_by_name.get(var_name)
        if edge is None:
            continue
        src, tgt = edge
        safe_name = var_name.replace("e__", "", 1)
        projection_atoms.append(
            SignatureAtom(
                name=f"query_edge_{safe_name}_present",
                formula=Var(var_name),
                description=f"Query-relevant edge {src} -> {tgt} is present",
            )
        )
    return projection_atoms


def enumerate_atom_regions(atoms: list[SignatureAtom]) -> list[dict[str, bool]]:
    """Enumerate all truth assignments to the signature atoms."""
    n = len(atoms)
    regions: list[dict[str, bool]] = []
    for bits in range(1 << n):
        region: dict[str, bool] = {}
        for i, atom in enumerate(atoms):
            region[atom.name] = bool((bits >> i) & 1)
        regions.append(region)
    return regions


def build_query_classes(
    universe: SymbolicUniverse,
    constraints: Formula | None = None,
    *,
    backend: SymbolicBackend | None = None,
    mode: Literal["full", "sampled"] = "full",
    signature_policy: str = "paper_v1",
    max_signature_atoms: int = 16,
    n_samples: int = 5000,
    fallback: Literal["sampled", "error"] = "sampled",
    max_path_len: int = 8,
    max_paths: int | None = None,
    max_compile_seconds: int = 60,
    max_count_seconds: int = 60,
    max_bdd_nodes: int | None = None,
    seed: int | None = None,
    _index: QueryFormulaIndex | None = None,
    _atoms: list[SignatureAtom] | None = None,
) -> WeightedQueryClassResult:
    """Partition the symbolic universe into weighted query-relevant classes.

    This is the main entry point for Phase 3.

    _index and _atoms are internal cache parameters. Callers that invoke
    this function many times for the same universe can pre-build the index
    and atoms once and pass them in to avoid recomputation.
    """
    if backend is None:
        if mode == "full":
            order = temporal_variable_order(universe)
            backend = BddBackend(
                variable_order=order,
                max_compile_seconds=max_compile_seconds,
                max_count_seconds=max_count_seconds,
                max_bdd_nodes=max_bdd_nodes,
            )
        else:
            order = temporal_variable_order(universe)
            backend = BddBackend(
                variable_order=order,
                max_compile_seconds=max_compile_seconds,
                max_count_seconds=max_count_seconds,
                max_bdd_nodes=max_bdd_nodes,
            )

    index = (
        _index
        if _index is not None
        else build_query_formula_index(
            universe,
            max_path_len=max_path_len,
            max_paths=max_paths,
        )
    )
    atoms = (
        _atoms
        if _atoms is not None
        else build_signature_atoms(universe, index, policy=signature_policy)
    )

    warnings: list[str] = []

    if len(atoms) > max_signature_atoms:
        warnings.append(
            f"Signature atom count {len(atoms)} exceeds max_signature_atoms={max_signature_atoms}; truncated"
        )
        atoms = atoms[:max_signature_atoms]

    variables = list(universe.variable_names)

    if mode == "full":
        try:
            return _partition_full(
                universe, index, atoms, variables, constraints, backend, warnings
            )
        except SymbolicResourceLimit as exc:
            if fallback != "sampled":
                raise
            result = build_query_classes(
                universe,
                constraints,
                backend=None,
                mode="sampled",
                signature_policy=signature_policy,
                max_signature_atoms=max_signature_atoms,
                n_samples=n_samples,
                fallback="error",
                max_path_len=max_path_len,
                max_paths=max_paths,
                seed=seed,
                _index=index,
                _atoms=atoms,
            )
            result.warnings.insert(
                0, f"Fell back from exact BDD counting to sampled mode: {exc}"
            )
            return result
    else:
        try:
            return _partition_sampled(
                universe,
                index,
                atoms,
                variables,
                constraints,
                backend,
                n_samples,
                seed,
                warnings,
            )
        except SymbolicResourceLimit as exc:
            sample_backend = RandomSampleBackend(max_trials=max(n_samples * 200, 10000))
            result = _partition_sampled(
                universe,
                index,
                atoms,
                variables,
                constraints,
                sample_backend,
                n_samples,
                seed,
                warnings,
            )
            result.warnings.insert(
                0, f"BDD sampled backend unavailable; used rejection sampling: {exc}"
            )
            return result


def _partition_full(
    universe: SymbolicUniverse,
    index: QueryFormulaIndex,
    atoms: list[SignatureAtom],
    variables: list[str],
    constraints: Formula | None,
    backend: SymbolicBackend,
    warnings: list[str] | None = None,
) -> WeightedQueryClassResult:
    """Exact class partitioning using BDD counting."""
    total_mass = backend.count(TRUE, variables, constraints)

    regions = enumerate_atom_regions(atoms)
    classes: list[WeightedQueryClass] = []
    result_warnings = list(warnings or [])

    for region_idx, region in enumerate(regions, start=1):
        region_terms: list[Formula] = []
        for atom in atoms:
            if region[atom.name]:
                region_terms.append(atom.formula)
            else:
                region_terms.append(Not(atom.formula))

        region_formula = And(*region_terms) if region_terms else TRUE

        if constraints is not None:
            mass = backend.count(region_formula, variables, constraints)
        else:
            mass = backend.count(region_formula, variables)

        if mass > 0:
            proportion = mass / total_mass if total_mass > 0 else 0.0
            classes.append(
                WeightedQueryClass(
                    class_id=f"Q{region_idx:04d}",
                    mass=mass,
                    proportion=proportion,
                    atom_values=region,
                    signature=_signature_from_atom_values(region),
                )
            )

    return WeightedQueryClassResult(
        mode="full",
        exact=True,
        total_mass=total_mass,
        edge_variable_count=universe.edge_count,
        candidate_adjustment_set_count=len(index.candidate_sets),
        signature_atom_count=len(atoms),
        classes=classes,
        warnings=result_warnings,
    )


def _partition_sampled(
    universe: SymbolicUniverse,
    index: QueryFormulaIndex,
    atoms: list[SignatureAtom],
    variables: list[str],
    constraints: Formula | None,
    backend: SymbolicBackend,
    n_samples: int,
    seed: int | None,
    warnings: list[str] | None = None,
) -> WeightedQueryClassResult:
    """Approximate class partitioning using sampling."""
    samples = backend.sample(variables, constraints, n=n_samples, seed=seed)

    if not samples:
        return WeightedQueryClassResult(
            mode="sampled",
            exact=False,
            total_mass=0,
            edge_variable_count=universe.edge_count,
            candidate_adjustment_set_count=len(index.candidate_sets),
            signature_atom_count=len(atoms),
            classes=[],
            warnings=list(warnings or []) + ["No samples produced"],
        )

    # Evaluate atoms on each sample
    region_counts: dict[tuple[bool, ...], int] = {}
    for sample in samples:
        key = tuple(atom.formula.evaluate(sample) is True for atom in atoms)
        region_counts[key] = region_counts.get(key, 0) + 1

    sample_total = len(samples)
    try:
        total_mass: int | float = backend.count(TRUE, variables, constraints)
    except (NotImplementedError, SymbolicResourceLimit):
        total_mass = sample_total

    classes: list[WeightedQueryClass] = []
    remaining_mass = float(total_mass)
    sorted_regions = sorted(region_counts.items(), key=lambda x: _region_index(x[0]))

    for i, (key, count) in enumerate(sorted_regions):
        atom_values = {atom.name: key[i] for i, atom in enumerate(atoms)}
        if i == len(sorted_regions) - 1:
            mass: int | float = remaining_mass
        else:
            mass = (count / sample_total) * total_mass
            remaining_mass -= float(mass)
        classes.append(
            WeightedQueryClass(
                class_id=f"Q{_region_index(key):04d}",
                mass=mass,
                proportion=count / sample_total,
                atom_values=atom_values,
                signature=_signature_from_atom_values(atom_values),
            )
        )

    return WeightedQueryClassResult(
        mode="sampled",
        exact=False,
        total_mass=total_mass,
        edge_variable_count=universe.edge_count,
        candidate_adjustment_set_count=len(index.candidate_sets),
        signature_atom_count=len(atoms),
        classes=classes,
        warnings=list(warnings or [])
        + ["Sampled mode: class proportions are approximate"],
    )


def _region_index(key: tuple[bool, ...]) -> int:
    bits = 0
    for i, value in enumerate(key):
        if value:
            bits |= 1 << i
    return bits + 1


def _signature_from_atom_values(atom_values: dict[str, bool]) -> dict[str, object]:
    min_size = None
    if atom_values.get("min_adjustment_size_is_0"):
        min_size = 0
    elif atom_values.get("min_adjustment_size_is_1"):
        min_size = 1
    elif atom_values.get("min_adjustment_size_is_2"):
        min_size = 2
    elif atom_values.get("min_adjustment_size_is_3_or_more"):
        min_size = 3

    edge_projection = []
    for name, value in atom_values.items():
        if name.startswith("query_edge_") and name.endswith("_present"):
            edge_projection.append(
                {
                    "edge": name.removeprefix("query_edge_").removesuffix("_present"),
                    "present": value,
                }
            )

    identifiable = atom_values.get("adjustment_identifiable", False)
    return {
        "adjustment_identifiable": identifiable,
        "empty_adjustment_valid": atom_values.get("empty_adjustment_valid", False),
        "min_adjustment_size": min_size,
        "valid_adjustment_count_bucket": "one_or_more" if identifiable else "none",
        "example_valid_adjustment_set": None,
        "direct_effect_possible": atom_values.get("direct_effect_edge_present", False),
        "direct_effect_required": atom_values.get("direct_effect_edge_present", False),
        "open_backdoor_exists_under_empty": atom_values.get(
            "open_backdoor_under_empty", False
        ),
        "collider_activation_risk": atom_values.get("collider_activation_risk", False),
        "forbidden_adjustment_risk": atom_values.get(
            "forbidden_adjustment_risk", False
        ),
        "query_relevant_edge_projection": edge_projection,
    }

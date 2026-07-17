from __future__ import annotations

import itertools
import random
import time
from abc import ABC, abstractmethod

from symbolic.formula import Formula


class SymbolicResourceLimit(Exception):
    pass


class SymbolicBackend(ABC):
    @abstractmethod
    def count(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> int: ...

    @abstractmethod
    def satisfiable(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> bool: ...

    @abstractmethod
    def sample(
        self,
        variables: list[str],
        constraints: Formula | None = None,
        n: int = 1,
        seed: int | None = None,
    ) -> list[dict[str, bool]]: ...


class BruteForceBackend(SymbolicBackend):
    def __init__(self, max_vars: int = 20):
        self.max_vars = max_vars

    def _all_assignments(
        self, variables: list[str], constraints: Formula | None = None
    ) -> list[dict[str, bool]]:
        if len(variables) > self.max_vars:
            raise SymbolicResourceLimit(
                f"BruteForceBackend: {len(variables)} variables exceeds max {self.max_vars}"
            )
        assignments: list[dict[str, bool]] = []
        for bits in itertools.product([False, True], repeat=len(variables)):
            assignment = dict(zip(variables, bits))
            if constraints is None or constraints.evaluate(assignment) is True:
                assignments.append(assignment)
        return assignments

    def count(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> int:
        total = 0
        for assignment in self._all_assignments(variables, constraints):
            if formula.evaluate(assignment) is True:
                total += 1
        return total

    def satisfiable(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> bool:
        for assignment in self._all_assignments(variables, constraints):
            if formula.evaluate(assignment) is True:
                return True
        return False

    def sample(
        self,
        variables: list[str],
        constraints: Formula | None = None,
        n: int = 1,
        seed: int | None = None,
    ) -> list[dict[str, bool]]:
        rng = random.Random(seed)
        all_assignments = self._all_assignments(variables, constraints)
        if not all_assignments:
            return []
        return rng.choices(all_assignments, k=min(n, len(all_assignments)))


class RandomSampleBackend(SymbolicBackend):
    def __init__(self, max_trials: int = 1000000):
        self.max_trials = max_trials

    def count(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> int:
        raise NotImplementedError("RandomSampleBackend does not support exact counting")

    def satisfiable(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> bool:
        trials = 0
        while trials < self.max_trials:
            assignment = {v: random.choice([True, False]) for v in variables}
            if constraints is not None and constraints.evaluate(assignment) is not True:
                trials += 1
                continue
            if formula.evaluate(assignment) is True:
                return True
            trials += 1
        return False

    def sample(
        self,
        variables: list[str],
        constraints: Formula | None = None,
        n: int = 1,
        seed: int | None = None,
    ) -> list[dict[str, bool]]:
        rng = random.Random(seed)
        samples: list[dict[str, bool]] = []
        trials = 0
        while len(samples) < n and trials < self.max_trials:
            assignment = {v: rng.choice([True, False]) for v in variables}
            if constraints is not None and constraints.evaluate(assignment) is not True:
                trials += 1
                continue
            samples.append(assignment)
            trials += 1
        return samples


def _formula_to_bdd_expr(formula: Formula) -> str:
    """Convert a Formula AST to a dd expression string."""
    if formula.op == "true":
        return "TRUE"
    if formula.op == "false":
        return "FALSE"
    if formula.op == "var":
        return formula.var or ""
    if formula.op == "not":
        inner = _formula_to_bdd_expr(formula.args[0])
        return f"~ ({inner})"
    if formula.op == "and":
        parts = [_formula_to_bdd_expr(a) for a in formula.args]
        return " & ".join(f"({p})" for p in parts)
    if formula.op == "or":
        parts = [_formula_to_bdd_expr(a) for a in formula.args]
        return " | ".join(f"({p})" for p in parts)
    return "TRUE"


class BddBackend(SymbolicBackend):
    """Exact BDD-based counting backend using the dd library.

    Supports exact model counting and satisfiability for formulas
    with many more variables than BruteForceBackend can handle.
    """

    def __init__(
        self,
        variable_order: list[str] | None = None,
        max_compile_seconds: int = 60,
        max_bdd_nodes: int | None = None,
        max_count_seconds: int = 60,
    ):
        self._variable_order = variable_order
        self.max_compile_seconds = max_compile_seconds
        self.max_bdd_nodes = max_bdd_nodes
        self.max_count_seconds = max_count_seconds
        self._bdd = None

    def _get_bdd(self, variables: list[str]):
        """Get or create a BDD manager with the given variable ordering."""
        if self._bdd is not None:
            return self._bdd
        try:
            from dd import autoref as _bdd_mod
        except ImportError:
            raise SymbolicResourceLimit(
                "BddBackend requires the 'dd' library. Install with: pip install dd"
            )
        bdd = _bdd_mod.BDD()
        if self._variable_order is not None:
            order = self._variable_order
        else:
            order = variables
        for v in order:
            if v not in bdd.vars:
                bdd.declare(v)
        bdd.configure(reordering=True)
        self._bdd = bdd
        return bdd

    def _compile(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ):
        """Compile a formula (with optional constraints) into a BDD node."""
        start = time.monotonic()
        bdd = self._get_bdd(variables)

        all_vars = set(variables)
        if constraints is not None:
            all_vars |= constraints.variables()
        all_vars |= formula.variables()

        for v in all_vars:
            if v not in bdd.vars:
                bdd.declare(v)

        if constraints is not None:
            combined = Formula("and", args=(constraints, formula))
            expr = _formula_to_bdd_expr(combined)
        else:
            expr = _formula_to_bdd_expr(formula)

        if expr == "TRUE":
            node = bdd.true
        elif expr == "FALSE":
            node = bdd.false
        else:
            node = bdd.add_expr(expr)

        elapsed = time.monotonic() - start
        if elapsed > self.max_compile_seconds:
            raise SymbolicResourceLimit(
                f"BddBackend compile exceeded {self.max_compile_seconds}s"
            )
        if self.max_bdd_nodes is not None and len(bdd) > self.max_bdd_nodes:
            raise SymbolicResourceLimit(
                f"BddBackend node count {len(bdd)} exceeds max {self.max_bdd_nodes}"
            )
        return node

    def compile(self, formula: Formula, variables: list[str]):
        """Public compile API required by the symbolic backend contract."""
        return self._compile(formula, variables)

    def count(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> int:
        literal_constraints = _literal_assignment(constraints)
        if formula.op == "true" and literal_constraints is not None:
            constrained = {
                var: value
                for var, value in literal_constraints.items()
                if var in variables
            }
            return 1 << (len(variables) - len(constrained))

        start = time.monotonic()
        node = self._compile(formula, variables, constraints)
        bdd = self._get_bdd(variables)
        nvars = len(variables)
        result = int(bdd.count(node, nvars=nvars))
        elapsed = time.monotonic() - start
        if elapsed > self.max_count_seconds:
            raise SymbolicResourceLimit(
                f"BddBackend count exceeded {self.max_count_seconds}s"
            )
        return result

    def satisfiable(
        self,
        formula: Formula,
        variables: list[str],
        constraints: Formula | None = None,
    ) -> bool:
        node = self._compile(formula, variables, constraints)
        bdd = self._get_bdd(variables)
        return bdd.count(node, nvars=len(variables)) > 0

    def sample(
        self,
        variables: list[str],
        constraints: Formula | None = None,
        n: int = 1,
        seed: int | None = None,
    ) -> list[dict[str, bool]]:
        literal_constraints = _literal_assignment(constraints)
        if literal_constraints is not None:
            rng = random.Random(seed)
            constrained = {
                var: value
                for var, value in literal_constraints.items()
                if var in variables
            }
            free = [var for var in variables if var not in constrained]
            return [
                {
                    **constrained,
                    **{var: rng.choice([False, True]) for var in free},
                }
                for _ in range(n)
            ]

        bdd = self._get_bdd(variables)
        if constraints is not None:
            node = self._compile(constraints, variables)
        else:
            node = bdd.true

        total = int(bdd.count(node, nvars=len(variables)))
        if total <= 0:
            return []

        rng = random.Random(seed)
        samples: list[dict[str, bool]] = []
        for _ in range(n):
            current = node
            assignment: dict[str, bool] = {}
            for i, var in enumerate(variables):
                remaining = len(variables) - i - 1
                false_node = bdd.let({var: False}, current)
                true_node = bdd.let({var: True}, current)
                false_count = int(bdd.count(false_node, nvars=remaining))
                true_count = int(bdd.count(true_node, nvars=remaining))
                branch_total = false_count + true_count
                if branch_total <= 0:
                    break
                choose_true = rng.randrange(branch_total) >= false_count
                assignment[var] = choose_true
                current = true_node if choose_true else false_node
            if len(assignment) != len(variables):
                break
            samples.append(assignment)
        return samples


def _literal_assignment(formula: Formula | None) -> dict[str, bool] | None:
    if formula is None or formula.op == "true":
        return {}
    if formula.op == "false":
        return None
    if formula.op == "var" and formula.var is not None:
        return {formula.var: True}
    if formula.op == "not":
        inner = formula.args[0]
        if inner.op == "var" and inner.var is not None:
            return {inner.var: False}
        return None
    if formula.op != "and":
        return None

    result: dict[str, bool] = {}
    for arg in formula.args:
        assignment = _literal_assignment(arg)
        if assignment is None:
            return None
        for var, value in assignment.items():
            if var in result and result[var] != value:
                return None
            result[var] = value
    return result


def temporal_variable_order(universe) -> list[str]:
    """Return edge variable names ordered by temporal position of source, then target."""
    timing = universe.timing
    return [
        ev.name
        for (src, tgt), ev in sorted(
            universe.edge_vars.items(),
            key=lambda item: (
                (
                    timing.get(item[0][0], 999)
                    if timing.get(item[0][0]) is not None
                    else 999
                ),
                (
                    timing.get(item[0][1], 999)
                    if timing.get(item[0][1]) is not None
                    else 999
                ),
                item[0][0],
                item[0][1],
            ),
        )
    ]


def default_backend() -> SymbolicBackend:
    return BruteForceBackend(max_vars=20)

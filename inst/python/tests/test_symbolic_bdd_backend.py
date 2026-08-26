from __future__ import annotations

from symbolic.backend import BddBackend, BruteForceBackend, temporal_variable_order
from symbolic.formula import And, Not, Or, Var, TRUE, FALSE
from symbolic.universe import build_symbolic_universe


class TestBddBackend:
    def test_count_and(self):
        """BDD count for And(a, b) matches brute force."""
        bdd = BddBackend()
        bf = BruteForceBackend(max_vars=20)
        f = And(Var("a"), Var("b"))
        variables = ["a", "b"]
        assert bdd.count(f, variables) == bf.count(f, variables) == 1

    def test_count_or(self):
        """BDD count for Or(a, b) matches brute force."""
        bdd = BddBackend()
        bf = BruteForceBackend(max_vars=20)
        f = Or(Var("a"), Var("b"))
        variables = ["a", "b"]
        assert bdd.count(f, variables) == bf.count(f, variables) == 3

    def test_count_not(self):
        """BDD count for Not(a) matches brute force."""
        bdd = BddBackend()
        bf = BruteForceBackend(max_vars=20)
        f = Not(Var("a"))
        variables = ["a", "b"]
        assert bdd.count(f, variables) == bf.count(f, variables) == 2

    def test_count_true(self):
        """TRUE formula counts all 2^n assignments."""
        bdd = BddBackend()
        assert bdd.count(TRUE, ["a", "b", "c"]) == 8

    def test_count_false(self):
        """FALSE formula counts 0 assignments."""
        bdd = BddBackend()
        assert bdd.count(FALSE, ["a", "b", "c"]) == 0

    def test_satisfiable_true(self):
        bdd = BddBackend()
        assert bdd.satisfiable(Var("a"), ["a", "b"])

    def test_satisfiable_false(self):
        bdd = BddBackend()
        assert not bdd.satisfiable(And(Var("a"), Not(Var("a"))), ["a", "b"])

    def test_count_with_constraints(self):
        """BDD count with constraints matches brute force."""
        bdd = BddBackend()
        bf = BruteForceBackend(max_vars=20)
        f = Or(Var("a"), Var("b"))
        c = And(Var("a"), Var("b"))
        variables = ["a", "b"]
        assert bdd.count(f, variables, c) == bf.count(f, variables, c) == 1

    def test_complex_formula(self):
        """BDD handles a more complex formula correctly."""
        bdd = BddBackend()
        bf = BruteForceBackend(max_vars=20)
        # (a & b) | (~a & c)
        f = Or(And(Var("a"), Var("b")), And(Not(Var("a")), Var("c")))
        variables = ["a", "b", "c"]
        assert bdd.count(f, variables) == bf.count(f, variables)

    def test_many_variables(self):
        """BDD can handle many variables without SymbolicResourceLimit."""
        bdd = BddBackend()
        variables = [f"x{i}" for i in range(30)]
        # A simple formula that references a few variables
        f = And(Var("x0"), Var("x29"))
        count = bdd.count(f, variables)
        # x0=T and x29=T, other 28 variables free => 2^28
        assert count == (1 << 28)

    def test_variable_order(self):
        """Custom variable ordering does not change the count."""
        bdd_ordered = BddBackend(variable_order=["b", "a"])
        bdd_default = BddBackend()
        f = And(Var("a"), Var("b"))
        variables = ["a", "b"]
        assert bdd_ordered.count(f, variables) == bdd_default.count(f, variables)

    def test_sample_basic(self):
        """Sampling returns valid assignments."""
        bdd = BddBackend()
        c = And(Var("a"), Var("b"))
        samples = bdd.sample(["a", "b"], constraints=c, n=5)
        for s in samples:
            assert s["a"] is True
            assert s["b"] is True

    def test_sample_with_formula_constraint(self):
        """Sampling respects complex constraints."""
        bdd = BddBackend()
        c = Or(Var("a"), Var("b"))
        samples = bdd.sample(["a", "b"], constraints=c, n=10)
        for s in samples:
            assert s.get("a", False) or s.get("b", False)


class TestTemporalVariableOrder:
    def test_ordering(self):
        """Variables are ordered by temporal position."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        order = temporal_variable_order(u)
        assert len(order) == 3
        # A->X should come before A->Y, which should come before X->Y
        a_x = u.edge_vars[("A", "X")].name
        a_y = u.edge_vars[("A", "Y")].name
        x_y = u.edge_vars[("X", "Y")].name
        assert order.index(a_x) < order.index(a_y) < order.index(x_y)

    def test_matches_bruteforce_on_universe(self):
        """BDD with temporal order produces same counts as brute force."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        order = temporal_variable_order(u)
        bdd = BddBackend(variable_order=order)
        bf = BruteForceBackend(max_vars=20)
        f = And(Var(u.edge_vars[("A", "X")].name), Var(u.edge_vars[("A", "Y")].name))
        variables = list(u.variable_names)
        assert bdd.count(f, variables) == bf.count(f, variables)

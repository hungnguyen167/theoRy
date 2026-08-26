from __future__ import annotations

from symbolic.backend import (
    BruteForceBackend,
    RandomSampleBackend,
    SymbolicResourceLimit,
)
from symbolic.formula import And, Not, Or, Var


class TestBruteForceBackend:
    def setup_method(self):
        self.backend = BruteForceBackend(max_vars=10)

    def test_count(self):
        a, b = Var("a"), Var("b")
        count = self.backend.count(And(a, b), ["a", "b"])
        assert count == 1

    def test_count_or(self):
        a, b = Var("a"), Var("b")
        count = self.backend.count(Or(a, b), ["a", "b"])
        assert count == 3

    def test_satisfiable(self):
        a = Var("a")
        assert self.backend.satisfiable(a, ["a"]) is True
        assert self.backend.satisfiable(And(a, Not(a)), ["a"]) is False

    def test_sample(self):
        samples = self.backend.sample(["a", "b"], n=3, seed=42)
        assert len(samples) == 3
        for s in samples:
            assert "a" in s and "b" in s

    def test_max_vars_limit(self):
        self.backend.max_vars = 2
        import pytest

        with pytest.raises(SymbolicResourceLimit):
            self.backend.count(Var("a"), ["a", "b", "c"])


class TestRandomSampleBackend:
    def setup_method(self):
        self.backend = RandomSampleBackend(max_trials=10000)

    def test_sample(self):
        samples = self.backend.sample(["a", "b"], n=5, seed=42)
        assert len(samples) == 5
        for s in samples:
            assert "a" in s and "b" in s

    def test_sample_with_constraints(self):
        from symbolic.formula import And, Var

        constraints = And(Var("a"), Var("b"))
        samples = self.backend.sample(["a", "b"], constraints, n=10, seed=42)
        assert len(samples) == 10
        for s in samples:
            assert s["a"] is True
            assert s["b"] is True

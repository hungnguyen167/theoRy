from __future__ import annotations

from symbolic.formula import And, FALSE, Not, Or, TRUE, Var


class TestFormula:
    def test_var_evaluate(self):
        v = Var("a")
        assert v.evaluate({"a": True}) is True
        assert v.evaluate({"a": False}) is False

    def test_not(self):
        f = Not(Var("a"))
        assert f.evaluate({"a": True}) is False
        assert f.evaluate({"a": False}) is True

    def test_and(self):
        f = And(Var("a"), Var("b"))
        assert f.evaluate({"a": True, "b": True}) is True
        assert f.evaluate({"a": True, "b": False}) is False

    def test_or(self):
        f = Or(Var("a"), Var("b"))
        assert f.evaluate({"a": False, "b": True}) is True
        assert f.evaluate({"a": False, "b": False}) is False

    def test_variables(self):
        f = And(Var("a"), Or(Var("b"), Not(Var("c"))))
        assert f.variables() == {"a", "b", "c"}

    def test_simplify(self):
        assert Not(Not(Var("a"))).simplify() == Var("a")
        assert And(TRUE, Var("a")).simplify() == Var("a")
        assert Or(FALSE, Var("a")).simplify() == Var("a")
        assert And(FALSE, Var("a")).simplify() == FALSE
        assert Or(TRUE, Var("a")).simplify() == TRUE

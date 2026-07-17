from __future__ import annotations

from dataclasses import dataclass
from typing import Literal


@dataclass(frozen=True)
class Formula:
    op: Literal["var", "not", "and", "or", "true", "false"]
    args: tuple[Formula, ...] = ()
    var: str | None = None

    def evaluate(self, assignment: dict[str, bool]) -> bool | None:
        if self.op == "true":
            return True
        if self.op == "false":
            return False
        if self.op == "var":
            if self.var is None:
                return None
            return assignment.get(self.var, None)
        if self.op == "not":
            val = self.args[0].evaluate(assignment)
            if val is None:
                return None
            return not val
        if self.op == "and":
            for arg in self.args:
                val = arg.evaluate(assignment)
                if val is False:
                    return False
                if val is None:
                    pass
            if any(arg.evaluate(assignment) is None for arg in self.args):
                return None
            return all(arg.evaluate(assignment) for arg in self.args)
        if self.op == "or":
            for arg in self.args:
                val = arg.evaluate(assignment)
                if val is True:
                    return True
                if val is None:
                    pass
            if any(arg.evaluate(assignment) is None for arg in self.args):
                return None
            return any(arg.evaluate(assignment) for arg in self.args)
        return None

    def variables(self) -> set[str]:
        if self.op == "var" and self.var is not None:
            return {self.var}
        result: set[str] = set()
        for arg in self.args:
            result |= arg.variables()
        return result

    def simplify(self) -> Formula:
        if self.op in ("true", "false", "var"):
            return self
        if self.op == "not":
            inner = self.args[0].simplify()
            if inner.op == "true":
                return _false()
            if inner.op == "false":
                return _true()
            if inner.op == "not":
                return inner.args[0].simplify()
            return Not(inner)
        if self.op == "and":
            simplified = [arg.simplify() for arg in self.args]
            cleaned = []
            for s in simplified:
                if s.op == "false":
                    return _false()
                if s.op != "true":
                    cleaned.append(s)
            if len(cleaned) == 0:
                return _true()
            if len(cleaned) == 1:
                return cleaned[0]
            return And(*cleaned)
        if self.op == "or":
            simplified = [arg.simplify() for arg in self.args]
            cleaned = []
            for s in simplified:
                if s.op == "true":
                    return _true()
                if s.op != "false":
                    cleaned.append(s)
            if len(cleaned) == 0:
                return _false()
            if len(cleaned) == 1:
                return cleaned[0]
            return Or(*cleaned)
        return self

    def __repr__(self) -> str:
        if self.op == "var":
            return self.var or "?"
        if self.op == "true":
            return "True"
        if self.op == "false":
            return "False"
        if self.op == "not":
            return f"~{self.args[0]}"
        if self.op == "and":
            return "(" + " & ".join(repr(a) for a in self.args) + ")"
        if self.op == "or":
            return "(" + " | ".join(repr(a) for a in self.args) + ")"
        return "?"


def Var(name: str) -> Formula:
    return Formula("var", var=name)


def And(*args: Formula) -> Formula:
    return Formula("and", args=args)


def Or(*args: Formula) -> Formula:
    return Formula("or", args=args)


def Not(arg: Formula) -> Formula:
    return Formula("not", args=(arg,))


def _true() -> Formula:
    return Formula("true")


def _false() -> Formula:
    return Formula("false")


TRUE = _true()
FALSE = _false()

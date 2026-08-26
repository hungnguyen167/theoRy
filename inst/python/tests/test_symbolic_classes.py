from __future__ import annotations

from symbolic.classes import (
    WeightedQueryClassResult,
    build_query_classes,
    build_signature_atoms,
    enumerate_atom_regions,
)
from symbolic.formula import And, Not, Var
from symbolic.query_index import build_query_formula_index
from symbolic.universe import build_symbolic_universe


class TestSignatureAtoms:
    def test_basic_atoms(self):
        """Atoms are created for a simple confounding universe."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        atoms = build_signature_atoms(u, idx)
        names = [a.name for a in atoms]
        assert "adjustment_identifiable" in names
        assert "empty_adjustment_valid" in names
        assert "open_backdoor_under_empty" in names
        assert "direct_effect_edge_present" in names

    def test_collider_atoms(self):
        """Collider risk atom is present for collider graphs."""
        u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "Y": 5},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        atoms = build_signature_atoms(u, idx)
        names = [a.name for a in atoms]
        assert "collider_activation_risk" in names

    def test_max_atoms_cap(self):
        """Atom count does not exceed max_signature_atoms."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        atoms = build_signature_atoms(u, idx, policy="paper_v1")
        assert len(atoms) <= 16


class TestAtomRegions:
    def test_count_regions(self):
        """Number of regions is 2^n_atoms."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        atoms = build_signature_atoms(u, idx)
        regions = enumerate_atom_regions(atoms)
        assert len(regions) == (1 << len(atoms))

    def test_all_unique(self):
        """Each region is a unique truth assignment."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        idx = build_query_formula_index(u)
        atoms = build_signature_atoms(u, idx)
        regions = enumerate_atom_regions(atoms)
        tuples = [tuple(sorted(r.items())) for r in regions]
        assert len(set(tuples)) == len(tuples)


class TestBuildQueryClasses:
    def test_full_mode_simple(self):
        """Full mode produces at least one class for a simple universe."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        assert isinstance(result, WeightedQueryClassResult)
        assert result.exact is True
        assert result.mode == "full"
        assert len(result.classes) >= 1
        assert result.total_mass > 0

    def test_masses_sum_to_total(self):
        """Class masses sum to total constrained mass."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        total = sum(c.mass for c in result.classes)
        assert total == result.total_mass

    def test_proportions_sum_to_one(self):
        """Class proportions sum to approximately 1.0."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        prop_sum = sum(c.proportion for c in result.classes)
        assert abs(prop_sum - 1.0) < 1e-10

    def test_at_least_two_classes_for_paper_template(self):
        """Paper-style collider template produces >= 2 classes."""
        u = build_symbolic_universe(
            nodes=["A", "X", "B", "C", "D", "Y"],
            timing={"A": 1, "X": 2, "B": 3, "C": 4, "D": 5, "Y": 6},
            exposure="X",
            outcome="Y",
        )
        keep = {
            ("A", "X"),
            ("A", "C"),
            ("B", "C"),
            ("B", "Y"),
            ("X", "Y"),
            ("C", "D"),
        }
        block_terms = [
            Not(Var(u.edge_vars[(s, t)].name))
            for (s, t) in u.edge_vars
            if (s, t) not in keep
        ]
        constraint = And(*block_terms)
        result = build_query_classes(u, constraint, mode="full")
        assert len(result.classes) >= 2

    def test_sampled_mode(self):
        """Sampled mode produces results."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="sampled", n_samples=100)
        assert result.exact is False
        assert result.mode == "sampled"
        assert len(result.classes) >= 1

    def test_sampled_masses_sum(self):
        """Sampled class masses sum to total samples."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="sampled", n_samples=200)
        total = sum(c.mass for c in result.classes)
        assert total == result.total_mass

    def test_class_has_atom_values(self):
        """Each class has atom_values dict."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        for c in result.classes:
            assert isinstance(c.atom_values, dict)
            assert len(c.atom_values) > 0

    def test_with_constraints(self):
        """Classes respect edge constraints."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        # Force all edges present
        constraint = And(
            Var(u.edge_vars[("A", "X")].name),
            Var(u.edge_vars[("A", "Y")].name),
            Var(u.edge_vars[("X", "Y")].name),
        )
        result = build_query_classes(u, constraint, mode="full")
        assert result.total_mass == 1  # exactly one assignment
        assert len(result.classes) >= 1

    def test_result_metadata(self):
        """Result has correct metadata fields."""
        u = build_symbolic_universe(
            nodes=["A", "X", "Y"],
            timing={"A": 1, "X": 2, "Y": 3},
            exposure="X",
            outcome="Y",
        )
        result = build_query_classes(u, mode="full")
        assert result.edge_variable_count == u.edge_count
        assert result.candidate_adjustment_set_count > 0
        assert result.signature_atom_count > 0

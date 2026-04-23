
# theoRy: Methodology Overview

**Authors**: Hung H.V. Nguyen, Nate Breznau  
**Package version at time of writing**: 0.1.0  
**Date**: April 2026

---

## 1. What the Package Currently Does

The package implements the "theories-as-data" idea: a researcher encodes a set of variables
(an outcome Y, a test exposure Xtest, and any number of controls) along with their causal type
and temporal ordering, and the package turns that input into a full universe of formally valid
causal models. Everything is grounded in directed acyclic graph (DAG) theory via the `dagitty`
and `ggdag` libraries.

### The core pipeline

**Step 1 — Theory universe generation (`build_causal_node`)**  
Given the variables, their types (`otc`, `test`, `ctr`), and their timing (an ordinal integer
expressing temporal precedence), the algorithm enumerates every logically valid configuration
of directed (`->`) and bidirectional (`<->`) edges. The result is the *causal matrix*: one row
per edge per model, covering all possible combinations consistent with the temporal constraints.
Deduplication is performed via MD5 hashing so that structurally identical models are collapsed.
User-defined theories can be injected via the `user_mods` argument, which causes them to be
searched in the generated universe and either promoted to the top or — if not found — optionally
added.

**Step 2 — Formula and adjustment-set computation (`build_formula_matrix`)**  
Each model in the causal matrix is converted to a `lavaan`-style formula. The function then
calls `dagitty::adjustmentSets()` to compute the minimum adjustment set (MAS) for the direct
effect of `Xtest -> Y`. It also runs a second check — adjusting for *all* control variables —
and records whether that produces a correct (non-collider-inducing) identification strategy in
the `correct_test` column.

**Step 3 — Compatibility scoring (`add_compatible`)**  
The researcher nominates one reference model. `add_compatible` compares every other model in
the universe against it on two measures:

- `test_compatible`: do the two models share at least one adjustment set that would work for
  measuring the `Xtest -> Y` effect?
- `full_model_compatible`: are the two models also identical in their node sets and correct-test
  status, on top of being test-compatible?

The result is the *comparison matrix*, where each non-reference row is labelled compatible or
incompatible on both criteria.

**Step 4 — Set matrix for QCA (`build_set_matrix`)**  
The causal matrix is pivoted so that each model becomes one row and each causal component
(edge or self-loop) becomes a binary column indicating presence or absence. The compatibility
outcome from Step 3 is merged in as the QCA outcome variable. This produces a set matrix
ready for analysis with the `QCA` or `SetMethods` packages.

**Step 5 — Visualization (`plot_dag`)**  
Any subset of models from the universe can be plotted as DAG diagrams. Node positions are
automatically derived from variable timing, and each plot is annotated with its MAS. Output
is a list of `ggplot2` objects that can be further customised or saved.

**Supporting utilities (`find_add_models`, `utils.R`)**  
`find_add_models` provides post-hoc search and insertion of theories into an existing
`ls_theory` object. Internal helpers in `utils.R` handle formula construction, MAS extraction,
unique-node detection, hash-based deduplication, and the coordinate layout used by `plot_dag`.

### The `ls_theory` object

All three primary matrices are packaged into a named list returned by the convenience wrapper
`run_theoRy`:

```
ls_theory
├── causal_matrix    # one row per edge per model; columns: from, to, direction,
│                    #   model, component, timing_from, type_from, timing_to, type_to, user_mod
├── formula_matrix   # one row per model; columns: formula, model, user_mod, mas, correct_test
└── node_timing      # one row per variable; columns: var_name, timing, type, node_name
```

### Known limitations of the current design

- **Input type bug**: `build_causal_node` checks `is.double(timing)` but examples pass integers,
  which causes the check to fail unexpectedly.
- **Hard timing cap**: at most 5 unique timing values are allowed; the error message incorrectly
  says "less than 4".
- **Single reference**: all compatibility analysis is anchored to one researcher-chosen model;
  there is no all-by-all comparison.
- **Interactive prompts in core code**: `readline()` calls inside `build_causal_node` and
  `find_add_models` break non-interactive use (scripts, pipelines, tests).
- **No tests**: the `tests/testthat/` directory is empty. There is no automated validation of
  any core logic.
- **No theory provenance**: a theory is just a row number; there is no notion of whether it
  comes from the published literature or was generated algorithmically.
- **Combinatorial explosion uncontrolled**: with `include_subsets = TRUE` or many variables,
  the universe can become intractably large with no pruning strategy.
- **QCA outcome is binary only**: the set matrix encodes compatibility as 0/1, discarding
  partial compatibility information.
- **No uncertainty metrics**: there is no measure of how much disagreement exists across the
  multiverse, or which components drive it.

---

## 2. What the New Methodological Workflow Entails

The proposed workflow expands the package's scope from a single-reference comparison tool into
a full *theoretical multiverse analysis* framework. The methodological logic is as follows.

### Theories as the unit of analysis

A *theory* is a causal explanation of an outcome Y. It can be reduced to nodes (variables)
and edges (causal or correlational relationships), encoded as a DAG, and stored as a row in
the theory universe. The universe contains both *observed theories* — those drawn from the
published literature and encoded by a researcher or LLM — and *unobserved theories* — all
formally valid alternatives generated by the algorithm that no one has yet written down.

### The multiverse

The set of all theories — observed and unobserved — constitutes the *theoretical multiverse*.
It is not simply the list of generated models; it includes the metadata about each theory's
origin, its hypothesis targets, and its provenance.

### Dyadic analysis

Every theory in the multiverse can serve as a *reference model (ego)* and every other theory is
then a *comparison model (alter)*. This produces a full N×(N−1) dyad matrix. For each dyad the
analysis records:

1. **Identification state**: is each theory individually identified for the `Xtest -> Y` claim?
2. **Dyad consistency**: are ego and alter both identified, both unidentified, or in conflict?
3. **MAS compatibility**: do the two theories agree on at least one valid adjustment set?
4. **Full-theory compatibility**: do they agree on nodes, adjustment correctness, and MAS?
5. **Conflict class**: if incompatible, what type of structural disagreement explains why?
6. **Minimal repair**: what is the smallest set of node or edge changes that would make the
   alter consistent with the ego?

### Uncertainty measurement

Aggregating across all dyads yields *uncertainty metrics*:

- **Theory-level**: proportion of the multiverse compatible with a given theory.
- **Component-level**: which specific edges or nodes, when present or absent, most strongly
  predict dyad incompatibility? These are the theoretical "hot spots" where development is
  most needed.
- **Multiverse-level**: overall proportion of dyads that are consistent (identification
  uncertainty index).
- **Reference sensitivity**: do the headlines change materially depending on which theory is
  chosen as ego?

### Meta-analysis layer

Once the dyad matrix exists, the analysis moves to the multiverse level:

- **Descriptives**: how much of the plausible multiverse is covered by published theories?
  Are there regions of the space that are densely or sparsely occupied?
- **Theory zones**: clusters of theories with similar compatibility profiles, suggesting
  implicit theoretical traditions.
- **QCA / set logic**: which combinations of causal components are necessary or sufficient
  for high compatibility, correct identification, or coverage of the MAS?

### Hypothesis-test focus

The analysis is organised around a specific causal hypothesis: the effect of `Xtest` on `Y`.
For any theory in the multiverse the package asks:

- **MAS test**: would a researcher using only the minimum adjustment set from this theory
  successfully isolate the `Xtest -> Y` relationship?
- **Full-model test**: would a researcher using all variables in the theory successfully
  isolate it?
- **Dyad consistency**: are two theories likely to produce the same conclusion about whether
  that relationship is identified?

### Simulation for the Economizing Theory paper

The workflow supports a simulation-based demonstration where:

1. A subfield is defined by a set of known published theories (encoded by hand or LLM).
2. Scholar behaviour is simulated: each scholar follows only their preferred theory.
3. The package measures what fraction of the resulting research is likely to be unidentified
   or unidentifiable without cross-theory awareness.
4. Sensitivity analyses vary the size and composition of the observed theory set.

---

## 3. Main Differences Between the Two Approaches

| Dimension | Current package | New workflow |
|---|---|---|
| **Unit of analysis** | Generated model row | Theory with provenance and metadata |
| **Comparison structure** | One reference vs. many | All-by-all dyad matrix |
| **Output of comparison** | Compatible / incompatible | Compatible / incompatible + conflict class + minimal repair |
| **Uncertainty** | Not measured | Quantified at component, theory, and multiverse level |
| **Theory origin** | Undistinguished | Observed (literature) vs. unobserved (generated) |
| **Hypothesis grounding** | Implicit in MAS | Explicit hypothesis object with ego/alter semantics |
| **QCA input** | Binary compatibility from one reference | Dyad or component outcomes aggregated across all pairs |
| **Simulation** | Absent | First-class workflow for paper demonstrations |
| **Visualization** | Individual DAG plots | Multiverse-level summaries, uncertainty maps, zone maps |
| **Testing** | None | Required; unit tests for every structural layer |
| **Scale management** | No pruning | Needs explicit pruning and lazy-evaluation strategies |

The philosophical shift: the current package asks *"does the world agree with my theory?"*
The new workflow asks *"how much do theories in this space agree with each other, why do they
disagree, and where does theoretical development matter most?"*

---

## 4. What Needs to Be Done

### 4a. Foundational fixes (must come first)

- **Fix the `is.double` input validation bug** in `build_causal_node.R` line 55. Integer
  timing vectors (the natural R type from `c(0, -1, -3, -2)`) currently fail the check.
- **Remove interactive `readline()` calls** from core functions. Non-interactive mode must be
  supported everywhere. Replace with function parameters or callbacks.
- **Start a real test suite** covering: input validation, model generation correctness, MAS
  computation, compatibility logic, and deduplication.
- **Correct the timing-cap error message** (says "less than 4", code allows 5).

### 4b. Theory object redesign

Extend `ls_theory` or replace it with a structured object that carries:

- Per-theory metadata: source, observed / generated flag, theory name or citation, asserted
  hypothesis variable.
- A clear design decision on whether the object is an S3 class, an R5/R6 class, or a
  structured list with a constructor.
- A `theory_registry` table separate from the causal matrix that stores one row per theory and
  links to model numbers.

### 4c. All-pairs dyad engine (new function: `build_dyad_matrix`)

Replace `add_compatible`'s single-reference logic with a function that:

- Accepts an `ls_theory` object plus an optional subset of theory numbers as ego candidates.
- Returns a dyad matrix with columns: `ego_mod`, `alter_mod`, `ego_identified`,
  `alter_identified`, `dyad_consistent`, `mas_compatible`, `full_compatible`, `conflict_class`,
  `repair_candidates`.
- Reuses `add_mas`, `create_formula`, and `unq_nodes_detect` as internal helpers.
- Supports parallelisation for large universes.

### 4d. Uncertainty and scoring layer (new function: `score_uncertainty`)

- Aggregate the dyad matrix into component- and theory-level scores.
- Compute: proportion compatible per theory, edge/node uncertainty contribution, reference
  sensitivity index, theory-zone membership.
- Return a structured summary object suitable for both reporting and QCA input.

### 4e. Rebuilt QCA export (`build_set_matrix` redesign)

- Accept dyad or component-level summaries rather than a single reference.
- Support fuzzy-set or graded outcomes, not only binary 0/1.
- Retain backward compatibility for users already calling the current function.

### 4f. Simulation module (new function family: `simulate_subfield`)

- Define scenario inputs: a set of observed theories, scholar-preference rules, sample size,
  and identification threshold.
- Use the existing graph generator as the theory data-generating process.
- Produce replicable outputs (seed-controlled) including: unidentified rate, coverage of the
  theoretical space, sensitivity to including or excluding specific theories.

### 4g. Minimal-repair diagnostics (new function: `find_repairs`)

- For an inconsistent ego/alter pair, find the smallest set of node or edge changes to restore
  consistency.
- Prioritise repair candidates by the number of theories that would become compatible if the
  change were adopted.

### 4h. Summary visualizations (new function: `plot_multiverse`)

- **Dyad compatibility matrix**: heatmap of all-by-all compatibility.
- **Uncertainty map**: nodes and edges coloured by their uncertainty contribution score.
- **Theory-zone map**: cluster plot of theories by compatibility profile.
- **Repair chart**: bar chart of which components, if changed, reduce total multiverse
  uncertainty the most.

### 4i. Documentation

- Rewrite the vignette to explain the full workflow without needing to read an external paper.
- Define all key terms (theory, observed, unobserved, dyad, ego, alter, repair, uncertainty)
  in the package documentation.
- Update the README roadmap to reflect the new design.
- Add a `NEWS.md` file tracking changes from v0.1.0 onwards.

---

## 5. Areas Needing Further Brainstorming

### 5a. The right representaton for "observed" theories

What does it mean for a theory to be "from the literature"? Possible answers include: the
researcher authors a formula by hand; an LLM parses a text excerpt and extracts a DAG; a
structured form is filled in. Each implies different tooling and different error modes. The
package needs a design decision here before the theory object can be built.

### 5b. Conflict class taxonomy

When two theories are incompatible, the current package offers no explanation. A useful
conflict taxonomy might include: missing confounder, spurious collider, mediator treated as
confounder, different exposure timing, contradictory back-door paths. The right taxonomy
depends on both the formal causal structure and what is meaningful to substantive researchers.
This is largely an open methodological question.

### 5c. Minimal repair semantics

"Minimal" could mean fewest edges changed, fewest nodes changed, smallest graph edit distance,
or the change that maximises downstream compatibility with the rest of the multiverse. These
are not the same answer and the choice has methodological implications. There may also be
multiple equally minimal repairs; how should those be ranked or presented?

### 5d. Theory-zone definition

Should zones be defined by clustering over edge-presence vectors, over MAS signatures, or over
dyad compatibility profiles? Profile-based clustering is most aligned with the paper's goals
but is computationally more expensive. A good zone algorithm should be stable (zones should
not shift wildly with small changes to the universe) and interpretable (the centroid of a zone
should be describable in substantive language).

### 5e. Reference sensitivity as a first-class output

Whether multiverse headlines change depending on which theory is chosen as ego is important for
the paper's validity claims. Designing this as a formal sensitivity analysis — rather than just
running the analysis with different references — requires decisions about what constitutes a
material change and how to summarise sensitivity concisely.

### 5f. Identification vs. identifiability

The current package checks whether a model is *correctly adjusted* (given all controls) but
does not formally distinguish: (a) the effect is point-identified, (b) it is partially
identified, or (c) it is unidentified entirely via any strategy. For the paper's simulation
claims about "unidentified" research, this distinction matters. `dagitty::isIdentified()` may
be the relevant function.

### 5g. Scalability boundary

At what universe size does the all-by-all dyad matrix become computationally untenable? With
N theories, the dyad matrix has N×(N−1) rows. For a real subfield with 50–200 theories, that
is manageable. For the full generated universe with thousands of models it may not be. The
package needs an explicit strategic decision: either (a) full-matrix only for small universes
and summary statistics for large ones, or (b) lazy/streaming evaluation throughout. This
affects the data structure design fundamentally.

### 5h. LLM-assisted theory encoding

The methodological workflow mentions that theories can be encoded using LLMs. The package
could support this by providing a structured schema for theory input that an LLM can be
prompted to populate. Questions include: what is the schema, how is uncertainty in LLM-parsed
edges handled, and how does the package validate LLM-generated inputs?

### 5i. Linking to empirical data (future phase)

The README and workflow both mention linking the theoretical multiverse to real data to measure
which theoretical investments "do not matter" because results do not change. This is a
substantial additional layer. Before designing it, the core multiverse analysis needs to be
stable and interpretable, because otherwise the empirical layer will be asked to compensate for
methodological ambiguities that should be resolved at the theory level.

### 5j. Interactive explorer scope

An interactive Shiny-based workbench is mentioned in the README. Before scoping it, it is worth
deciding whether the primary user is a methodologist exploring an unfamiliar subfield, a
reviewer checking a submitted paper's theoretical assumptions, or a student learning causal
inference. Each user profile implies a different interaction model and a different set of
essential views.

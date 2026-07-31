# theoRy

An R package for comparing causal-theory multiverses through an R client and a
Python FastAPI computation engine.

[Hung H.V. Nguyen](https://orcid.org/0000-0001-9496-6217)<br>
[Nate Breznau](https://sites.google.com/site/nbreznau/)

## Overview

theoRy represents a theory multiverse in two layers: a registry of possible
node and edge components, and model states that assign claims to those
components. The R package is the public client for building inputs, requesting
analyses, and plotting results. The Python backend in `inst/python/` owns state
expansion, pairwise comparison, Delta-U, simulations, clustering, and symbolic
analysis. R communicates with that backend over HTTP on localhost by default.

## Installation

theoRy is not yet on CRAN. Install the R package from GitHub:

```r
install.packages("remotes")
remotes::install_github("hungnguyen167/theoRy")
```

Install the cross-platform Python engine dependencies from a source checkout:

```bash
python -m pip install -e inst/python
```

Full causal analyses default to the R backend so that identification is not
limited to the backdoor criterion. Install the Python-to-R bridge and the
Dagitty/CausalEffect R packages:

```bash
python -m pip install -e 'inst/python[rpy2]'
```

```r
install.packages(c("dagitty", "causaleffect"))
```

Select `causal_backend = "r"`, `"auto"`, or `"native"` in
`build_dyad_matrix()`. `"r"` is the default and uses Dagitty plus
`causaleffect` for general identification. `"auto"` first uses native
backdoor identification and falls back to R when needed; `"native"` never
loads R and supports backdoor identification only.

## Engine Lifecycle

Start the FastAPI engine once at the beginning of every R session that uses the
modern API, and stop it when the work is complete:

```r
library(theoRy)

start_theory_engine()
# Modern theoRy calls go here.
stop_theory_engine()
```

The default URL is `http://localhost:8000`; override it with
`options(theoRy.engine_url = "https://engine.example.org")`. The launcher uses
`processx` and works the same way on Linux, macOS, and Windows. It only stops
backend processes launched by the current R session. To deliberately shut down
a compatible engine started elsewhere, use
`stop_theory_engine(stop_external = TRUE)`; this uses the engine's HTTP
shutdown endpoint and never kills a process merely because it occupies a port.

## Modern Workflow

```r
start_theory_engine()

registry <- build_component_registry(
  nodes = c("Z", "X", "Y"),
  timing = c(1, 2, 3),
  exposure = "X",
  outcome = "Y"
)

states <- expand_model_states(
  registry,
  mode = "exhaustive",
  edge_statuses = c("causal", "unknown", "non-causal")
)

dyads <- build_dyad_matrix(
  registry,
  states,
  mode = "full",
  exposure = "X",
  outcome = "Y"
)

lynchpins <- compute_delta_u(
  dyads,
  compatibility_metric = "mas_compatible"
)

simulation <- run_simulation(
  "illusion_of_precision",
  compatibility_metric = "identified_compatible"
)

stop_theory_engine()
```

For a one-call concrete pipeline, use `analyze_theory()`. The explicit steps
above are preferable when selecting a causal compatibility metric for Delta-U
or simulations.

### Interactive Workflow

Use `build_component_registry_interactive()` to answer a guided questionnaire,
or run the complete pipeline with:

```r
result <- analyze_theory(input_mode = "interactive")
```

The questionnaire requires named variables, exposure, outcome, and a time for
every variable. The focal exposure and outcome each have one fixed time, while
up to two other variables may have two allowed positions. The resulting
multiverse contains one timing assignment per model. It can also collect
required/forbidden directed paths, possible latent-confounding pairs, and
variables eligible to be absent.

### Programmatic Timing Uncertainty

Exposure and outcome must always be declared and have one fixed time. For
non-focal variables, `NA` timing values require an explicit finite
`time_points` vector; `timing_options` can give a node a smaller allowed set.
The engine reports timing assignments pruned by required paths and applies a
global model-count limit before exhaustive expansion.

```r
registry <- build_component_registry(
  nodes = c("Education", "Income", "Health"),
  timing = c(1, NA, 4),
  time_points = 1:4,
  timing_options = list(Income = c(2, 3)),
  exposure = "Education",
  outcome = "Health",
  optional_nodes = "Income"
)
```

## Compatibility Metrics

The modern compatibility API exposes exactly three pairwise metrics:

| Metric | Type | Definition |
|---|---|---|
| `similarity_rate` | numeric in `[0, 1]` | Component-level structural agreement. An edge applicable in exactly one model contributes one disagreement and one repair; an edge inapplicable in both models is ignored. |
| `mas_compatible` | logical or unavailable | Both models retain at least one common minimal adjustment strategy for the same total-effect query. For partial models, the strategy must survive every valid resolved completion. |
| `identified_compatible` | logical or unavailable | Both models support general identification of the same total-effect query **and** have equal declared relevant node sets after removing robust directed-path intermediates. Two non-identified models are not compatible. |

`repair_cost` is a helper, not a fourth compatibility metric. It counts
component-level edits, so a missing node and each one-sided incident edge each
contribute one repair.

### Empty and absent adjustment sets

A valid empty adjustment set and the absence of any valid adjustment set are
different results:

| Result | Python/JSON shape | Meaning |
|---|---|---|
| Valid empty set `{}` | `[[]]` | Adjustment is valid without conditioning; two such profiles can be MAS-compatible. |
| No valid set | `[]` | No adjustment strategy exists; even two no-set models are not MAS-compatible. |
| Unavailable query | `null` | The query is missing, a query node is absent, completion coverage is incomplete, or computation failed. |

### Unknown edges and completions

Unknown edges are never treated as absent for causal metrics. A partial model is
evaluated through fully resolved descendants already represented in the
multiverse. Necessary causal status is available only when all valid
completions are covered; incomplete coverage returns an unavailable value.
A represented non-identified completion is already a valid counterexample.

Synthetic causal simulations may materialize missing resolved descendants as
completion support. Those descendants are internal support artifacts: they do
not increase `n_models` and are excluded from ordinary dyad aggregation,
Delta-U ranking, and clustering. Seeded simulations do not silently add models
to a user corpus and instead require completion-closed input.

## Causal Queries

`mas_compatible` and `identified_compatible` generally require both `exposure`
and `outcome`; `similarity_rate` does not. Generated Precision Illusion
simulations are the exception: their fixed design lets the backend infer
exposure `X1` and outcome `Y`. The causal query is the total effect
`P(Y | do(X = x))` in an observed ADMG. A query node absent from one model
makes that model's profile unavailable rather than non-identified.

The native backend supports tested backdoor-adjustment queries, including
declared bidirected latent-confounding relations. General identification,
including effects identifiable beyond adjustment such as front-door cases,
uses the optional R `causaleffect` backend. Native-only requests outside that
scope return an explicit unsupported result rather than silently using a
structural comparison.

## Simulations

Crux of Certainty and Ghost Discovery accept any canonical compatibility
metric:

```r
compatibility_metric = "similarity_rate" # or "mas_compatible" or "identified_compatible"
```

```r
illusion <- run_simulation_illusion(
  compatibility_metric = "mas_compatible"
)

crux <- run_simulation_crux(
  compatibility_metric = "identified_compatible",
  exposure = "X1",
  outcome = "X2"
)

ghost <- run_simulation_ghost(
  compatibility_metric = "similarity_rate"
)
```

Precision Illusion requires `mas_compatible` or `identified_compatible` and
defaults to `mas_compatible`; `similarity_rate` is its structural comparison
baseline rather than a valid selected metric. Its main results are
`mean_similarity_rate`, `compatibility_rate`, `precision_illusion_gap`,
resolved and partial model counts, design, and diagnostics.

All simulations are directed-only. `include_bidirectional = TRUE` is rejected
in concrete and symbolic modes rather than ignored; bidirected components are
still supported by the general non-simulation APIs.

The selected metric drives scenario aggregation, Delta-U, before/after shifts,
and clustering. Simulation measurements are not clamped to acceptance bounds;
a synthetic candidate must satisfy its threshold or be regenerated. Strict
synthetic acceptance thresholds currently apply to structural runs, while
causal runs report their measured binary rates without reusing structural
cutoffs.

Structural similarity remains the recommended and default Ghost Discovery
metric because it carries continuous information. Boolean causal metrics are
supported, but missing values are rejected and no-variance profile matrices
return an explicit degenerate/no-cluster result instead of arbitrary UMAP
clusters. In particular, identification compatibility cannot distinguish
different mechanisms when every model identifies the query.

Symbolic simulations currently support `similarity_rate` only. Their symbolic
identification fields describe adjustment identification, not general ID, so
requests for either causal compatibility metric are rejected rather than
relabeled incorrectly.

## Breaking Change

The former strict/full modern dyad metric and its model helper fields were
removed, not aliased. `identified_compatible` has different general-ID
semantics, and the old composite scoring controls were replaced by the single
`compatibility_metric` selector. Restart the engine after upgrading: in-memory
sessions created by the previous backend schema are incompatible with current
requests and responses.

As of `0.2.0`, `identified_compatible` is stricter: two models are identified-
compatible only when **both** independently identify the same total-effect
query **and** their relevant declared node sets are exactly equal after
removing robust directed-path intermediates. For each resolved model, the
relevant set is all declared present nodes (observed **and** latent) minus
nodes that lie on at least one directed exposure-outcome path in the declared
directed graph; bidirected edges never make a node an intermediate. For a
partial model, a node is removed only when it is a directed-path intermediate
in **every** valid represented completion (the robust union rule), so an
uncertain possible mediator is retained rather than ignored; incomplete
completion coverage makes the relevant set unavailable. Identification itself
is still computed by `causaleffect` over the observed latent-projected ADMG;
only cross-model comparability uses the declared node set. Two non-identified
models are not compatible, and either unavailable identification or an
unavailable relevant set returns unavailable.

## Deprecated Legacy API

`run_theoRy()`, `build_causal_node()`, `build_formula_matrix()`,
`build_set_matrix()`, `add_compatible()`, and `find_add_models()` are the
deprecated, pure-R workflow. The `full_model_compatible` column produced by
legacy `add_compatible()` is a distinct historical concept. It is not one of
the three modern metrics and has not been redefined as general identification.

## Funding

This package was developed within the project
"[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)".
Nate Breznau, Principal Investigator. Deutsche Forschungsgemeinschaft (German
Research Foundation), Project Number 464546557.

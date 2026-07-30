# theoRy

An R package for comparing causal-theory multiverses through an R client and a
Python FastAPI computation engine.

[Hung H.V. Nguyen](https://orcid.org/0000-0001-9496-6217)<br>
[Nate Breznau](https://sites.google.com/site/nbreznau/)

## Abstract

This package supports our method of comparing theories to determine if they are compatible or not, and why. This is done through a combination of causal inference, set logic, visualization and meta-analysis. The method takes an existing theory specified at the causal level in the form of a directed acyclical graph (causal path model) and compares it with a multiverse of simulated and researcher-specified alternatives. Through meta-analysis, the method determines which particular model components of causal paths (edges) and variables (nodes) are more or less important when comparing models under conditions of theoretical uncertainty. The goal is to enable a researcher to take weak theory in a given area of science and discover where theoretical development is needed most. ‘Needed’ refers to where models are less compatible at the meta-level, and thus where theoretical development will provide the greatest knowledge gains. The compatibility is analyzed partly through qualitative comparative analysis of the causal model components and their roles alone or in particular sets. We developed an R software package, called theoRy, to both demonstrate our method and enable other researchers to use it. The package builds on the existing daggity, ggdag, and QCA packages. 

The method uses a set of variables and basic information about their chronology, and transforms them into a multiverse of potential causal models stored in a network matrix. Built on top of dagitty and ggdag, this package: (a) develops an algorithm to isolate and categorize the node(s) or edge(s) that make models incompatible, (b) provides tools to analyze models visually, assumptively and statistically, and (c) tools to meta-analyze causal assumptions qualitatively and statistical outcomes quantitatively.

## Package Overview

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

Install the Python engine dependencies from a source checkout:

```bash
python -m pip install -e inst/python
```

The backend calls R through `rpy2`. Dagitty computes minimal adjustment sets,
while `causaleffect` implements general causal identification for ADMGs. Install
the R dependencies in the same R library visible to `rpy2`:

```r
install.packages(c("dagitty", "causaleffect"))
```

`causaleffect` is listed in `DESCRIPTION` under `Suggests`, not `Imports`,
because it is invoked by the Python/rpy2 process rather than directly by R
package code.

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
`options(theoRy.engine_url = "https://engine.example.org")`. On supported Linux
systems, `start_theory_engine()` applies the `libstdc++.so.6` preload needed by
the R/rpy2 bridge.

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

## Compatibility Metrics

The modern compatibility API exposes exactly three pairwise metrics:

| Metric | Type | Definition |
|---|---|---|
| `similarity_rate` | numeric in `[0, 1]` | Component-level structural agreement. An edge applicable in exactly one model contributes one disagreement and one repair; an edge inapplicable in both models is ignored. |
| `mas_compatible` | logical or unavailable | Both models retain at least one common minimal adjustment strategy for the same total-effect query. For partial models, the strategy must survive every valid resolved completion. |
| `identified_compatible` | logical or unavailable | Both models support general identification of the same total causal effect. Two non-identified models are not compatible. |

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

Dagitty remains responsible for minimal adjustment sets. General
identification, including effects identifiable beyond adjustment such as the
front-door case, is delegated to R `causaleffect` through `rpy2`.

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

## Deprecated Legacy API

`run_theoRy()`, `build_causal_node()`, `build_formula_matrix()`,
`build_set_matrix()`, `add_compatible()`, and `find_add_models()` are the
deprecated, pure-R workflow. The `full_model_compatible` column produced by
legacy `add_compatible()` is a distinct historical concept. It is not one of
the three modern metrics and has not been redefined as general identification.


## Future developments

- Figure out the algorithmic association of components to be able to generate the multiverse quickly. The goal is to be able to quickly run these models with 12 variables. 
- Find a way to calculate DAG results our selves rather than calling DAGiity. Edges, nodes and causal or not is all we need for this.
- Publish app in R Journal after developing the algorithmic basis.
- Reorient temporal ordering so that Y is always at time 0 (currently Y can be at different listed time points in the same multiverse)
- Force user to specify temporal ordering, and then optionally 'free' specific nodes by allowing them to be at specified time points (like X2 could come before or after X1) or free them completely so that they could be at any time point (not recommended as it blows up the size of the multiverse and is too 'weak' theoretically).
- Fix run_theoRy() help menu so that it names the objects properly (component registry etc)
## Funding

This package was developed within the project
"[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)".
Nate Breznau, Principal Investigator. Deutsche Forschungsgemeinschaft (German
Research Foundation), Project Number 464546557.

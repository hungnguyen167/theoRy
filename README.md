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

theoRy requires R 4.2.2 or later and Python 3.10 or later. The R package ships
the Python backend source, but R package installers do not install Python
packages. Install both parts before starting the engine.

### 1. Install the R package

theoRy is not yet on CRAN. Install it from GitHub:

```r
install.packages("remotes")
remotes::install_github("hungnguyen167/theoRy")
```

The required R packages, including Dagitty, are installed through the package
metadata. Install `causaleffect` as well when using the optional general-ID
bridge:

```r
install.packages("causaleffect")
```

### 2. Create a Python environment

First locate the backend bundled with the installed R package:

```bash
Rscript -e 'cat(system.file("python", package = "theoRy"))'
```

On **Linux or macOS**, create a virtual environment and install the backend
with its R bridge:

```bash
ENGINE_DIR="$(Rscript -e 'cat(system.file("python", package = "theoRy"))')"
python3 -m venv ~/.virtualenvs/theory
~/.virtualenvs/theory/bin/python -m pip install --upgrade pip
~/.virtualenvs/theory/bin/python -m pip install "${ENGINE_DIR}[rpy2]"
```

On **Windows PowerShell**:

```powershell
$engineDir = Rscript -e "cat(system.file('python', package = 'theoRy'))"
py -3 -m venv "$HOME\.virtualenvs\theory"
$python = "$HOME\.virtualenvs\theory\Scripts\python.exe"
& $python -m pip install --upgrade pip
& $python -m pip install "${engineDir}[rpy2]"
```

For development from a source checkout, the equivalent command is:

```bash
python -m pip install -e 'inst/python[rpy2]'
```

Ensure that `Rscript` is on `PATH` so `rpy2` can locate R. If it cannot, set
`R_HOME` to the directory reported by `R RHOME` before installing `rpy2`.

### 3. Start the installed backend

Pass the virtual environment's Python executable to the R launcher:

```r
library(theoRy)

theory_python <- if (.Platform$OS.type == "windows") {
  file.path(Sys.getenv("USERPROFILE"), ".virtualenvs", "theory",
            "Scripts", "python.exe")
} else {
  path.expand("~/.virtualenvs/theory/bin/python")
}

start_theory_engine(python = theory_python)
stop_theory_engine()
```

Full causal analyses default to the R backend for adjustment-set computation.
Select `causal_backend = "r"`, `"auto"`, or `"native"` in
`build_dyad_matrix()`. `"r"` is the default and uses Dagitty for adjustment
sets. `"auto"` first uses native backdoor adjustment and falls back to R when
needed; `"native"` never loads R and supports backdoor adjustment only.
`identified_compatible` always uses the native fixed-direct
complete-conditioning predicate.

## Engine Lifecycle

Start the FastAPI engine once at the beginning of every R session that uses the
modern API, and stop it when the work is complete:

```r
library(theoRy)

start_theory_engine(python = theory_python)
# Modern theoRy calls go here.
stop_theory_engine()
```

You may omit `python = theory_python` when the virtual environment is already
activated and its `python` command is first on `PATH`.

The default URL is `http://localhost:8000`; override it with
`options(theoRy.engine_url = "https://engine.example.org")`. The launcher uses
`processx` and works the same way on Linux, macOS, and Windows. It only stops
backend processes launched by the current R session. To deliberately shut down
a compatible engine started elsewhere, use
`stop_theory_engine(stop_external = TRUE)`; this uses the engine's HTTP
shutdown endpoint and never kills a process merely because it occupies a port.

## Modern Workflow

```r
start_theory_engine(python = theory_python)

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
  "consensus_illusion",
  compatibility_metric = "identified_compatible"
)

stop_theory_engine()
```

For a one-call concrete pipeline, use `analyze_theory()`. The explicit steps
above are preferable when selecting a causal compatibility metric for Delta-U
or simulations. Concrete `analyze_theory()` defaults to exhaustive expansion
so its crux analysis is resolution-closed.

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
All non-`NA` values supplied through `timing`, `time_points`, and
`timing_options` must be integers >= 1; `NA` in `timing` marks an unspecified
non-focal node.
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
| `identified_compatible` | logical or unavailable | Both models satisfy the fixed-direct exposure-to-outcome complete-conditioning d-separation predicate **and** have exactly equal declared node-presence sets (all present nodes except exposure/outcome). Two non-identified models are not compatible. |

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

## Crux Modes

`compute_delta_u()` (and `analyze_theory()`) supports two crux semantics:

- **Marginal crux** (default) ranks each applicable edge that is still
  `unknown` in at least one model by evaluating both the causal and the
  non-causal resolution of that component.
- **Global crux** ranks each non-preset edge by evaluating both directions:
  the edge is globally forced to causal and then to non-causal across every
  model where it is applicable, including models where it is unknown or has
  the opposite resolved status. The two results are compared for the ranking;
  `top_k` applies in global mode as it does in marginal mode.

`global_status` remains as a deprecated compatibility argument in the R
signatures. If supplied in global mode it must be `"causal"` or
`"non-causal"`, is ignored with a warning, and is not sent to the backend.
It is still rejected in marginal mode.

Both modes are model-remapping analyses: they never mutate model claims and
never recompute adjustment sets or identification. Each hypothetically
resolved model is mapped to the existing multiverse model whose semantic state
matches the resolution, and the mapped models' precomputed dyad records are
reused. The original baseline model and dyad counts are not changed. The one
exception is a hypothetical **directed causal** branch: timing-ineligible
model slots whose source or target timing is missing, or whose source timing is
greater than or equal to its target timing, are excluded from that branch.
Marginal mode applies this only among applicable unknown instances; global mode
applies it across all applicable models. Non-causal and bidirected branches do
not timing-prune. No edge or component is removed and no baseline state or dyad
is mutated.

For example, if a source is fixed at time 2 and a target can flexibly occur at
times 1 or 3, the causal branch excludes the time-1 target slot but retains the
time-3 slot. Results report `timing_pruned_models_causal` (or the corresponding
non-causal field), `models_pruned_*`, and per-direction
`post_model_count_*`/`post_dyad_count_*`, plus
`insufficient_post_models_*` flags; baseline `model_count` and `dyad_count`
remain the original full counts. Post compatibility is compared with that
original full baseline, while improved/worsened counts cover retained aligned
pairs. A *resolution-closed* multiverse (e.g. exhaustive expansion) is still
required for every retained remapping; when an exact match is missing, ranking
fails with a completion-coverage error instead of synthesizing new models.
Global and explicit single-component resolutions use the same strict coverage
policy.

```r
rankings <- compute_delta_u(dyads, crux_mode = "marginal", top_k = 10)

global <- compute_delta_u(dyads, crux_mode = "global", top_k = 10)
```


## Causal Queries

`mas_compatible` and `identified_compatible` generally require both `exposure`
and `outcome`; `similarity_rate` does not. Generated Consensus Illusion
simulations are the exception: their fixed design lets the backend infer
exposure `X1` and outcome `Y`. MAS queries target the total effect
`P(Y | do(X = x))`; `identified_compatible` targets the fixed direct
`X -> Y` effect. Causal queries require that direct registry edge to be fixed
as causal and applicable in every queried model. A query node absent from one
model makes that model's profile unavailable rather than non-identified.

The native backend supports tested backdoor-adjustment queries, including
declared bidirected latent-confounding relations. The optional R
`causaleffect` dependency remains available for general-ID callers, but
it is not used to determine `identified_compatible`.

## Simulations

Crux of Certainty and Ghost Discovery accept any canonical compatibility
metric:

```r
compatibility_metric = "similarity_rate" # or "mas_compatible" or "identified_compatible"
```

```r
consensus <- run_simulation_consensus(
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

Consensus Illusion requires `mas_compatible` or `identified_compatible` and
defaults to `mas_compatible`; `similarity_rate` is its structural comparison
baseline rather than a valid selected metric. Its main results are
`mean_similarity_rate`, `compatibility_rate`, `consensus_illusion_gap`,
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



## Reproducibility

Clone the repository, install R 4.2.2 or later with the `dagitty` and
`causaleffect` packages, and make sure `Rscript` is on `PATH`:

```bash
git clone https://github.com/hungnguyen167/theoRy.git
cd theoRy
Rscript -e 'install.packages(c("dagitty", "causaleffect"))'
```

The simulation environment is locked for Python 3.13. Create it with the
commands for your platform. The lock file includes hashes for all resolved
Python packages.

### Linux

```bash
python3.13 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install --require-hashes \
  -r simulations/requirements.lock.txt
.venv/bin/python simulations/scripts/run_simulations.py
.venv/bin/python simulations/scripts/verify_outputs.py
```

If a Conda-provided C++ runtime prevents `rpy2` from loading R on a
Debian/Ubuntu x86-64 system, run the driver with the system runtime preloaded:

```bash
LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libstdc++.so.6 \
  .venv/bin/python simulations/scripts/run_simulations.py
```

### macOS

```bash
python3.13 -m venv .venv
.venv/bin/python -m pip install --upgrade pip
.venv/bin/python -m pip install --require-hashes \
  -r simulations/requirements.lock.txt
.venv/bin/python simulations/scripts/run_simulations.py
.venv/bin/python simulations/scripts/verify_outputs.py
```

### Windows PowerShell

```powershell
py -3.13 -m venv .venv
& ".\.venv\Scripts\python.exe" -m pip install --upgrade pip
& ".\.venv\Scripts\python.exe" -m pip install --require-hashes `
  -r simulations/requirements.lock.txt
& ".\.venv\Scripts\python.exe" simulations/scripts/run_simulations.py
& ".\.venv\Scripts\python.exe" simulations/scripts/verify_outputs.py
```

This workflow produces three result sets (Consensus Illusion, Crux of
Certainty, and Ghost Discovery) under `simulations/results/`, and only Figure 2
and Figure 3 under `simulations/figures/`. A successful run ends with
`All verifier checks passed.`

## Funding

This package was developed within the project
"[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)".
Nate Breznau, Principal Investigator. Deutsche Forschungsgemeinschaft (German
Research Foundation), Project Number 464546557.

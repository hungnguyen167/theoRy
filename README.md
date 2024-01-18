# theoRy
An R package for comparing many causal models

[Hung H.V. Nguyen](https://orcid.org/0000-0001-9496-6217) <br>
[Nate Breznau](https://sites.google.com/site/nbreznau/) <br>

## Overview

We develop a method to compare models to determine if they are compatible or mutually exclusive regarding their capacity to identify a causal effect of a test variable X on an outcome Y. This method has the goals of identifying a multiverse of causal models, determining when and why they are incompatible, and how the results at the level of statistical testing depend on causal model specifications. Ultimately, the method points to where improvements in theory are most needed, and potentially yield the greatest returns to scientific knowledge development. The method uses a set of variables and basic information about their chronology, and transforms them into a multiverse of potential causal models stored in a network matrix. Built on top of dagitty and ggdag, this package: (a) develops an algorithm to isolate and categorize the node(s) or edge(s) that make models incompatible, (b) provides tools to analyze models visually, assumptively and statistically, and (c) tools to meta-analyze causal assumptions qualitatively and statistical outcomes quantitatively.

## Installation

The package is still in its early phase and is thus not yet available on CRAN. To install the package directly from Github, use
```
install.packages("remotes") ## if it's not yet installed in your machine
remotes::install_github("https://github.com/hungnguyen167/theoRy")
```

If you encounter any issue when installing the package, please use the issues section: https://github.com/hungnguyen167/theoRy/issues

## Main functions

| Function | Description | 
|----------|-------------|
| `run_theoRy` | A wrapper function to build necessary matrices for theory comparison (causal matrix, formula matrix, and node-timing matrix)
| `build_causal_node` |  Build causal matrix or build node-timing matrix
| `build_formula_matrix` | Build formula matrix
| `find_add_models` | Find or add models to the model universe
| `build_set_matrix` | Build a set matrix ready for set theory analysis out of the causal matrix
| `add_compatible` |  Add test compatible or full model compatible columns to the formula matrix
| `plot_dag` | Plot DAG models from a `ls_theory` object


## Future developments

## DFG Funding

This app was developed within the project "[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)"

Deutsche Forschungsgemeinschaft (German Science Foundation, DFG) - Project Number 464546557

German Title "[Der Beitrag von Theorie zur (Lösung der) Reliabilitätskrise](https://gepris.dfg.de/gepris/projekt/464546557)"

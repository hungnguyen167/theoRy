# theoRy

An R package for meta-analyzing theory via underlying causal models and assumptions

[Hung H.V. Nguyen](https://orcid.org/0000-0001-9496-6217) <br>
[Nate Breznau](https://sites.google.com/site/nbreznau/) <br>

## Overview

This package supports our method of comparing theories to determine if they are compatible or not, and why. This is done through a combination of causal inference, set logic, visualization and meta-analysis. The method takes an existing theory specified at the causal level in the form of a directed acyclical graph (causal path model) and compares it with a multiverse of simulated and researcher-specified alternatives. Through meta-analysis, the method determines which particular model components of causal paths (edges) and variables (nodes) are more or less important when comparing models under conditions of theoretical uncertainty. The goal is to enable a researcher to take weak theory in a given area of science and discover where theoretical development is needed most. ‘Needed’ refers to where models are less compatible at the meta-level, and thus where theoretical development will provide the greatest knowledge gains. The compatibility is analyzed partly through qualitative comparative analysis of the causal model components and their roles alone or in particular sets. We developed an R software package, called theoRy, to both demonstrate our method and enable other researchers to use it. The package builds on the existing daggity, ggdag, and QCA packages. 

The method uses a set of variables and basic information about their chronology, and transforms them into a multiverse of potential causal models stored in a network matrix. Built on top of dagitty and ggdag, this package: (a) develops an algorithm to isolate and categorize the node(s) or edge(s) that make models incompatible, (b) provides tools to analyze models visually, assumptively and statistically, and (c) tools to meta-analyze causal assumptions qualitatively and statistical outcomes quantitatively.

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

### FUNCTIONS

*subset_matrix* - a function to subset compatibilitiy and set matricies by specifying which components of the model should or should not be included

*compare_theory* - a function to assign various scores to components and sets of components based on their risk to making a model (in)compatible

*visualize_theory* - a function to visually display the comparison scores based on use specified goals of theoretical comparison

*link_data* - a function to link a node matrix (theoretical multiverse) with real data

*compare_theory_outcomes* - a function to analyze the linked data through the models underlying the theory multiverse. User can specify functional form, estimation parameters and data parameters. Output includes additional compare_theory scores that demonstrate where alternative theoreical assumptions lead to statistically significantly different results on the Xtest -> Y comparison

### INTERATIVE APP

We will develop a function or easy deploymnet strategy where users can visually perform our method, most likley based on shiny.

## DFG Funding

This app was developed within the project "[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)". Nate Breznau, Principal Investigator.

Deutsche Forschungsgemeinschaft (German Science Foundation, DFG) - Project Number 464546557

German Title "[Der Beitrag von Theorie zur (Lösung der) Reliabilitätskrise](https://gepris.dfg.de/gepris/projekt/464546557)"

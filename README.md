# theoRy
An R app for comparing many causal models

[Nate Breznau](https://sites.google.com/site/nbreznau/) <br>
[Hung H.V. Nguyen](https://orcid.org/0000-0001-9496-6217) <br>

## Overview

We develop a method to compare models to determine if they are compatible or mutually exclusive regarding their capacity to identify a causal effect of a test variable X on an outcome Y. This method has the goals of identifying a multiverse of causal models, determining when and why they are incompatible, and how the results at the level of statistical testing depend on causal model specifications. Ultimately, the method points to where improvements in theory are most needed, and potentially yield the greatest returns to scientific knowledge development. The method uses a set of variables and basic information about their chronology, and transforms them into a multiverse of potential causal models stored in a network matrix. Using R Studio statistical software with DAGgity (DAGs - directed acyclic graphs) and lavaan (SEM - structural equation modeling) package infrastructures, it: (a) develops an algorithm to isolate and categorize the node(s) or edge(s) that make models incompatible, (b) provides tools to analyze models visually, assumptively and statistically, and (c) tools to meta-analyze causal assumptions qualitatively and statistical outcomes quantitatively.

## DFG Funding

This app was developed within the project "[The Role of Theory in Understanding (and Resolving) the Reproducibility Crisis](https://gepris.dfg.de/gepris/projekt/464546557?language=en)"

Deutsche Forschungsgemeinschaft (German Science Foundation, DFG) - Project Number 464546557

German Title "[Der Beitrag von Theorie zur (Lösung der) Reliabilitätskrise](https://gepris.dfg.de/gepris/projekt/464546557)"

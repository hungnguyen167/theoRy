message("theoRy v0.1")
message("Breznau, Nate and Hung H.V. Nguyen")

packages <- c("tidyverse",
              "dagitty",
              "here",
              "data.table")

pacman::p_load(packages, character.only = T)

source("R/build_causal_matrix.R")

source("R/build_formula_matrix.R")

source("R/build_dag_matrix.R")

source("R/identify_compatible.R")

source("R/plot_dag_matrix.R")

#source("R/keep_minimal.R")







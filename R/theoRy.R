message("theoRy v1.0")
message("Breznau, Nate and Hung H.V. Nguyen")

packages <- c("tidyverse",
              "dagitty",
              "here",
              "data.table",
              "QCA")

pacman::p_load(packages, character.only = T)

source("R/build_causal_matrix.R")

source("R/build_formula_matrix.R")

source("R/build_dag_matrix.R")

source("R/identify_compatible.R")

source("R/plot_dag_matrix.R")

run_theory <- function(inputs){
    causal_matrix <- build_causal_matrix(inputs, return_node=FALSE)
    node_timing <- build_causal_matrix(inputs, return_node=TRUE)
    formula_matrix <- build_formula_matrix(causal_matrix)
    ls_theory <- list(causal_matrix = causal_matrix, node_timing=node_timing, formula_matrix=formula_matrix)
    return(ls_theory)
}





message("theoRy v1.0")
message("Breznau, Nate and Nguyen, Hung H. V.")




#source("R/build_causal_node.R")

#source("R/find_add_models.R")

#source("R/build_formula_matrix.R")


#source("R/add_compatible.R")

#source("R/plot_dag.R")

#source("R/build_set_matrix.R")

theoRy <- function(nodes, types, timing,include_subsets=FALSE, user_mods=NULL){
    causal_matrix <- build_causal_node(nodes, types, timing,include_subsets=include_subsets,
                                         user_mods=user_mods, return_node=FALSE)
    node_timing <- build_causal_node(nodes, types, timing,include_subsets=include_subsets, return_node=TRUE)
    formula_matrix <- build_formula_matrix(causal_matrix)
    ls_theory <- list(causal_matrix = causal_matrix, node_timing=node_timing, formula_matrix=formula_matrix)
    return(ls_theory)
}




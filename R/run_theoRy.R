#' A wrapper function to build necessary matrices for theory comparison
#' @description
#' This function returns three main matrices required in theory comparison, namely the node-timing matrix (user inputs),
#' the causal matrix, and the formula matrix. See \code{\link{build_causal_node}} and \code{\link{build_formula_matrix}}.
#'
#' @param nodes the input nodes/variable names, a character vector/list.
#' Should be of the same length and order with types and timing.
#' @param types the input types of nodes, a character vector/list. Takes only three values: "otc" (outcome), "ctr" (control),
#' or "test" (test). Should be of the same length and order with nodes and timing.
#' @param timing the input timing of nodes, a character vector/list. Should be of the same length and order with nodes and types.
#' @param user_mods User-defined model(s), optional. This argument allows users to input their theoretical biases.
#' If the model(s) are found in the matrix, they will be pushed to the top in the orders that they are introduced.
#' If the model(s) are not found in the matrix, the user can choose to add it to the matrix or not.
#' @param include_subsets if TRUE, the matrix will include models where certain nodes (besides outcome and exposure)
#' do not exist. Note that model A where X does not cause anything but still exists is a different theoretical claim than
#' model B where X does not exist entirely. The matrix can get significantly larger when this option is allowed. Default to FALSE.
#'
#' @returns a list with three objects: causal_matrix, formula_matrix, and node_timing matrix.
#'
#' @examples
#' nodes <- c("y","xtest","ctr1","ctr2")
#' timing <- c(0,-1,-3,-2)
#' types <- c("otc","test","ctr","ctr")
#' user_mods <- c("y ~ xtest + ctr2; xtest ~ ctr1 + ctr2", "y ~ xtest + ctr1; xtest ~ ctr1 + ctr2")
#' ls_theory <- run_theoRy(nodes=nodes, types=types, timing=timing, include_subsets=TRUE, user_mods=user_mods)
#'
#' @export

run_theoRy <- function(nodes, types, timing,include_subsets=FALSE, user_mods=NULL){
    causal_matrix <- build_causal_node(nodes, types, timing,include_subsets=include_subsets,
                                         user_mods=user_mods, return_node=FALSE)
    node_timing <- build_causal_node(nodes, types, timing,include_subsets=include_subsets, return_node=TRUE)
    formula_matrix <- build_formula_matrix(causal_matrix, node_timing=node_timing)
    ls_theory <- list(causal_matrix = causal_matrix, node_timing=node_timing, formula_matrix=formula_matrix)
    return(ls_theory)
}




#' Build formula matrix
#'
#' @description
#' `build_formula_matrix` creates a formula matrix (in the lavaan format) from an input causal matrix.
#'
#' @details
#' This function is included in `run_theoRy`. Unless computing the formula matrix separately is require, users are encouraged to use
#' `run_theoRy` instead.
#'
#'
#'
#' @param causal_matrix the input causal matrix. Created from theoRy:::build_causal_node
#' @param node_timing the input node timing matrix. Created from theoRy:::build_causal_node
#'
#' @returns A formula matrix with 5 columns. The formula column is in R-like syntax.
#' The MAS column is the minimum adjustment sets to measure the direct effect from Xtest to Y. correct_test (yes or no)
#' is whether the model is correctly adjusted when all X variables are adjusted. This is useful for \code{\link{add_compatible}}
#' later.
#' @examples
#' formula_matrix <- build_formula_matrix(causal_matrix, node_timing)
#'
#' @export



build_formula_matrix <- function(causal_matrix, node_timing=NULL) {
    causal_matrix_t <- data.table::copy(causal_matrix)

    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix_t, by = "model"), create_formula)

    formula_matrix <- data.table::data.table(formula = unlist(formula_list), model = unique(causal_matrix_t$model))
    reduced_matrix <- unique(causal_matrix_t[, .SD, .SDcols = c("model", "user_mod")])
    formula_matrix <- formula_matrix[reduced_matrix, on = "model", nomatch = 0]

    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    mas <- lapply(split(formula_matrix, by="model"), add_mas, additional_args)

    formula_matrix$mas <- mas
    timing_Y <- unname(unlist(node_timing[which(node_timing$node_name=="Y"),"timing"]))
    Xs_after_Y <- node_timing %>% dplyr::filter(timing>=timing_Y) %>% dplyr::select(node_name) %>% unlist() %>% unname()
    mas_adj <- lapply(split(formula_matrix, by="model"), add_mas, additional_args, adjusted=TRUE,Xs_after_Y=Xs_after_Y)
    formula_matrix$correct_test <- mas_adj

    return(formula_matrix)
}









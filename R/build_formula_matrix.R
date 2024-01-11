#' Build formula matrix
#'
#' @description
#' `add_compatible` returns a comparison matrix, which is a formula matrix
#'  with both the test compatible and full model compatible columns. The user is strongly recommended to
#'  pick a reference model which they want to compare against the model universe.
#'  If not chosen, reference model is default to the first model in the formula matrix.
#'
#' @details
#' add_compatible requires both correct_test and formula to be present in the formula matrix.
#' This is the default behavior when using build_formula_matrix to create the formula matrix.
#'
#'
#' @param causal_matrix the input causal matrix. Create from \code{\link{build_causal_matrix}}

#'
#' @returns
#'
#' @examples
#' formula_matrix <- build_formula_matrix(causal_matrix)
#'
#' @export



build_formula_matrix <- function(causal_matrix) {
    data.table::setDT(causal_matrix)
    causal_matrix_t <- data.table::copy(causal_matrix)

    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix_t, by = "model"), create_formula)

    formula_matrix <- data.table::data.table(formula = unlist(formula_list), model = unique(causal_matrix_t$model))
    reduced_matrix <- unique(causal_matrix_t[, .SD, .SDcols = c("model", "user_mod")])
    formula_matrix <- formula_matrix[reduced_matrix, on = "model", nomatch = 0]

    message("Finished creating formulas. Now adding MAS.")
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    mas <- lapply(formula_matrix$formula, add_mas, additional_args, return_string=FALSE)

    formula_matrix$mas <- mas

    mas_adj <- lapply(formula_matrix$formula, add_mas, additional_args, adjusted=TRUE, unq_Xs=all_unique_Xs)
    formula_matrix$correct_test <- mas_adj

    return(formula_matrix)
}









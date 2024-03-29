#' Build a set matrix ready for set theory analysis out of the causal matrix
#'
#' @description
#' `build_set_matrix` creates a set matrix used in theory analysis
#'

#'
#' @param causal_matrix the input causal matrix. Created from \code{\link{build_causal_node}}.
#' @param outcome_var the variable in the compatibility matrix used to measure the outcome. Default to "test_compatible".
#' @param outcome_positive the label in outcome_var that indicates a positive outcome. Default to "compatible".
#' @param cmp_matrix the compatibility matrix. Created from \code{\link{add_compatible}}.

#'
#' @returns A set matrix used for theory comparison.
#' @examples
#' set_matrix <- build_set_matrix(causal_matrix=causal_matrix, cmp_matrix=cmp_matrix)
#'
#' @export

build_set_matrix <- function(causal_matrix,
                             outcome_var="test_compatible",
                             outcome_positive = "compatible", # choose the string that indicates a positive outcome
                             cmp_matrix) {

    causal_matrix_t <- causal_matrix %>%
        as.data.frame() %>%
        subset(select = c(model,component)) %>%
        tidyr::pivot_wider(
            names_from = component, values_from = component,
            values_fn = list(component = function(x) 1),
            values_fill = list(component = 0)
        )



    # add formula results
    formula_matrix_t <- as.data.frame(cmp_matrix) %>% dplyr::select(model, !!sym(outcome_var))

    ref_mod <- cmp_matrix %>%
        as.data.frame() %>%
        dplyr::filter(!!sym(outcome_var) == "reference model") %>%
        dplyr::select(model, formula, mas)
    formula_matrix_t <- subset(as.data.frame(formula_matrix_t), model != as.numeric(ref_mod[1,1])) %>%
        mutate(outcome = ifelse(!!sym(outcome_var) == outcome_positive, 1, 0)) %>%
        dplyr::select(-!!sym(outcome_var))


    causal_matrix_t <- causal_matrix_t %>%
        subset(model != as.numeric(ref_mod[1,1])) %>%
        dplyr::left_join(formula_matrix_t, by = "model")

    print(paste("Reference Model NUMBER = ", ref_mod[1,1], sep = ""))
    print(paste("Reference Model FORMULA is ", ref_mod[1,2], sep = ""))
    print(paste("Reference Model MAS = ", ref_mod[1,3], sep = ""))
    print(paste("Set Matrix OUTCOME set to ", outcome_var, " = ", outcome_positive))
    return(as.data.frame(causal_matrix_t))

}


#' Add test compatible or full model compatible columns to the formula matrix
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
#' @param formula_matrix the input formula matrix. Created from \code{\link{build_formula_matrix}}
#' @param effect Effect type for computing the minimum adjustment sets (MAS). See also \code{\link[dagitty]{adjustmentSets}}
#' @param ref_mod the input reference model. Should avoid using models with correct_test=="no" (incorrectly adjusted)
#' as the reference model.

#'
#' @returns A comparison matrix (data.table format) with two notable columns: test_compatible and full_model_compatible.
#' Consult the paper that goes along with the package for a deeper understanding of what test_compatible and full_model_compatible are.
#'
#' @examples
#' cmp_matrix <- add_compatible(formula_matrix, effect="direct", ref_mod=1)
#'
#' @references TBA
#' @export




add_compatible <- function(formula_matrix,
                           effect="direct",
                           ref_mod=NULL){
    ## Check routines

    if (is.null(effect)){
        effect = "direct"
        warning("Effect not given, use default (direct) instead.")
    }


    if (is.null(ref_mod)){
        ref_mod = 1
        warning("User did not choose a reference model, default to the first model")
    }
    else {
        ref_mod = which(formula_matrix_t[,model]==ref_mod)
    }
    ref_correct <- formula_matrix_t[formula_matrix_t[,model]==ref_mod, "correct_test"]
    if(ref_correct == "no"){
        warning("Reference model is not correctly adjusted. This is not recommended!")
    }



    ## Add MAS
    data.table::setDT(formula_matrix)
    formula_matrix_t <- data.table::copy(formula_matrix)
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    mas <- lapply(formula_matrix_t$formula, add_mas, additional_args)

    ## Extract reference and comparison MAS
    ref_adj <- mas[[ref_mod]]
    cmp_adj <- mas[-ref_mod]
    for (i in seq_along(ref_adj)){
        ref_adj[[i]] <- gsub("\\{|\\}|\\s", "", ref_adj[[i]])
    }
    for (i in seq_along(cmp_adj)){
        cmp_adj[[i]] <- gsub("\\{|\\}|\\s", "",cmp_adj[[i]])
    }

    ## Compare
    ls_cmp <- list()
    ctr <- 1
    for (i in seq_along(cmp_adj)){
        if (ref_mod==ctr){
            ctr = ctr+1
        }
        identical <-  sapply(ref_adj, function(x) any(x %in% cmp_adj[[i]]))
        if(any(isTRUE(identical))){
            ls_temp <- list(list(cat="compatible", model=ctr))
            ls_cmp <- append(ls_cmp, ls_temp)
        } else {
            ls_temp <- list(list(cat="incompatible", model=ctr))
            ls_cmp <- append(ls_cmp, ls_temp)
        }
        ctr <- ctr+1
    }

    ## Add back in
    formula_ref <- formula_matrix_t[ref_mod]
    formula_ref$test_compatible <- "reference model"
    formula_cmp <- formula_matrix_t[-ref_mod]
    formula_cmp$test_compatible <- unlist(lapply(ls_cmp, function(sublist) sublist$cat))
    cmp_matrix <- rbind(formula_ref, formula_cmp)
    cmp_matrix$unq_nodes <- lapply(cmp_matrix$formula, unq_nodes_detect,additional_args)
    ref_unq_nodes <- unlist(cmp_matrix[test_compatible=="reference model", "unq_nodes"])
    ref_correct_test <- unlist(cmp_matrix[test_compatible=="reference model", "correct_test"])
    cmp_matrix <- cmp_matrix %>%
        tibble::as_tibble() %>%
        dplyr::arrange(model) %>%
        dplyr::mutate(
            full_model_compatible = dplyr::case_when(
                test_compatible == "reference model" ~ "reference model",
                test_compatible != "reference model" &
                    unq_nodes  == ref_unq_nodes  &
                    correct_test == ref_correct_test ~ "compatible",
                TRUE ~ "incompatible"
            )
        )
    data.table::setDT(cmp_matrix)

    return(cmp_matrix)
}



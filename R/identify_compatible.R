library(ggdag)
library(dagitty)

identify_compatible <- function(formula_matrix, causal_matrix, effect="direct",
                                ref_mod=NULL){
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    if (is.null(effect)){
        effect = "direct"
        warning("Effect not given, use default (direct) instead.")
    }
    formulas <- formula_matrix$formula
    adj_ls <- list()
    for (i in seq_along(formulas)){
        formulas_vector <- strsplit(formulas[i], ",")[[1]]
        dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
        adjsets <- adjustmentSets(dag, "Xtest","Y",type = "minimal", effect=effect)
        names(adjsets) <- NULL
        adj_ls <- append(adj_ls, list(adjsets))
    }
    adj_ls <- lapply(adj_ls, function(inner_list){
        c(inner_list)
    })

        if (is.null(ref_mod)){
        ref_mod = 1
        warning("User did not choose a reference model, default to 1")
    }

    ref_adj <- adj_ls[[ref_mod]]
    cmp_adj <- adj_ls[-1]
    ls_cmp <- list()
    print(length(cmp_adj))
    for (i in seq_along(cmp_adj)){
        identical <- lapply(i, function(x) identical(x, ref_adj))
        single_excl <-  lapply(i, function(x) any(ref_adj %in% x))
        if(any(identical==TRUE)){
            ls_temp <- list(list(cat="compatible", where=which(results==TRUE)))
            ls_cmp <- append(ls_cmp, ls_temp)
        } else {
            if(any(single_excl==TRUE)) {
                ls_temp <- list(list(cat="singularly exclusive", where=which(results==TRUE)))
                ls_singl <- append
            }
        }
        
    }
    return(ls_cmp)
}


test <- identify_compatible(reduced_formula_matrix, reduced_causal_matrix, effect="direct")
test2 <- identify_compatible(reduced_formula_matrix, reduced_causal_matrix, 
                             effect="direct", ref_mod=31)
cmp_causal_matrix <- identify_compatible(new_formula, new_causal)
cmp_formula_matrix  <- build_formula_matrix(new_causal) 


additional_args <- list(
    exposure="Xtest",
    outcome="Y"
)
formulas_vector <- strsplit(unlist(reduced_formula_matrix[67,1]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))


formulas_vector <- strsplit(unlist(reduced_formula_matrix[12,1]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))

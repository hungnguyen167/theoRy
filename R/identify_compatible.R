library(ggdag)
library(dagitty)

identify_compatible <- function(formula_matrix, causal_matrix, effect=NULL){
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
    
    
    return(adj_ls)
}
test <- identify_compatible(reduced_formula_matrix, reduced_causal_matrix, effect="direct")

cmp_causal_matrix <- identify_compatible(new_formula, new_causal)
cmp_formula_matrix  <- build_formula_matrix(new_causal) 

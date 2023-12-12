library(ggdag)
library(dagitty)



ident_redunt <- function(formula_matrix, causal_matrix, effect="direct"){
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    formulas <- formula_matrix$formula
    short_causal_matrix <- tibble()
    for (i in seq_along(formulas)){
        formulas_vector <- strsplit(formulas[i], ",")[[1]]
        dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
        adjsets <- unlist(adjustmentSets(dag, "Xtest","Y",effect="direct"))
        df_temp <- causal_matrix %>%
            filter(model==i) %>%
            filter(from %in% c(adjsets,"Xtest") 
                   | ((direction == "~~") & (!from %in% c(adjsets, "Xtest")))
                   )
        
        short_causal_matrix <- bind_rows(short_causal_matrix, df_temp)
        
        #short_formula_matrix <- build_formula_matrix2(short_causal_matrix)
    }
    return(short_causal_matrix)
}
new_causal <- ident_redunt(formula_matrix, causal_matrix)
new_formula <- build_formula_matrix(new_causal) 

additional_args <- list(
    exposure="Xtest",
    outcome="Y"
)
formulas_vector <- strsplit(unlist(new_formula[5,1]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))


formulas_vector <- strsplit(unlist(formula_matrix[5,1]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))

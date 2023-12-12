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
            filter((from %in% adjsets & to %in% adjsets) | to == "Y" | direction == "~~")
        if (!is.null(df_temp)){
            short_causal_matrix<- bind_rows(short_causal_matrix, df_temp)
        }
        
        #short_formula_matrix <- build_formula_matrix2(short_causal_matrix)
    }
    return(short_causal_matrix)
}
test <- ident_redunt(formula_matrix, causal_matrix)
test2 <- build_formula_matrix2(test) 
    

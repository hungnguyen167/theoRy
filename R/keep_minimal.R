require(ggdag)
require(dagitty)



keep_minimal <- function(formula_matrix, causal_matrix, effect="direct"){
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    formulas <- formula_matrix$formula
    adj_ls <- list()
    for (i in seq_along(formulas)){
        formulas_vector <- strsplit(formulas[i], ",")[[1]]
        dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
        adjsets <- unique(unlist(adjustmentSets(dag, "Xtest","Y",type="minimal",effect="direct"),
                          use.names = FALSE))
        names(adjsets) <- NULL
        adj_ls <- append(adj_ls, list(adjsets))
        
    }
    adj_ls <- lapply(adj_ls, function(inner_list){
        c(inner_list, "Xtest","Y")
    })
    reduced_causal_matrix <- tibble()
    for (i in 1:length(adj_ls)){
        df_temp <- causal_matrix %>% 
            filter(model == i) %>%
            filter(from %in% adj_ls[[i]] & to %in% adj_ls[[i]])
        reduced_causal_matrix <- bind_rows(reduced_causal_matrix, df_temp)
    }

    return(reduced_causal_matrix)
}
#reduced_causal_matrix <- keep_minimal(formula_matrix, causal_matrix)
#reduced_formula_matrix <- build_formula_matrix(reduced_causal_matrix) 



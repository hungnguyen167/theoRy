library(tidyverse)
library(ggdag)
library(dagitty)
build_formula_matrix <- function(causal_matrix){
    
    create_formula <- function(nested_df){
        causal_corr <- nested_df %>%
            group_by(to) %>%
            reframe(from=from,
                    formula = paste(to, direction, paste(from, collapse = " + "), sep=" ")) %>%
            distinct(formula, .keep_all=TRUE) %>%
            mutate(
                ord = case_when(
                    str_detect(to, "Y.*") ~ 1,
                    str_detect(to, "M.*") ~ 2,
                    str_detect(to, "X.*") ~ 3,
                    TRUE ~ 0
                ),
                pair = paste(pmin(to, from), pmax(to, from), sep="_")
            ) %>%
            filter(!str_detect(formula, "none")) %>%
            arrange(ord) %>%
            distinct(pair, .keep_all = TRUE) 

        formulas <- causal_corr$formula
        formula <- unlist(paste(formulas, collapse = ","))
        return(formula)
        
    }
    
    formula_matrix <- causal_matrix %>%
        group_by(model) %>%
        nest() %>%
        mutate(
            formula = map(data, create_formula)
        ) %>% 
        filter(str_detect(formula, "Y")) %>%
        select(-data)
    
    
    return(formula_matrix)
}


formula_matrix <- build_formula_matrix(causal_matrix)


## Graphing
additional_args <- list(
    exposure="X1",
    outcome="Y"
)

formulas_vector <- strsplit(unlist(formula_matrix[1,2]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))

formulas_vector <- strsplit(unlist(formula_matrix[122,2]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))


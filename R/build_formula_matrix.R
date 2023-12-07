library(tidyverse)
library(ggdag)
library(dagitty)
library(furrr)
library(progressr)
build_formula_matrix <- function(causal_matrix){
    # Check type
    # Start of function
    create_formula <- function(nested_df){
        formula_df <- nested_df %>%
            filter(direction %in% c("~","~~")) %>%
            group_by(model,to) %>%
            reframe(from=from,
                    formula = ifelse(direction == "~", ## if causal,
                                      paste(to, direction, paste(from, collapse = " + "), sep=" "), ## add to one formula
                                      paste(from, direction, to, collapse=", "))) %>% ## if correlational, separate
            mutate(
                ord = case_when(
                    str_detect(to, "Y.*") ~ 1,
                    str_detect(to, "M.*") ~ 2,
                    str_detect(to, "X.*") ~ 3,
                    TRUE ~ 0
                )
            ) %>%
            distinct(formula, .keep_all = TRUE) %>%
            arrange(ord) %>%
            ungroup()


        
        formulas <- formula_df$formula
        formula <- unlist(paste(formulas, collapse = ", "))
        return(formula)
        
        
    }
    
    formula_matrix <- causal_matrix %>%
        group_by(model) %>%
        group_split() %>%
        future_map(~ summarise(.x, formula = create_formula(.x))) %>%
        bind_rows() %>%
        mutate(
            model = row_number()
        ) 
    
    
    return(formula_matrix)
}


tic()
formula_matrix <-build_formula_matrix(causal_matrix)
toc()


## Graphing
additional_args <- list(
    exposure="X1",
    outcome="Y"
)

formulas_vector <- strsplit(unlist(formula_matrix[2,1]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag, "Y")
formulas_vector <- strsplit(unlist(formula_matrix[40,1]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag2, "Y")




## Unused code: filter(str_detect(formula, "M\\d+\\s~[^,]+X\\d+") | !str_detect(formula, "M\\d+")) %>% 

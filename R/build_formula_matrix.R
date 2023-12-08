library(tidyverse)
library(ggdag)
library(dagitty)
library(furrr)
library(progressr)
library(tictoc)
build_formula_matrix <- function(causal_matrix){
    # Check type
    # Start of function
    create_formula <- function(nested_df){
        causal_df <- nested_df %>% 
            filter(direction == "~") %>%
            group_by(model, to) %>%
            reframe(formula =  paste(to, direction, paste(from, collapse = " + "), sep=" "))%>% ## if correlational, separate
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
        formula_df <- nested_df %>%
            filter(direction == "~~") %>%
            group_by(model, to) %>%
            reframe(formula = paste(to, direction, from, sep = " ")) %>%
            bind_rows(causal_df)

        
        formulas <- formula_df$formula
        formula <- unlist(paste(formulas, collapse = ", "))
        return(formula)
        
        
    }
    
    formula_matrix <- causal_matrix %>%
        group_by(model) %>%
        group_split() %>%
        future_map(~ summarise(.x, formula = create_formula(.x)), 
                   .options = furrr_options(scheduling = 2)) %>%
        bind_rows() %>%
        mutate(
            model = row_number()
        ) 
    
    
    return(formula_matrix)
}
plan(multisession, workers=2)

tic()
formula_matrix <-build_formula_matrix(causal_matrix)
toc()


## Graphing
additional_args <- list(
    exposure="Xtest",
    outcome="Y"
)

formulas_vector <- strsplit(unlist(formula_matrix[2,1]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag, "Y")
formulas_vector <- strsplit(unlist(formula_matrix[40,1]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag2, "Y")




## Unused code: filter(str_detect(formula, "M\\d+\\s~[^,]+X\\d+") | !str_detect(formula, "M\\d+")) %>% 

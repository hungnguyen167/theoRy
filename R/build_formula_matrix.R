library(tidyverse)
library(ggdag)
library(dagitty)
library(furrr)
library(data.table)
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
                    str_detect(to, "X.*") ~ 2
                    )
            ) %>%
            distinct(formula, .keep_all = TRUE) %>%
            arrange(ord) %>%
            ungroup()
        formula_df <- nested_df %>%
            filter(direction == "~~") %>%
            group_by(model, to) %>%
            reframe(formula = paste(to, direction, from, sep = " ")) 
        formula_df <- bind_rows(causal_df, formula_df)

        
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



tic()
formula_matrix <-build_formula_matrix(causal_matrix)
toc()


create_formula <- function(nested_dt) {
    causal_dt <- nested_dt[direction == "~"]
    causal_dt[, formula := paste(to, direction, paste(from, collapse = " + "), sep=" "), by = .(model, to)]
    causal_dt[, ord := fifelse(str_detect(to, "Y.*"), 1,
                               fifelse(str_detect(to, "M.*"), 2,
                                       fifelse(str_detect(to, "X.*"), 3, 0)))]
    causal_dt <- unique(causal_dt, by = "formula")
    setorder(causal_dt, ord)
    
    formula_dt <- nested_dt[direction == "~~"]
    formula_dt[, formula := paste(to, direction, from, sep = " "), by = .(model, to)]
    formula_dt <- rbindlist(list(causal_dt, formula_dt), use.names = TRUE, fill=TRUE)
    
    formulas <- formula_dt$formula
    formula <- paste(formulas, collapse = ", ")
    return(formula)
}
build_formula_matrix2 <- function(causal_matrix) {
    setDT(causal_matrix) # Convert to data.table if not alread
    
    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix, by = "model"), create_formula)
    
    # Combine results into a data.table
    formula_matrix <- unique(data.table(formula = unlist(formula_list),model=1:length(formula_list)), 
                             by="formula")
    
    
    # Assign model numbers (if needed)
    return(formula_matrix)
}

tic()
formula_matrix2 <-build_formula_matrix2(causal_matrix)
toc()





## Graphing
additional_args <- list(
    exposure="Xtest",
    outcome="Y"
)

formulas_vector <- strsplit(unlist(formula_matrix[2,1]), ",")[[1]]
dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag, "Y")
formulas_vector <- strsplit(unlist(formula_matrix[256,1]), ",")[[1]]
dag2 <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
ggdag_parents(dag2, "Y")





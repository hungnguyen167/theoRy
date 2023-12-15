require(tidyverse)
require(data.table)
require(tictoc)


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
build_formula_matrix <- function(causal_matrix) {
    setDT(causal_matrix) # Convert to data.table if not alread
    
    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix, by = "model"), create_formula)
    
    # Combine results into a data.table
    formula_matrix <- unique(data.table(formula = unlist(formula_list),model=1:length(formula_list)), 
                             by="formula")
    
    
    # Assign model numbers (if needed)
    return(formula_matrix)
}


#tic()
#formula_matrix <-build_formula_matrix(causal_matrix)
#toc()










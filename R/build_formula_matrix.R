require(tidyverse)
require(data.table)
require(dagitty)
require(ggdag)


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
add_mas <- function(row, additional_args) {
    fvector <- strsplit(as.character(row[1]), ",")[[1]]
    dag_args <- lapply(fvector, as.formula)
    dag <- do.call(dagify, c(dag_args, additional_args))
    mas <- adjustmentSets(dag, exposure = "Xtest", outcome = "Y", effect = "direct")
    if (length(unlist(mas)) == 0) {
        return("none")
    } 
    else {
        mas_output <- capture.output(mas)
        return(paste(mas_output, collapse = ","))
    }

}

build_formula_matrix <- function(causal_matrix) {
    setDT(causal_matrix) # Convert to data.table if not already
    
    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix, by = "model"), create_formula)

    formula_matrix <- data.table(formula = unlist(formula_list), model =1:as.numeric(length(formula_list)))
    
    # this makes sure that original model numbers are kept
    formula_matrix <- unique(formula_matrix, 
                             by="formula")
    message("Finished creating formulas. Now adding MAS.")
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    mas <- lapply(formula_matrix$formula, add_mas, additional_args)
    
    formula_matrix$mas <- mas
    formula_matrix$model <- as.numeric(formula_matrix$model)
    
    return(formula_matrix)
}

message("function build_formula_matrix loaded")


#tic()
formula_matrix <-build_formula_matrix(causal_matrix)
#toc()










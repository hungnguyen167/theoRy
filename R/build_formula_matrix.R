require(tidyverse)
require(data.table)
require(dagitty)
require(ggdag)


create_formula <- function(nested_dt) {
    causal_dt <- nested_dt[direction == "->"]
    causal_dt[, direction:= "~"]
    causal_dt[, formula := paste(to, direction, paste(from, collapse = " + "), sep=" "), by = .(model, to)]
    causal_dt[, ord := fifelse(str_detect(to, "Y.*"), 1,
                               fifelse(str_detect(to, "M.*"), 2,
                                       fifelse(str_detect(to, "X.*"), 3, 0)))]
    causal_dt <- unique(causal_dt, by = "formula")
    setorder(causal_dt, ord)
    
    formula_dt <- nested_dt[direction == "<->"]
    formula_dt[, direction:= "~~"]
    formula_dt[, formula := paste(to, direction, from, sep = " "), by = .(model, to)]
    
    
    
    formula_dt <- rbindlist(list(causal_dt, formula_dt), use.names = TRUE, fill=TRUE)
    
    
    
    formulas <- formula_dt$formula
    formula <- paste(formulas, collapse = ", ")
    return(formula)
}
add_mas <- function(row, additional_args, adjusted=FALSE, unq_Xs=NULL, return_string=FALSE) {
    fvector <- strsplit(as.character(row[1]), ",")[[1]]
    dag_args <- lapply(fvector, as.formula)
    dag <- do.call(dagify, c(dag_args, additional_args))
    
    if (adjusted==TRUE){
        unq_nodes <- unique(c(edges(dag)$v, edges(dag)$w))
        unq_Xs <- unq_nodes[!unq_nodes %in% c("Y","Xtest")]
        adjustedNodes(dag) <- unq_Xs
        mas <- adjustmentSets(dag, exposure = "Xtest", outcome = "Y", effect = "direct")
        if (length(mas)==0) {
            return("no")
        } 
        else{
            return("yes")
        }
    }
    
    else {
        
        mas <- adjustmentSets(dag, exposure = "Xtest", outcome = "Y", effect = "direct")
        if (length(unlist(mas)) == 0) {
            return("none")
        } 
        else{
            mas_output <- capture.output(mas)
            if(return_string){
                return(paste(mas_output, collapse = ","))
            }
            else{
                return(mas_output)
            }
        }
    }
    
}

build_formula_matrix <- function(causal_matrix) {
    setDT(causal_matrix) # Convert to data.table if not already
    
    # Use lapply to apply the function to each group
    formula_list <- lapply(split(causal_matrix, by = "model"), create_formula)

    formula_matrix <- data.table(formula = unlist(formula_list), model = unique(causal_matrix$model))
    reduced_matrix <- unique(causal_matrix[, .SD, .SDcols = c("model", "user_mod")])
    formula_matrix <- formula_matrix[reduced_matrix, on = "model", nomatch = 0]
    
    # this makes sure that original model numbers are kept
    formula_matrix <- unique(formula_matrix, 
                             by="formula")
    message("Finished creating formulas. Now adding MAS.")
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    mas <- lapply(formula_matrix$formula, add_mas, additional_args, return_string=FALSE)
    
    formula_matrix$mas <- mas
    
    mas_adj <- lapply(formula_matrix$formula, add_mas, additional_args, adjusted=TRUE, unq_Xs=all_unique_Xs)
    formula_matrix$correct_test <- mas_adj
    
    return(formula_matrix)
}

message("function build_formula_matrix loaded")


#tic()
#formula_matrix <-build_formula_matrix(ls_theory$causal_matrix)
#toc()










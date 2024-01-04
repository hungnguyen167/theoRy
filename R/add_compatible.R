library(ggdag)
library(dagitty)
require(data.table)
add_mas <- function(row, additional_args, adjusted=FALSE, unq_Xs=NULL) {
    fvector <- strsplit(as.character(row[1]), ",")[[1]]
    dag_args <- lapply(fvector, as.formula)
    dag <- do.call(dagify, c(dag_args, additional_args))
    
    if (adjusted==TRUE){
        unq_Xs <- unique(c(edges(dag)$v, edges(dag)$w))
        unq_Xs <- unq_Xs[!unq_Xs %in% c("Y","Xtest")]
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
            return(paste(mas_output, collapse = ","))
        }
    }
    
}

add_compatible <- function(formula_matrix, effect="direct",
                                ref_mod=NULL){
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    if (is.null(effect)){
        effect = "direct"
        warning("Effect not given, use default (direct) instead.")
    }
    mas <- lapply(formula_matrix$formula, add_mas, additional_args)


    if (is.null(ref_mod)){
        ref_mod = 1
        warning("User did not choose a reference model, default to the first model")
    }
    else {
        ref_mod = which(formula_matrix[,model]==ref_mod)
    }
    ref_adj <- mas[[ref_mod]]
    cmp_adj <- mas[-ref_mod]
    ls_cmp <- list()
    ctr <- 1
    for (i in seq_along(cmp_adj)){
        if (ref_mod==ctr){
            ctr = ctr+1
        }
        identical <- lapply(cmp_adj[[i]], function(x) any(identical(x, unlist(ref_adj))))
        if(any(identical==TRUE)){
            ls_temp <- list(list(cat="compatible", model=ctr))
            ls_cmp <- append(ls_cmp, ls_temp)
        } else {
            ls_temp <- list(list(cat="incompatible", model=ctr))
            ls_cmp <- append(ls_cmp, ls_temp)
        }
        ctr <- ctr+1
    }
    formula_ref <- formula_matrix[ref_mod]
    formula_ref$test_compatible <- "reference model"
    formula_cmp <- formula_matrix[-ref_mod]
    formula_cmp$test_compatible <- unlist(lapply(ls_cmp, function(sublist) sublist$cat))
    
    cmp_matrix <- rbind(formula_ref, formula_cmp)
    cmp_matrix <- cmp_matrix %>%
        as_tibble() %>%
        arrange(model) %>%
        mutate(
            full_model_compatible = case_when(
                test_compatible == "reference model" ~ "reference model",
                test_compatible  == "compatible"  & correct_test == "yes" ~ "compatible",
                TRUE ~ "incompatible"
            )
        ) 
    setDT(cmp_matrix)
    return(cmp_matrix)
}

message("function add_compatible loaded")
#cmp_matrix <- test_compatible(formula_matrix, ref_mod = 10)



#formulas_vector <- strsplit(unlist(formula_matrix[9,1]), ",")[[1]]
#dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))



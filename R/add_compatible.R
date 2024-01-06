library(ggdag)
library(dagitty)
require(data.table)

unq_nodes_detect <- function(row, additional_args){
    fvector <- strsplit(as.character(row[1]), ",")[[1]]
    dag_args <- lapply(fvector, as.formula)
    dag <- do.call(dagify, c(dag_args, additional_args))
    unq_nodes <- paste(unique(c(edges(dag)$v, edges(dag)$w)), collapse="_")
    return(unq_nodes)
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
    for (i in seq_along(ref_adj)){
        ref_adj[[i]] <- gsub("\\{|\\}|\\s", "", ref_adj[[i]])
    }
    for (i in seq_along(cmp_adj)){
        cmp_adj[[i]] <- gsub("\\{|\\}|\\s", "",cmp_adj[[i]])
    }
    ls_cmp <- list()
    ctr <- 1
    for (i in seq_along(cmp_adj)){
        if (ref_mod==ctr){
            ctr = ctr+1
        }
        identical <-  sapply(ref_adj, function(x) any(x %in% cmp_adj[[i]]))
        if(any(isTRUE(identical))){
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
    cmp_matrix$unq_nodes <- lapply(cmp_matrix$formula, unq_nodes_detect,additional_args)
    ref_unq_nodes <- unlist(cmp_matrix[test_compatible=="reference model", "unq_nodes"])
    cmp_matrix <- cmp_matrix %>%
        as_tibble() %>%
        arrange(model) %>%
        mutate(
            full_model_compatible = case_when(
                test_compatible == "reference model" ~ "reference model",
                test_compatible != "reference model" & 
                    unq_nodes  == ref_unq_nodes  & correct_test == "yes" ~ "compatible",
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



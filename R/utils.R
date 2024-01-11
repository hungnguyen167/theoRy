match_base <- function(nested_dt, nested_base) {
    temp <- data.table::copy(nested_dt)
    temp <- temp[, .(from, to, direction)]
    data.table::setorder(temp, from, to, direction)
    ident <- data.table::fifelse(identical(temp, nested_base),1,0)
    return(ident)
}
remove_redundant <- function(nested_dt){
    unique_from <- nested_dt[, .(UniqueValues = names(table(from)[table(from) == 1]))]
    unique_to <- nested_dt[, .(UniqueValues = names(table(to)[table(to) == 1]))]
    common_unq <- data.table::fintersect(unique_from, unique_to)
    common_unq <- unlist(unique(common_unq))
    nested_dt <- nested_dt[!(from == to & !(from %in% common_unq))]
    return(nested_dt)

}


dt_to_string <- function(dt) {
    data.table::setorder(dt, from, to)
    dt[,model:=NULL]
    return(dt)
}

dt_to_hash <- function(dt){
    dt[,group_hash := digest::digest(.SD, algo = "md5"), by = model]
    return(unique(dt$group_hash))
}

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

create_formula <- function(nested_dt) {
    setorder(nested_dt, from, to, direction)
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


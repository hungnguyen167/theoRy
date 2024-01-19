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
    dag <- do.call(ggdag::dagify, c(dag_args, additional_args))
    unq_nodes <- paste(sort(unique(c(dagitty::edges(dag)$v, dagitty::edges(dag)$w))), collapse="_")
    return(unq_nodes)
}

add_mas <- function(row, additional_args, adjusted=FALSE, unq_Xs=NULL, return_string=FALSE) {
    fvector <- strsplit(as.character(row[1]), ",")[[1]]
    dag_args <- lapply(fvector, as.formula)
    dag <- do.call(ggdag::dagify, c(dag_args, additional_args))

    if (adjusted==TRUE){
        unq_nodes <- unique(c(dagitty::edges(dag)$v, dagitty::edges(dag)$w))
        unq_Xs <- unq_nodes[!unq_nodes %in% c("Y","Xtest")]
        dagitty::adjustedNodes(dag) <- unq_Xs
        mas <- dagitty::adjustmentSets(dag, exposure = "Xtest", outcome = "Y", effect = "direct")
        if (length(mas)==0) {
            return("no")
        }
        else{
            return("yes")
        }
    }

    else {

        mas <- dagitty::adjustmentSets(dag, exposure = "Xtest", outcome = "Y", effect = "direct")
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
    data.table::setorder(nested_dt, from, to, direction)
    causal_dt <- nested_dt[direction == "->"]
    causal_dt[, direction:= "~"]
    causal_dt[, formula := paste(to, direction, paste(from, collapse = " + "), sep=" "), by = .(model, to)]
    causal_dt[, ord := data.table::fifelse(stringr::str_detect(to, "Y.*"), 1,
                               data.table::fifelse(stringr::str_detect(to, "M.*"), 2,
                                       data.table::fifelse(stringr::str_detect(to, "X.*"), 3, 0)))]
    causal_dt <- unique(causal_dt, by = "formula")
    data.table::setorder(causal_dt, ord)

    formula_dt <- nested_dt[direction == "<->"]
    formula_dt[, direction:= "~~"]
    formula_dt[, formula := paste(to, direction, from, sep = " "), by = .(model, to)]



    formula_dt <- data.table::rbindlist(list(causal_dt, formula_dt), use.names = TRUE, fill=TRUE)



    formulas <- formula_dt$formula
    formula <- paste(formulas, collapse = ", ")
    return(formula)
}

build_plot_info <- function(ls_info) {
    formula_matrix <- data.table::copy(ls_info$formula_matrix)
    node_timing <- data.table::copy(ls_info$node_timing)
    data.table::setorder(formula_matrix, model)
    node_timing <- node_timing %>%
        tibble::as_tibble() %>%
        dplyr::mutate(
            timing = as.numeric(timing)
        ) %>%
        dplyr::arrange(dplyr::desc(timing))

    ## x_coords
    x_coords <- list()
    pl <- round(2/(length(unique(node_timing$timing))-1),3)
    for (i in seq_along(node_timing$timing)){
        if(i==1){
            x_coords[[i]] <- 1
        } else {
            if(node_timing$timing[i] == node_timing$timing[i-1]){
                x_coords[[i]] <- x_coords[[i-1]]
            } else{
                x_coords[[i]] <- x_coords[[i-1]] - pl
            }
        }
    }
    x_coords <- unlist(x_coords)
    names(x_coords) <- node_timing$node_name


    ## y_coords
    y_coords <- list()
    buffer_y <- 2
    previous_timing <- Inf
    last_idx <- 1
    for (i in seq_along(node_timing$timing)){
        noXtestY <- node_timing[which(!node_timing$node_name %in% c("Xtest","Y")),]
        if(node_timing$node_name[i] %in% c("Xtest","Y")){
            y_coords[[i]] <- 0

        } else{
            if(node_timing$timing[i] == max(noXtestY$timing)){
                if(node_timing$timing[i] == previous_timing){
                    y_coords[[i]] <- y_coords[[i-1]] - buffer_y
                }
                else{
                    y_coords[[i]] <- 0.5
                    last_idx <- i
                }
            }
            else {
                if(node_timing$timing[i] == previous_timing){
                    y_coords[[i]] <- y_coords[[i-1]] - buffer_y
                }
                else{
                    y_coords[[i]] <- y_coords[[last_idx]] + 0.25
                    last_idx <- i
                }
            }
            previous_timing <- node_timing$timing[i]
        }

    }
    y_coords <- unlist(y_coords)
    names(y_coords) <- node_timing$node_name



    crds <- list(x = x_coords,
                 y = y_coords)

    additional_args <- list(
        exposure="Xtest",
        outcome="Y",
        coords = crds
    )
    dag_matrix <- list()
    for (f in 1:nrow(formula_matrix)) {
        fvector <- strsplit(unlist(formula_matrix[f,1]), ",")[[1]]
        # create dag object syntax
        dag <- do.call(ggdag::dagify, c(lapply(fvector, as.formula), additional_args))
        # extract adjustment sets
        model <- formula_matrix$model[f]
        dag_matrix[[f]] <- dag
    }
    minX <- min(x_coords)
    maxX <- max(x_coords)
    minY <- min(y_coords)
    maxY <- max(y_coords)
    plot_info <- list(minX=minX, maxX=maxX, minY=minY, maxY=maxY, dag_matrix=dag_matrix)

    return(plot_info)
}



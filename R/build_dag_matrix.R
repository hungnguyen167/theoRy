require(tidyverse)
require(data.table)

build_dag_matrix <- function(ls_theory) { 
    formula_matrix <- ls_theory$formula_matrix
    node_timing <- ls_theory$node_timing
    setDT(formula_matrix)
    setorder(formula_matrix, model)
    node_timing <- node_timing %>%
        as_tibble() %>%
        mutate(
            timing = as.numeric(timing)
        ) %>%
        arrange(desc(timing)) 
    
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
                    y_coords[[i]] <- y_coords[[i-1]] + buffer_y
                } 
                else{
                    y_coords[[i]] <- -0.5
                    last_idx <- i
                }
            }
            else {
                if(node_timing$timing[i] == previous_timing){
                    y_coords[[i]] <- y_coords[[i-1]] + buffer_y
                } 
                else{
                    y_coords[[i]] <- y_coords[[last_idx]] - 0.25
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
        dag <- do.call(dagify, c(lapply(fvector, as.formula), additional_args))
        # extract adjustment sets
        model <- formula_matrix$model[f]
        dag_matrix[[model]] <- dag
    }
   
    result <- list(minX=minX, dag_matrix=dag_matrix)
    
    return(result)
}


message("function build_dag_matrix loaded")

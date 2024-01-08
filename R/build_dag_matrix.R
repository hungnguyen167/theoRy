require(tidyverse)


build_nodes <- function(inputs){ 
    # Generate plot coordinates from variable input
    node_timing <- tibble(var_name=inputs$nodes, timing=inputs$timing,
                          type=inputs$types) %>%
        arrange(timing) %>%
        group_by(type) %>%
        mutate(
            node_name = case_when(
                type == "otc" ~ "Y",
                type == "test" ~ "Xtest",
                type == "ctr" ~ paste0("X", row_number())
            )
        ) %>%
        ungroup()

    node_timing <- arrange(node_timing, -timing)
    return(node_timing)
}

build_dag_matrix <- function(formula_matrix,
                             add_nodes = 0,
                             new_node_vector1 = c("", -1.5, "ctr", "X3"),
                             new_node_vector2 = c("", 0.5, "ctr", "X4"),
                             new_node_vector3 = c("", -0.5, "ctr", "X5")) { # in case the user added formulas with extra variables, how many? integer
    
    dag_matrix <- list()
    
    #this should be arranged in case there were manual changes to the formula_matrix
    formula_matrix <- arrange(formula_matrix, model)

    # get node_timing
    node_timing <- as.data.frame(build_nodes(inputs))
    
    # this needs to be imporved to handle many nodes, this is a hack version so far
    if(add_nodes == 1) {
        node_timing[nrow(node_timing)+1,] <- new_node_vector1
    }
    
    if(add_nodes == 2) {
        node_timing[nrow(node_timing)+1,] <- new_node_vector1
        node_timing[nrow(node_timing)+1,] <- new_node_vector2
    }
    
    if(add_nodes == 3) {
        node_timing[nrow(node_timing)+1,] <- new_node_vector1
        node_timing[nrow(node_timing)+1,] <- new_node_vector2
        node_timing[nrow(node_timing)+1,] <- new_node_vector3}
    
    # setup plotting positions/coordinates
    pl <- round(2/(length(unique(node_timing$timing))-1),3)
    #needs to be df for transformations to work, and numeric
    node_timing <- as.data.frame(node_timing)
    node_timing$timing <- as.numeric(node_timing$timing)
    
    #make sure plot time is in order
    node_timing <- arrange(node_timing, desc(timing))
    
    node_timing$plot_time <- NA
    node_timing$plot_time[1] <- 1
    node_timing$plot_time[2] <- ifelse(node_timing$timing[1] == node_timing$timing[2],
                                       1, 1-pl)
    # check that there are not three simultaneous variables (this affects plotting)
    simult <- FALSE
    
    if (length(node_timing$timing) == 5) {
    simult <- ifelse(length(unique(node_timing$timing[node_timing$var_name != "Y" & node_timing$var_name != "Xtest"]) == 1),
                     TRUE,
                     FALSE)
    }
    
    if(length(node_timing$node_name) == 6) {
        node_timing$plot_time[3] <- ifelse(node_timing$timing[2] == node_timing$timing[3],
                                           node_timing$plot_time[2], node_timing$plot_time[2]-pl)
        node_timing$plot_time[4] <- ifelse(node_timing$timing[3] == node_timing$timing[4],
                                           node_timing$plot_time[3], node_timing$plot_time[3]-pl)
        node_timing$plot_time[5] <- ifelse(node_timing$timing[4] == node_timing$timing[5],
                                           node_timing$plot_time[4], node_timing$plot_time[4]-pl)
        node_timing$plot_time[6] <- ifelse(node_timing$timing[5] == node_timing$timing[6],
                                           node_timing$plot_time[5], node_timing$plot_time[5]-pl)
    }
    
    if(length(node_timing$node_name) == 5) {
        node_timing$plot_time[3] <- ifelse(node_timing$timing[2] == node_timing$timing[3],
                                           node_timing$plot_time[2], node_timing$plot_time[2]-pl)
        node_timing$plot_time[4] <- ifelse(node_timing$timing[3] == node_timing$timing[4],
                                           node_timing$plot_time[3], node_timing$plot_time[3]-pl)
        node_timing$plot_time[5] <- ifelse(node_timing$timing[4] == node_timing$timing[5],
                                           node_timing$plot_time[4], node_timing$plot_time[4]-pl)
    }
    
    if(length(node_timing$node_name) == 4) {
        node_timing$plot_time[3] <- ifelse(node_timing$timing[2] == node_timing$timing[3],
                                           node_timing$plot_time[2], node_timing$plot_time[2]-pl)
        node_timing$plot_time[4] <- ifelse(node_timing$timing[3] == node_timing$timing[4],
                                           node_timing$plot_time[3], node_timing$plot_time[3]-pl)
        node_timing[5,] <- NA
        node_timing$node_name[5] <- "X5"
        node_timing$plot_time[5] <- min(node_timing$plot_time, na.rm = T)
    }
    
    if(length(node_timing$node_name) == 3) {
        node_timing$plot_time[3] <- ifelse(node_timing$timing[2] == node_timing$timing[3],
                                           node_timing$plot_time[2], node_timing$plot_time[2]-pl)
        node_timing[4:5,] <- NA
        node_timing$node_name[4:5] <- c("X4, X5")
        node_timing$plot_time[4:5] <- c(min(node_timing$plot_time, na.rm = T), min(node_timing$plot_time, na.rm = T))
    }
    
    #defunkt
    py = -1
    if(simult){
        py = -0.5
    }
    
    # here we create the coordinates list, for variables that are not in the 
    # models they just appear as the minimum values but are not plotted
    
    node_timing <- node_timing %>%
        mutate(plot_time_y = ifelse(node_name == "Y", 0,
                             ifelse(node_name == "Xtest", 0,
                             ifelse(node_name == "X1", -1,
                             ifelse(node_name == "X2", 1,
                             ifelse(node_name == "X3" & timing > -1, 1,
                             ifelse(node_name == "X3" & timing == -1, 0.5,
                             ifelse(node_name == "X4" & timing >=0, -0.5,
                             ifelse(node_name == "X4" & timing < 0 & timing > -1, 1, 
                             ifelse(node_name == "X4" & timing == -1, -0.5, -0.75)))))))))
        )

    crds <- list(x = c(node_timing$plot_time[1],
                       node_timing$plot_time[2],
                       node_timing$plot_time[3],
                       node_timing$plot_time[4],
                       node_timing$plot_time[5],
                       node_timing$plot_time[6]), 
                 y = c(node_timing$plot_time_y[1],
                       node_timing$plot_time_y[2],
                       node_timing$plot_time_y[3],
                       node_timing$plot_time_y[4],
                       node_timing$plot_time_y[5],
                       node_timing$plot_time_y[6])
    )
    
    # for plotting later we need the minimal X position value
    minX <- as.data.frame(min(c(node_timing$plot_time[1],
                  node_timing$plot_time[2],
                  node_timing$plot_time[3],
                  node_timing$plot_time[4],
                  node_timing$plot_time[5],
                  node_timing$plot_time[6], na.rm = T)))
    colnames(minX) <- c("minX")
    write_rds(minX, here::here("Results", "minX.RDS"))
    
    c <- c(paste(node_timing$node_name[1]),
           paste(node_timing$node_name[2]),
           paste(node_timing$node_name[3]),
           paste(node_timing$node_name[4]),
           paste(node_timing$node_name[5]),
           paste(node_timing$node_name[6]))
    
    crds$x <- setNames(crds$x, c(c))
    crds$y <- setNames(crds$y, c(c))
    
    additional_args <- list(
        exposure="Xtest",
        outcome="Y",
        coords = crds
        )

    for (f in 1:nrow(formula_matrix)) {
        fvector <- strsplit(unlist(formula_matrix[f,1]), ",")[[1]]
        # create dag object syntax
        dag <- do.call(dagify, c(lapply(fvector, as.formula), additional_args))
        # extract adjustment sets
        model <- formula_matrix$model[f]
        dag_matrix[[model]] <- dag
    }
   
 

    return(dag_matrix)
}
        
message("function build_dag_matrix loaded")

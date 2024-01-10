require(tidyverse)
require(data.table)
require(ggplot2)
require(ragg)
build_plot_info <- function(ls_theory) {
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
        dag <- do.call(dagify, c(lapply(fvector, as.formula), additional_args))
        # extract adjustment sets
        model <- formula_matrix$model[f]
        dag_matrix[[model]] <- dag
    }
    minX <- min(x_coords)
    maxX <- max(x_coords)
    minY <- min(y_coords)
    maxY <- max(y_coords)
    plot_info <- list(minX=minX, maxX=maxX, minY=minY, maxY=maxY, dag_matrix=dag_matrix)

    return(plot_info)
}



plot_dag <- function(ls_theory,
                            choose_plots = "all", # if not all, must be a vector with model numbers
                            choose_mas ="all",
                            save_path=NULL) {

    formula_matrix <- copy(ls_theory$formula_matrix)
    plot_info <- build_plot_info(ls_theory)
    if(is.numeric(choose_plots)){
            plots <- as.numeric(formula_matrix$model[formula_matrix$model %in% choose_plots])
            cat("Plotting only models", paste(choose_plots, collapse=","),"\n")
    } else {
        if(choose_plots=="all") {
            plots <- as.vector(formula_matrix$model)
            cat("Plotting all model numbers\n")
        } else {
            stop("choose_plots must be 'all' or a vector of model numbers\n")
        }

    }



    mas <- formula_matrix$mas
    for (i in seq_along(mas)){
        mas[[i]] <- gsub("\\{|\\}|\\s", "",mas[[i]])
    }
    if(is.character(choose_mas) & length(choose_mas)==1){
        if(choose_mas=="all"){
            cat("Plotting models with any MAS\n")
            plots_mas <- 1:length(mas)
        }
        else {
            cat("Plotting only models with MAS:", choose_mas, "\n")
            plots_mas <- list()
            for(i in seq_along(mas)){
                have_mas <- any(mas[[i]] %in% choose_mas)
                plots_mas[[i]] <- ifelse(have_mas, i, 0)
            }
        }

    } else if(is.character(choose_mas) & length(choose_mas) > 1){
        cat("Plotting only models with MAS:", paste(choose_mas, collapse="or"), "\n")
        plots_mas <- list()
        for(i in seq_along(mas)){
            have_mas <- any(mas[[i]] %in% choose_mas)
            plots_mas[[i]] <- ifelse(have_mas, i, 0)
        }
    } else{
        stop("choose_mas must be either 'all' (default) or a vector of chosen MAS to plot\n")
    }

    plots_mas <- plots_mas[plots_mas != 0]
    # subset by choose_plots and/or choose_plots_mas
    formula_matrix <- formula_matrix[plots_mas, ]
    formula_matrix <- formula_matrix[formula_matrix$model %in% plots,]




    xlim <- c(plot_info$minX-0.25, plot_info$maxX+0.25)
    dist <- abs(plot_info$maxY) - abs(plot_info$minY)
    if(dist>=0){
        ylim <- c(plot_info$minY-0.25-dist, plot_info$maxY + 0.25)
    } else{
        ylim <- c(plot_info$minY-0.25, plot_info$maxY + 0.25+abs(dist))
    }
    dag_plots <- list()
    for (i in 1:nrow(formula_matrix)){
        mod = as.numeric(formula_matrix[i, "model"])
        plot <- plot_info$dag_matrix[[i]] %>%
            ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
            geom_dag_point(colour="white", na.rm=FALSE) +
            geom_dag_edges() +
            geom_dag_text(colour="black", na.rm=FALSE) +
            xlim(xlim)+
            ylim(ylim) +
            annotate("text", label = paste0("MAS = ", paste(mas[as.numeric(formula_matrix$model[formula_matrix$model == i])],
                                                      collapse="|")),
                     x=xlim[2]-0.75, y= ylim[1]+0.15) +
            annotate("text", label=paste0("Model ", formula_matrix$model[formula_matrix$model == i]),
                     x=xlim[1]+1.0, y = ylim[2] -0.15, size=7) +
            theme_dag()
        dag_plots[[i]] <- plot
        }

    if(!is.null(save_path)) {
        if(is.vector(choose_plots) & is.numeric(choose_plots)) {
            cat("Saving plots", paste(choose_plots, collapse=","), "to", save_path,"\n")
            for (i in seq_along(dag_plots)) {
                cat("Saving plot of model", i,"\n")
                model_name <- as.numeric(formula_matrix[i,"model"])
                agg_png(filename = paste0(save_path, "model_", model_name, ".png"), width = 2400, height = 1200, res=360)
                print(dag_plots[[i]])
                invisible(dev.off())
            }
        }
    }



    return(dag_plots)
}


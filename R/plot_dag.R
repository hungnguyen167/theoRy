#' Plot DAG models
#'
#' @description
#' `plot_dag` creates ggplot2-style plots from a ls_theory object.
#'
#' @details
#' This requires the ls_theory object, created from \code{\link{theoRy}} to work. Plot functions are inhereted from
#' the \code{\link[ggdag]} package.
#'
#'
#'
#' @param ls_theory the input ls_theory object. Created from \code{\link{theoRy}}.
#' @param choose_plots models to plot. Default to "all". However, this option can be resource-intensive if
#' the model universe is too large. It is recommended to choose certain models to compare against one another.
#' @param save_path path to save plots. Default to NULL (not saving)
#' @returns DAG plots of chosen DAG models from the ls_theory.
#' @examples
#' dag_plots <- plot_dag(ls_theory, choose_plots=c(1,2,3,4,5,6))
#' for (i in seq_along(dag_plots)){
#'     print(dag_plots[[i]])
#' }
#' @references TBA
#' @export


plot_dag <- function(ls_theory,
                     choose_plots = "all",
                     save_path=NULL) {

    formula_matrix <- data.table::copy(ls_theory$formula_matrix)
    node_timing <- data.table::copy(ls_theory$node_timing)
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

    formula_matrix <- formula_matrix[model %in% plots,]


    mas <- formula_matrix$mas
    for (i in seq_along(mas)){
        if(stringr::str_detect(mas[[i]], "\\},\\{")){
            temp <- unlist(strsplit(mas[[i]], "\\},\\{"))
            for(j in seq_along(temp)){
                mas[[i]][[j]] <- gsub("\\{|\\}|\\s", "", temp[[j]])
                mas[[i]][[j]] <- paste0("{",mas[[i]][[j]],"}")
            }
        } else{
            mas[[i]] <- gsub("\\{|\\}|\\s", "",mas[[i]])
            mas[[i]] <- paste0("{", mas[[i]], "}")
        }
    }

    ls_info <- list(formula_matrix=formula_matrix, node_timing=node_timing)
    plot_info <- build_plot_info(ls_info)


    xlim <- c(plot_info$minX-0.25, plot_info$maxX+0.25)
    dist <- abs(plot_info$maxY) - abs(plot_info$minY)
    if(dist>=0){
        ylim <- c(plot_info$minY-0.25-dist, plot_info$maxY + 0.25)
    } else{
        ylim <- c(plot_info$minY-0.25, plot_info$maxY + 0.25+abs(dist))
    }
    dag_plots <- list()
    for (i in 1:length(mas)){
        mod = as.numeric(formula_matrix[i, "model"])
        plot <- plot_info$dag_matrix[[i]] %>%
            ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
            ggdag::geom_dag_point(colour="white", na.rm=FALSE) +
            ggdag::geom_dag_edges() +
            ggdag::geom_dag_text(colour="black", na.rm=FALSE) +
            xlim(xlim)+
            ylim(ylim) +
            ggplot2::annotate("text", label = ifelse(length(mas)>1,
                                            paste0("MAS = ", paste0(mas[[i]],
                                                             collapse="|")),
                                            paste0("MAS = ", mas[[i]])),
                     x=xlim[2]-0.75, y= ylim[1]+0.15) +
            ggplot2::annotate("text", label=paste0("Model ", mod),
                     x=xlim[1]+1.0, y = ylim[2] -0.15, size=7) +
            ggdag::theme_dag()
        dag_plots[[i]] <- plot
        }

    if(!is.null(save_path)) {
        if(is.vector(choose_plots) & is.numeric(choose_plots)) {
            cat("Saving plots", paste(choose_plots, collapse=","), "to", save_path,"\n")
            for (i in seq_along(dag_plots)) {

                model_name <- as.numeric(formula_matrix[i,"model"])
                cat("Saving plot of model", model_name,"\n")
                ragg::agg_png(filename = paste0(save_path, "model_", model_name, ".png"), width = 2400, height = 1200, res=360)
                print(dag_plots[[i]])
                invisible(dev.off())
            }
        }
    }



    return(dag_plots)
}


require(tidyverse)

# TO DO
# need to find a way to add commas see NOTE 1

plot_dag_matrix <- function(dag_matrix,
                            choose_plots = "all", # if not all, must be a vector with model numbers
                            choose_plots_mas = "all", # must be either all or "{x}", where 'x' is the MAS
                            formula_matrix_name = formula_matrix, # in case using a different named formula matrix
                            save_plots = FALSE, # option to save plots as png
                            title = TRUE,
                            #suppress_plots = FALSE, # to view plots or not while they are plotting
                            title_pos = 0.5,
                            title_labels = formula_matrix_name$model,
                            plot_xlim = c(minX,1.1), 
                            plot_ylim = c(-1.1,1.5)) {
    
    
    
    # error if choose_plots not a numeric vector or all
    if(!is.vector(choose_plots)) {
        # start with all plots specified for selection
        plots <- as.vector(formula_matrix_name$model) 
        message("Plotting all model numbers")
    } else {
        # change plot specification for subsetting
        plots <- as.numeric(formula_matrix_name$model[formula_matrix_name$model %in% choose_plots])
        if(is.vector(choose_plots) & is.numeric(choose_plots)) {
            # NOTE 1: need to find a way to add commas between choose_plots numbers
            message(paste(c("Plotting only plots ", choose_plots)))
        }
        else {
            stop("choose_plots must be a numeric vector with model numbers")
        }
    }
    
    # check choose_plots_mas
    # right now we can only search for a single MAS, update to vector in future for multiple
    if(is.vector(choose_plots_mas)) {
        message("Not subsetting by MAS")
        # choose all
        plots_mas <- as.vector(formula_matrix_name$mas) 
    } else {
        # check that parentheses with spaces occur
        if(grepl("\\{ | \\}", choose_plots_mas)) {
            message(paste(c("Plotting only plots with unique MAS", choose_plots_mas)))
            #setup subset
            plots_mas <- choose_plots_mas
        } else {
            stop("choose_plots_mas needs to have brackets with spaces, exactly the way MAS appears in formula_matrix$mas. For example \\{ X1,X2 \\}")
        }
    }
    
    if(save_plots) {
        if(is.vector(choose_plots) & is.numeric(choose_plots)) {
            # NOTE 1: need to find a way to add commas between choose_plots numbers
            message(paste(c("Saving only plots ", choose_plots," to png files")))
        }
        else {
            stop("choose_plots must be a numeric vector with model numbers")
        }        
    }
    
    # subset by choose_plots and/or choose_plots_mas
    formula_matrix_name <- formula_matrix_name[formula_matrix_name$model %in% plots,]
    formula_matrix_name <- formula_matrix_name[formula_matrix_name$mas %in% plots_mas,]
    
    dag_plots <- list()
    
    # find lowest X position in dagitty object
    minX <- as.numeric(readRDS(here::here("Results", "minX.RDS"))[1,1])-0.1
    
    formula_matrix_name$title <- title_labels
    
    if(title) { 
        for (l in formula_matrix_name$model){
            plot(dag_matrix[[l]], xlim = plot_xlim, ylim = plot_ylim)
            text(0.5,-1, paste0("MAS = ",formula_matrix_name$mas[formula_matrix_name$model == l]))
            text(minX+title_pos,1.4, paste0("Model ", formula_matrix_name$title[formula_matrix_name$model == l]), font = 2)
            dag_plots <- append(dag_plots, list(recordPlot()))
            }
    } else {
        for (l in formula_matrix_name$model){
            plot(dag_matrix[[l]], xlim = plot_xlim, ylim = plot_ylim)
            text(0.5,-1, paste0("MAS = ",formula_matrix_name$mas[formula_matrix_name$model == l]))
            dag_plots <- append(dag_plots, list(recordPlot()))
        }
    }
    
        
    
    if(save_plots) {
        for (n in 1:length(formula_matrix_name$model)) {
            png(file = here::here("Results", paste0("model_", title_labels[n], ".png")), width = 480, height = 240)
            replayPlot(dag_plots[[n]])
            dev.off()
        }
    }
    
    return(dag_plots)
}
        
message("function plot_dag_matrix loaded")

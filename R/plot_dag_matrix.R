library(tidyverse)

# TO DO
# need to find a way to add commas see NOTE 1

plot_dag_matrix <- function(dag_matrix,
                            choose_plots = "all", # if not all, must be a vector with model numbers
                            choose_plots_mas = "all", # must be either all or "{x}", where 'x' is the MAS
                            formula_matrix_name = formula_matrix, # in case using a different named formula matrix
                            save_plots = FALSE, # option to save plots as png
                            choose_save_plots = NA, # if not NA, must be a vector with plot numbers
                            title = TRUE,
                            #suppress_plots = FALSE, # to view plots or not while they are plotting
                            title_pos = 0.5) {
    
    
    
    # error if choose_plots not a numeric vector or all
    if(choose_plots == "all") {
        # start with all plots specified for selection
        plots <- as.vector(formula_matrix_name$model) 
        message("Plotting all model numbers")
    } else {
        # change plot specification for subsetting
        plots <- as.numeric(formula_matrix_name$model[formula_matrix_name$model %in% choose_plots])
        if(is.vector(choose_plots) & is.numeric(choose_plots)) {
            # NOTE 1: need to find a way to add commas between choose_plots numbers
            message(paste(c("Plotting only plots ", choose_plots," to png files")))
        }
        else {
            stop("choose_plots must be a numeric vector with model numbers")
        }
    }
    
    # check choose_plots_mas
    # right now we can only search for a single MAS, update to vector in future for multiple
    if(choose_plots_mas == "all") {
        message("Plotting all MAS")
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
    
    if(save_plots & sum(choose_save_plots, na.rm = T) > 0) {
        if(is.vector(choose_save_plots) & is.numeric(choose_save_plots)) {
            # NOTE 1: need to find a way to add commas between choose_plots numbers
            message(paste(c("Saving only plots ", choose_save_plots," to png files")))
        }
        else {
            stop("choose_save_plots must be a numeric vector with model numbers")
        }        
    }
    
    # subset by choose_plots and/or choose_plots_mas
    formula_matrix_name <- formula_matrix_name[formula_matrix_name$model %in% plots,]
    formula_matrix_name <- formula_matrix_name[formula_matrix_name$mas %in% plots_mas,]
    
    dag_plots <- list()
    
    # find lowest X position in dagitty object
    minX <- as.numeric(readRDS(here::here("Results", "minX.RDS"))[1,1])-0.1
    
    if(title) {
        for (l in formula_matrix_name$model){
            plot(dag_matrix[[l]], xlim = c(minX,1.1), ylim = c(-1.1,1.5))
            text(0.5,-1, paste0("MAS = ",formula_matrix_name$mas[formula_matrix_name$model == l]))
            text(minX+title_pos,1.4, paste0("Model ",formula_matrix_name$model[l]), font = 2)
            dag_plots <- append(dag_plots, list(recordPlot()))
            }
    } else {
        for (l in formula_matrix_name$model){
            plot(dag_matrix[[l]], xlim = c(minX,1.1), ylim = c(-1.1,1.5))
            text(0.5,-1, paste0("MAS = ",formula_matrix_name$mas[formula_matrix_name$model == l]))
            dag_plots <- append(dag_plots, list(recordPlot()))
        }
    }
    
        
    
    if(save_plots) {
        if(sum(choose_save_plots, na.rm =T) == 0) {
            lengn = formula_matrix_name$model
        } else {
            lengn = choose_save_plots
        }
        
  
        for (n in 1:length(lengn)) {
            png(file = here::here("Results", paste0("model_", formula_matrix_name$model[n], ".png")), width = 480, height = 240)
            replayPlot(dag_plots[[n]])
            dev.off()
        }
    }
    
    return(dag_plots)
}
        
message("function plot_dag_matrix loaded")

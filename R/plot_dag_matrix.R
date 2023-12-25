library(tidyverse)

plot_dag_matrix <- function(dag_matrix,
                            save_plots = FALSE,
                            title = TRUE,
                            title_pos = 0.5) {
    
    dag_plots <- list()
    
    # find lowest X position in dagitty object
    minX <- as.numeric(readRDS(here::here("Results", "minX.RDS"))[1,1])-0.1
    
    for (l in 1:length(dag_matrix)){
        plot(dag_matrix[[l]], xlim = c(minX,1.1), ylim = c(-1.1,1.5))
        text(0.5,-1, paste0("MAS = ",formula_matrix$mas[l]))
        if(title) text(minX+title_pos,1.4, paste0("Model ",formula_matrix$model[l]), font = 2)
        dag_plots <- append(dag_plots, list(recordPlot()))
    }
    
    if(save_plots) {
        m = 1
        for (n in length(dag_matrix)) {
            png(file = here::here("Results", paste0("model_", m, ".png")), width = 480, height = 240)
            replayPlot(dag_plots[[n]])
            dev.off()
            m = m+1
        }
    }
    return(dag_plots)
}
        
message("function plot_dag_matrix loaded")

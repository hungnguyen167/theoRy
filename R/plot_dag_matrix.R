library(tidyverse)


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

build_dags <- function(formula_matrix, 
                             save_plots = FALSE) {
    
    dag_plots <- list()
    
# need a way to find the minimum X value in dag_matrix
    
    for (l in 1:length(dag_matrix)){
        plot(dag_matrix[[l]], xlim = c(-1.1,1.1), ylim = c(-1.1,1.1))
        text(0.5,-1, paste0("MAS = ",formula_matrix$mas[l]))
        text(0,1, paste0("Model ",formula_matrix$model[l]), font = 2)
        dag_plots <- append(dag_plots, list(recordPlot()))
    }
    
    if(save_plots) {
        m = 1
        for (n in 1:6) {
            png(file = here::here("Results", paste0("model_", m, ".png")), width = 480, height = 240)
            replayPlot(dag_plots[[n]])
            dev.off()
            m = m+1
        }
    }
    return(dag_plots)
}
        


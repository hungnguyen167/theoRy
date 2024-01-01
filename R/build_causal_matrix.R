library(tidyverse)
library(tictoc)

build_causal_matrix <- function(inputs, resid_corr=TRUE){ 
    # Check if 'inputs' is a list and has required components
    if (!is.list(inputs) || !all(c("nodes", "timing", "types") %in% names(inputs))) {
        stop("Input must be a list containing 'nodes', 'timing', and 'types'.")
    }

    # Check if 'nodes', 'timing', and 'types' have the same length
    if (length(inputs$nodes) != length(inputs$timing) || length(inputs$nodes) != length(inputs$types)) {
        stop("'nodes', 'timing', and 'types' must be of the same length.")
    }
    
    # Check that only one outcome and one test variable are specified
    if (length(inputs$nodes[inputs$types == "test"]) > 1) {
        stop("Please specify only one variable as 'test'")
    }
    
    # Check if 'timing' has more than 4 unique values
    if (length(unique(inputs$timing)) > 5) {
        stop("Function not executed: The total number of unique 'timing' values 
             must be less than 4.")
        # In the future we should program this to be number of total variables minus 1
    }
    # Start of function

    ## Change variable names to Xn, Xtest, Y based on types
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
    
    # Create report for user
    message("VARIABLE SUMMARY")
    message(paste0("Y. Label = ", node_timing$var_name[node_timing$node_name == "Y"], 
                 ". Timing = ", node_timing$timing[node_timing$node_name == "Y"]))
    message(paste0("Xtest. Label = ", node_timing$var_name[node_timing$node_name == "Xtest"], 
                 ". Timing = ", node_timing$timing[node_timing$node_name == "Xtest"]))
    message(paste0("X1. Label = ", node_timing$var_name[node_timing$node_name == "X1"], 
                 ". Timing = ", node_timing$timing[node_timing$node_name == "X1"]))
    if (length(node_timing$var_name) > 3) message(paste0("X2. Label = ", node_timing$var_name[node_timing$node_name == "X2"], 
                     ". Timing = ", node_timing$timing[node_timing$node_name == "X2"]))

    if (length(node_timing$var_name) > 4) message(paste0("X3. Label = ", node_timing$var_name[node_timing$node_name == "X3"], 
                     ". Timing = ", node_timing$timing[node_timing$node_name == "X3"]))
    
    if (length(node_timing$var_name) > 5) message(paste0("X4. Label = ", node_timing$var_name[node_timing$node_name == "X4"], 
                     ". Timing = ", node_timing$timing[node_timing$node_name == "X4"]))

    # create node matrix without var_labels
    node_timing <- node_timing %>%
        select(-var_name)
    x_test_time <- node_timing[which(node_timing$type=="test"),"timing"]
    y_time <- node_timing[which(node_timing$type=="otc"),"timing"]
    if (x_test_time >= y_time){
        stop("X_test must take place before Y")
    }
    # Create tables for 'from' and 'to' nodes and join with node_timing for timings and types
    from_tbl <- node_timing %>%
        expand_grid(node_from=node_timing$node_name,node_to=node_timing$node_name) %>%
        select(-node_name) %>%
        rename(node_name=node_from) %>%
        select(node_name) %>%
        left_join(node_timing, by="node_name") %>%
        rename(
            timing_from = timing,
            node_from = node_name,
            type_from = type
        ) 
    to_tbl <- node_timing %>%
        expand_grid(node_from=node_timing$node_name,node_to=node_timing$node_name) %>%
        select(-node_name) %>%
        rename(node_name=node_to) %>%
        select(node_name) %>%
        left_join(node_timing, by="node_name") %>%
        rename(
            timing_to = timing,
            node_to = node_name,
            type_to = type
        ) 
    # Combine from_tbl and to_tbl to create pairs and determine the direction of influence
    
    pairs_tbl <- bind_cols(from_tbl, to_tbl) %>%
        distinct(node_from, node_to, .keep_all=TRUE) %>%
        mutate(
            direction = case_when(
                node_from == "Xtest" & node_to == "Y" ~ "~",
                node_from == node_to & timing_from == timing_to ~  "~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                & timing_from <  timing_to & resid_corr==TRUE ~ "~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                & timing_from ==  timing_to & resid_corr == TRUE~ "~~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                & timing_from <=  timing_to & resid_corr==FALSE ~ "~",
                str_detect(node_from, "X\\d+") & node_to == "Y" 
                & timing_from <  timing_to & resid_corr==TRUE ~ "~",
                str_detect(node_from, "X\\d+") & node_to == "Y" 
                & timing_from == timing_to & resid_corr == TRUE ~ "~~",
                str_detect(node_from, "X\\d+") & node_to == "Y" 
                & timing_from <=  timing_to & resid_corr==FALSE ~ "~",
                TRUE ~ NA_character_
            )
        ) %>%
        mutate(
            pairs = paste(pmin(node_to, node_from), pmax(node_to, node_from), sep="_")
        ) %>%
        distinct(pairs, .keep_all=TRUE) %>%
        mutate(
            pairs = paste(node_from, node_to, sep="_")
        ) %>%
        select(node_from, node_to,pairs, direction)
    # Define two types of options based on the direction
    two_opt <- pairs_tbl %>%
        filter(pairs != "Xtest_Y" & node_from != node_to)
    one_opt <- pairs_tbl %>%
        filter(pairs == "Xtest_Y" | node_from == node_to)
    
    # Create a unique values list from the options
    unique_values_list <- lapply(seq_len(nrow(two_opt)), function(i) c(two_opt$direction[i], ""))
    names(unique_values_list) <- two_opt$pairs
    unique_values_list <- c(unique_values_list, setNames(one_opt$direction, one_opt$pairs))
    
    ## Create all possible combinations
    causal_matrix <- expand.grid(unique_values_list, stringsAsFactors = FALSE) %>%
        distinct(.keep_all=TRUE) %>%
        pivot_longer(cols=c(1:length(.)), names_to = "from_to", values_to="direction") %>%
        separate(from_to, into=c("from","to"), sep="_") %>%
        mutate(
            model = rep(
                1:(nrow(.)/length(unique_values_list)),
                each = length(unique_values_list)
            )
        ) %>%
        group_by(model) %>%
        filter(any(to == "Y" & str_detect(from, "Xtest"))) %>%
        ungroup() %>%
        left_join(node_timing, by= join_by(from==node_name)) %>%
        rename(
            timing_from = timing,
            type_from = type
        ) %>%
        left_join(node_timing, by=join_by(to==node_name))%>%
        rename(
            timing_to = timing,
            type_to = type
        )
    
  
    return(causal_matrix)

    
    
}

message("function build_causal_matrix loaded")


## Example: 
#inputs <- list(nodes=c("a","b","c","d"), timing=c(-3,-3,-2,-1,0),
#               types=c("ctr","ctr","test","otc"))


#tic()
#causal_matrix <- build_causal_matrix(inputs)
#toc()


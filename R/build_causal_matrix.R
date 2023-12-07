library(tidyverse)
## TO DO: make causal_matrix a class 

build_causal_matrix <- function(inputs){
    # Check type
    
    # Start of function

    
    
    ## Change variable names to Xn, Xtest, Y, and Mn
    node_timing <- tibble(var_name=inputs$nodes, timing=inputs$timing,
                              type=inputs$types) %>%
        arrange(timing) %>%
        group_by(type) %>%
        mutate(
            node_name = case_when(
                type == "otc" ~ "Y",
                type == "test" ~ "Xtest",
                type == "ctr" ~ paste0("X", row_number()),
                type == "mod" ~ paste0("M", row_number())
            )
        ) %>%
        ungroup() %>%
        select(-var_name) 
    
    
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
    pairs_tbl <- bind_cols(from_tbl, to_tbl) %>%
        distinct(node_from, node_to, .keep_all=TRUE) %>%
        mutate(
            direction = case_when(
                node_from == "Xtest" & node_to == "Y" ~ "~",
                node_from == "Xtest" & str_detect(node_to, "M\\d+") ~ "~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                & timing_from <  timing_to ~ "~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                & timing_from ==  timing_to ~ "~~",
                str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+")
                & timing_from <  timing_to ~ "~",
                str_detect(node_from, "M\\d+") & node_to == "Y" ~ "~",
                str_detect(node_from, "X[0-9A-Za-z]+") & node_to == "Y" 
                & timing_from <=  timing_to ~ "~",
                str_detect(node_from, "M\\d+") & str_detect(node_to, "M\\d+") 
                & timing_from <  timing_to ~ "~",
                str_detect(node_from, "M\\d+") & str_detect(node_to, "M\\d+") 
                & timing_from ==  timing_to ~ "~~"
            )
        ) %>%
        filter(node_from != node_to & !is.na(direction)) %>%
        mutate(
            pairs = paste(pmin(node_to, node_from), pmax(node_to, node_from), sep="_")
        ) %>%
        distinct(pairs, .keep_all=TRUE) %>%
        mutate(
            pairs = paste(node_from, node_to, sep="_")
        ) %>%
        select(pairs, direction)
    two_opt <- pairs_tbl %>%
        filter(pairs != "Xtest_Y")
    one_opt <- pairs_tbl %>%
        filter(pairs == "Xtest_Y")
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
        filter(direction != "") %>%
        group_by(model) %>%
        filter(any(to == "Y" & str_detect(from, "Xtest"))) %>% ## keep only formulas that have Y caused by Xtest
        filter(
            any(str_detect(to, "M\\d+") & str_detect(from, "X[0-9A-Za-z]+")) |
            all(!str_detect(to, "M\\d+") & !str_detect(from, "M\\d+"))
        ) %>% ## if there is M in the formula, then it must be caused by at least one X
        ungroup() 
    
   
    
    return(causal_matrix)

}





## Example: 
inputs <- list(nodes=c("a","b","c","d","e","f"), timing=c(-1,-2,-2,-1,0,-1),
               types=c("ctr","ctr","ctr","test","otc","mod"))

causal_matrix <- build_causal_matrix(inputs)




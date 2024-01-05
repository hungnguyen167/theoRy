require(tidyverse)
require(data.table)


remove_redundant <- function(nested_dt){
    unique_from <- nested_dt[, .(UniqueValues = names(table(from)[table(from) == 1]))]
    unique_to <- nested_dt[, .(UniqueValues = names(table(to)[table(to) == 1]))]
    common_unq <- intersect(unique_from$UniqueValues, unique_to$UniqueValues)
    common_unq <- unique(common_unq)
    nested_dt <- nested_dt[!(from == to & !(from %in% common_unq))]
    return(nested_dt)
    
}

match_base <- function(nested_dt, nested_base) {
    temp <- copy(nested_dt)
    temp[,model:=NULL]
    temp <- temp[order(from,to)]
    ident <- ifelse(identical(temp, nested_base),1,0)
    return(ident)
}


dt_to_string <- function(dt) {
    dt[order(c("from", "to"))]
    dt[,model:=NULL]
    return(dt)
}

build_causal_node <- function(nodes, types, timing, user_mods=NULL, 
                                include_subsets=FALSE,return_node=FALSE){ 
    # Check if 'inputs' is a list and has required components
    if (!is.character(nodes) | !is.character(types) | !is.double(timing)) {
        stop("Wrong input format. Please check that nodes and types are character and timing is double!")
    }

    # Check if 'nodes', 'timing', and 'types' have the same length
    if (length(nodes) != length(timing) | length(nodes) != length(types)) {
        stop("'nodes', 'timing', and 'types' must be of the same length.")
    }
    
    # Check that only one outcome and one test variable are specified
    if (length(types[types == "test"]) > 1) {
        stop("Please specify only one variable as 'test'")
    }
    # Check if types are not test, ctr, or otc
    if (!all(types %in% c("otc","ctr","test"))){
        stop("Types must be one of 'otc', 'ctr', or 'test'!")
    }
    # Check if 'timing' has more than 4 unique values
    if (length(unique(timing)) > 5) {
        stop("Function not executed: The total number of unique 'timing' values 
             must be less than 4.")
        # In the future we should program this to be number of total variables minus 1
    }
    # Start of function
    ## Turn input from lavaan format into list
   

    ## Change variable names to Xn, Xtest, Y based on types
    
    
    node_timing <- tibble(var_name=nodes, timing=timing,
                              type=types) %>%
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
    if (!is.null(user_mods)){
        base_matrix <- tibble()
        mod_ctr <- 1
        for (mod in user_mods){
            ls_formulas <- strsplit(mod, "\\;")[[1]]
            for (formula in ls_formulas){
                if (grepl("[ A-Za-z]~[ A-Za-z]", formula)){
                    node_to = strsplit(formula, "\\~")[[1]][1]
                    node_froms = strsplit(strsplit(formula, "~")[[1]][2],"\\+")[[1]]
                    for (node_from in node_froms){
                        df_temp <- tibble(from_var = node_from, to_var=node_to, 
                                          direction = "->", model = mod_ctr)
                        base_matrix <- bind_rows(base_matrix, df_temp)
                    }
                }
                else if (grepl("[ A-Za-z]~~[ A-Za-z]", formula)){
                    node_from = strsplit(formula, "\\~\\~")[[1]][1]
                    node_to = strsplit(formula, "\\~\\~")[[1]][2]
                    df_temp <- tibble(from_var = node_from, to_var=node_to, 
                                      direction = "<->", model=mod_ctr)
                    base_matrix <- bind_rows(base_matrix, df_temp)
                    
                }
            }
            mod_ctr <- mod_ctr+1
        }
        
        
        base_matrix <- base_matrix %>%
            mutate_at(vars(all_of(c("from_var","to_var"))), ~ str_replace_all(., " ", "")) %>%
            left_join(node_timing, by=c("from_var"="var_name")) %>%
            rename(timing_from = timing, type_from=type, from=node_name) %>%
            left_join(node_timing, by=c("to_var"="var_name")) %>%
            rename(timing_to = timing, type_to=type, to=node_name) %>% 
            mutate(
                component = ifelse(from!=to, paste(from,to,sep="_"), from) 
            ) %>%
            select(from, to, direction, model, component, timing_from, type_from, timing_to, type_to)
        setDT(base_matrix) 
    }
    
    x_test_time <- node_timing[which(node_timing$type=="test"),"timing"]
    y_time <- node_timing[which(node_timing$type=="otc"),"timing"]
    if (x_test_time >= y_time){
        stop("X_test must take place before Y")
    }
    if (return_node==TRUE){
        return(node_timing)
    }
    else{
        ## Create report for user
        
        message("VARIABLE SUMMARY")
        message(paste0("Y. Label = ", node_timing$var_name[node_timing$node_name == "Y"], 
                       ". Timing = ", node_timing$timing[node_timing$node_name == "Y"]))
        message(paste0("Xtest. Label = ", node_timing$var_name[node_timing$node_name == "Xtest"], 
                       ". Timing = ", node_timing$timing[node_timing$node_name == "Xtest"]))
        no_Xtest_Y <- node_timing %>% filter(!node_name %in% c("Y","Xtest"))
        for (i in 1:nrow(no_Xtest_Y)){
            message(paste0(no_Xtest_Y[i,"node_name"], ". Label = ", no_Xtest_Y[i,"var_name"], 
                           ". Timing = ", no_Xtest_Y[i,"timing"]))
        }
        ## create node matrix without var_labels
        node_timing <- node_timing %>%
            select(-var_name)
        
        ## Create tables for 'from' and 'to' nodes and join with node_timing for timings and types
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
                    str_detect(node_from, "X[0-9A-Za-z]+") & node_to == "Y" & timing_from <= timing_to ~ "->",
                    node_from == "Y" & str_detect(node_to, "X\\d+") & timing_from <= timing_to ~ "->",
                    node_from == node_to & timing_from == timing_to ~  "->",
                    str_detect(node_from, "X[0-9A-Za-z]+") & str_detect(node_to, "X[0-9A-Za-z]+") 
                    & timing_from <  timing_to ~ "->",
                    str_detect(node_from, "X\\d+") & str_detect(node_to, "X\\d+") 
                    & timing_from ==  timing_to ~ "<->",
                    TRUE ~ NA_character_
                )
            ) %>%
            mutate(
                pairs = paste(pmin(node_to, node_from), pmax(node_to, node_from), sep="_")
            ) %>%
            filter(!is.na(direction))
        pairs_dha <- pairs_tbl %>% filter(direction == "~~") %>% distinct(pairs, .keep_all = TRUE)
        pairs_other <- pairs_tbl %>% filter(direction != "~~") 
        pairs_tbl <- bind_rows(pairs_dha, pairs_other) %>%
            mutate(
                pairs = paste(node_from, node_to, sep="_")
            ) %>%
            select(node_from, node_to,pairs, direction) 
        
        # Define two types of options based on the direction
        if (include_subsets == TRUE){
            # Display a warning message
            message("Option include_subsets is set to true.")
            message("The size of the matrix could get exponentially larger with more variables") 
            message("Do you want to proceed or set include_subsets to FALSE?")
            
            response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))
            
            if (response == "yes") {
                cat("Continue with include_subsets=TRUE\n")
                two_opt <- pairs_tbl %>%
                    filter(pairs != "Xtest_Y")
                one_opt <- pairs_tbl %>%
                    filter(pairs == "Xtest_Y")
                
            } else if (response == "no") {
                cat("Set include_subsets to FALSE.\n")
                two_opt <- pairs_tbl %>%
                    filter(!grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
                one_opt <- pairs_tbl %>%
                    filter(grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
            } else {
                stop("Invalid response. Please enter 'yes' or 'no'.\n")
            }
            
        }
        else{
            two_opt <- pairs_tbl %>%
                filter(!grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
            one_opt <- pairs_tbl %>%
                filter(grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
        }
         
        # Create a unique values list from the options
        unique_values_list <- lapply(seq_len(nrow(two_opt)), function(i) c(two_opt$direction[i], "none"))
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
                ),
                component = ifelse(from!=to, paste(from,to,sep="_"), from)
            ) %>%
            filter(direction != "none") %>%
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
        setDT(causal_matrix)
        
        
        rmv_ls <- lapply(split(causal_matrix, by = "model"), remove_redundant)
        causal_matrix <- rbindlist(rmv_ls)
        dt_strings <- paste0(lapply(split(causal_matrix, by="model"), dt_to_string))
        unq_dts <- which(!duplicated(dt_strings))
        causal_matrix <- causal_matrix[model %in% unq_dts]
        if (!is.null(user_mods)){
            ls_base <- split(base_matrix,by="model")
            match_res <- list()
            for (i in seq_along(ls_base)){
                b_t <- copy(ls_base[[i]])
                b_t[, model:=NULL]
                b_t <- b_t[order(from,to)]
                match_ls_temp <- lapply(split(causal_matrix, by = "model"), match_base,
                                   b_t)
                user_mod_true <- as.numeric(names(match_ls_temp[match_ls_temp==1]))
                match_res_temp <- list(idx = unlist(user_mod_true), user_mod_n = i)
                match_res[[i]] <- match_res_temp
            }

            causal_matrix[, user_mod := 0]
            for (i in seq_along(match_res)){
                match_res_temp <- match_res[[i]]
                
                if(length(match_res_temp$idx) ==0){
                    cat("Model: '", user_mods[i], "' not found in the causal matrix. \n")
                    message("Do you want to add this to the matrix?")
                    response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))
                    if(response=="yes"){
                        message("Added")
                        b_t <- copy(ls_base[[i]])
                        b_t[, user_mod:=1]
                        b_t[, model := max(causal_matrix$model) + 1]
                        causal_matrix <- rbind(causal_matrix, b_t)
                    }
                    else if (response=="no"){
                        "Skipped"
                    }
                    else{
                        message("Invalid response. Skipped")
                    }
                }
                else{
                    cat("Model: '", user_mods[i], "' found in the matrix.\n")
                }
            }
            all_idx <- unname(do.call(c, lapply(match_res, function(x) x$idx)))
            causal_matrix[, user_mod:= ifelse(model %in% all_idx | user_mod==1, 1, 0)]
            
        }
        setorder(causal_matrix, -user_mod, model)
        causal_matrix[ , model:= .GRP, by=model]
        
        
        return(causal_matrix)
    }
        

}

message("function build_causal_matrix loaded")


## Example: 
#nodes <- c("a","b","c","d")
#timing <- c(-2,-1,-1,0)
#types <- c("ctr","ctr","test","otc")
#base_mod <- "d ~ a +c; c~a"

#tic()
#causal_matrix <- build_causal_matrix(nodes, types, timing, base_mod, include_subsets=TRUE,
                                   #  return_node=FALSE)
#toc()


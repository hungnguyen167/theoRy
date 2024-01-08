require(data.table)
require(tidyverse)
source("R/build_formula_matrix.R")

match_base <- function(nested_dt, nested_base) {
    temp <- copy(nested_dt)
    temp[,model:=NULL]
    temp <- temp[order(from,to)]
    ident <- ifelse(identical(temp, nested_base),1,0)
    return(ident)
}


find_add_models <- function(ls_theory=NULL, causal_matrix=NULL, node_timing=NULL, user_mods, 
                            on_ls=FALSE, add_nodes=NULL){
    if(is.null(user_mods)){
        stop("User-defined models must be provided!")
    }
    if(isFALSE(on_ls)){
        if(is.null(causal_matrix) | is.null(node_timing)){
            stop("Causal matrix and node timing must be provided if on_ls=FALSE")
        }
    } else{
        if(is.null(ls_theory) | !is.list(ls_theory)){
            stop("ls_theory must be a list that contains at least the causal matrix and the node timing matrix!")
        }
        causal_matrix <- ls_theory$causal_matrix
        node_timing <- ls_theory$node_timing
        
    }
    if(!is.null(add_nodes)){
        node_timing <- node_timing %>%
            add_row(var_name = add_nodes$var_names, timing = add_nodes$timing, 
                    type = add_nodes$types, node_name = add_nodes$node_names)
    }
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
    ls_base <- split(base_matrix,by="model")
    match_res <- list()
    new_causal <- copy(causal_matrix)
    new_causal[, user_mod := NULL]
    for (i in seq_along(ls_base)){
        b_t <- copy(ls_base[[i]])
        b_t[, model:=NULL]
        b_t <- b_t[order(from,to)]
        match_ls_temp <- lapply(split(new_causal, by = "model"), match_base,
                                b_t)
        user_mod_true <- as.numeric(names(match_ls_temp[match_ls_temp==1]))
        match_res_temp <- list(idx = unlist(user_mod_true), user_mod_n = i)
        match_res[[i]] <- match_res_temp
    }
    for (i in seq_along(match_res)){
        match_res_temp <- match_res[[i]]
        formula_new <- list()
        if(length(match_res_temp$idx) ==0){
            cat("Model: '", user_mods[i], "' not found in the causal matrix. \n")
            message("Do you want to add this to the matrix?")
            response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))
            if(response=="yes"){
                message("Added")
                b_t <- copy(ls_base[[i]])
                b_t[, user_mod:=1]
                b_t[, model := max(causal_matrix$model) + 1]
                if(isTRUE(on_ls)){
                    formula_temp <- build_formula_matrix(b_t)
                    formula_new[[i]] <- formula_temp
                }
                
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
    
    
    if(isTRUE(on_ls)){
        formula_dt <- rbindlist(formula_new)
        formula_matrix <- rbind(ls_theory$formula_matrix, formula_dt)
        new_ls_theory <- list(causal_matrix=causal_matrix, node_timing=node_timing, 
                              formula_matrix=formula_matrix)
        return(new_ls_theory)
    }
    else{
        return(causal_matrix)
    }
   
}



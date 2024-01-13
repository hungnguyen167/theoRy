require(data.table)
require(tidyverse)
#source("R/build_formula_matrix.R")


find_add_models <- function(ls_theory=NULL,
                            causal_matrix=NULL,
                            node_timing=NULL,
                            user_mods,
                            on_ls=FALSE,
                            add_nodes=NULL,
                            assert_mod_num=NULL){

    if(is.null(user_mods)){
        stop("User-defined models must be provided!")
    }
    if(isFALSE(on_ls)){
        if(is.null(causal_matrix) | is.null(node_timing)){
            stop("Causal matrix and node timing must be provided if on_ls=FALSE")
        }
    } else{
        if(is.null(ls_theory) | !is.list(ls_theory)){
            stop("ls_theory must be a list that contains at least the causal matrix, the formula matrix, and the node timing matrix!")
        }
        causal_matrix <- copy(ls_theory$causal_matrix)
        node_timing <- copy(ls_theory$node_timing)
        if(!"user_mod" %in% colnames(causal_matrix)){
            causal_matrix[,user_mod:=0]
        }
        formula_matrix <- copy(ls_theory$formula_matrix)

    }
    if(!is.null(add_nodes)){
        node_timing <- node_timing %>%
            mutate(
                unq_indentifier = paste(var_name, timing, type, node_name, sep="_"),
                timing = as.numeric(timing)
            )
        compare_ident <- unlist(node_timing$unq_indentifier)
        for (i in seq_along(add_nodes)){
            unq_identifier <- paste(add_nodes[[i]], collapse="_")
            match_ident <- any(unq_identifier %in% compare_ident)
            if(isTRUE(match_ident)){
                cat("The unique identifier",unq_identifier, "is already in the node_timing matrix!\n")
            }
            else{
                node_timing <- node_timing %>%
                    add_row(var_name = add_nodes[[i]]["var_name"], timing = as.numeric(add_nodes[[i]]["timing"]),
                            type = add_nodes[[i]]["type"], node_name = add_nodes[[i]]["node_name"])
            }
        }
        node_timing <- node_timing %>%
            select(-unq_indentifier)
    }
    base_matrix <- tibble()
    for (i in seq_along(user_mods)){
        ls_formulas <- strsplit(user_mods[[i]], "\\;")[[1]]
        for (j in seq_along(ls_formulas)){
            if (grepl("[ A-Za-z]~[ A-Za-z]", ls_formulas[[j]])){
                node_to = strsplit(ls_formulas[[j]], "\\~")[[1]][1]
                node_froms = strsplit(strsplit(ls_formulas[[j]], "~")[[1]][2],"\\+")[[1]]
                for (node_from in node_froms){
                    df_temp <- tibble(from_var = node_from,
                                      to_var=node_to,
                                      direction = "->",
                                      model = ifelse(!is.null(assert_mod_num),
                                                     assert_mod_num[i],
                                                     i))
                    base_matrix <- bind_rows(base_matrix, df_temp)
                }
            }
            else if (grepl("[ A-Za-z]~~[ A-Za-z]", ls_formulas[[j]])){
                node_from = strsplit(ls_formulas[[j]], "\\~\\~")[[1]][1]
                node_to = strsplit(ls_formulas[[j]], "\\~\\~")[[1]][2]
                df_temp <- tibble(from_var = node_from,
                                  to_var=node_to,
                                  direction = "<->",
                                  model=ifelse(!is.null(assert_mod_num),
                                                                  assert_mod_num[i],
                                                                  i))
                base_matrix <- bind_rows(base_matrix, df_temp)

            }
        }
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

    for (i in seq_along(ls_base)){
        b_t <- copy(ls_base[[i]])
        b_t <- b_t[, .(from, to, direction)]
        setorder(b_t, from, to, direction)
        match_ls_temp <- lapply(split(causal_matrix, by = "model"), match_base,
                                b_t)
        user_mod_true <- as.numeric(names(match_ls_temp[match_ls_temp==1]))
        match_res[[i]] <- list(idx = unlist(user_mod_true), user_mod_n = i)
        match_res[[i]] <- match_res[[i]]
    }

    causal_matrix[,prev_mod:=model]
    formula_new <- list()

    for (i in seq_along(match_res)){
        if(length(match_res[[i]]$idx) ==0){
            cat("Model: '", user_mods[i], "' not found in the causal matrix. \n")
            message("Do you want to add this to the matrix?")
            response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))
            if(response=="yes"){
                message("Added")
                b_t <- copy(ls_base[[i]])

                if(!is.null(assert_mod_num)){
                    b_t[,`:=`(prev_mod=max(causal_matrix$model)+1,user_mod=1)]
                    causal_matrix[, model:= fifelse(model < assert_mod_num[[i]],
                                                   model,
                                                   model+1)]
                    causal_matrix <- rbind(causal_matrix, b_t)

                } else{
                    b_t[, `:=`(model= max(causal_matrix$model)+1,prev_mod=max(causal_matrix$model)+1,user_mod=1)]
                    causal_matrix <- rbind(causal_matrix, b_t)

                }
                if(isTRUE(on_ls)){
                    formula_temp <- build_formula_matrix(b_t)
                    formula_new[[i]] <- formula_temp
                }



           } else if (response=="no"){
                "Skipped"
            } else{
                message("Invalid response. Skipped")
            }

        }
        else{
            cat("Model: '", user_mods[i], "' found in the matrix. Position:", match_res[[i]]$idx,"\n")

            if(!is.null(assert_mod_num)){
                if(match_res[[i]]$idx != assert_mod_num[[i]]){
                    cat("Swapped current model (", match_res[[i]]$idx,") to model number", assert_mod_num[[i]], " and vice versa. \n")
                    causal_matrix[,prev_mod:=model]
                    causal_matrix[,model:=fifelse(model==match_res[[i]]$idx,assert_mod_num[[i]],
                                                  fifelse(model==assert_mod_num[[i]], match_res[[i]]$idx,
                                                          model))]
                    causal_matrix[,user_mod:=fifelse(model==assert_mod_num[[i]], 1, user_mod)]

                    for (j in i:length(match_res)){
                        if(length(match_res[[j]]$idx)>0){
                            if (match_res[[j]]$idx == assert_mod_num[[i]]){
                                match_res[[j]]$idx <- match_res[[i]]$idx
                            }
                        }
                    }
                    match_res[[i]]$idx <- assert_mod_num[[i]]

                } else{
                        cat("Asserted position is equal to current position. Skipped.\n")
                    }
                }


            }

        }


    if(isTRUE(on_ls)){


        causal_copy <- copy(causal_matrix)
        causal_copy[, model_ref:=model]
        causal_copy <- causal_copy[, .(model_ref, prev_mod)]
        formula_matrix_m <- formula_matrix[causal_copy, on = .(model = prev_mod), nomatch = NA]
        formula_matrix_m <- unique(formula_matrix_m, by="formula")
        formula_matrix_m[, model:=model_ref]
        formula_matrix_m[, model_ref:=NULL]
        formula_matrix_m <- formula_matrix_m[!is.na(formula),]
        if(any(length(match_res[[i]])>0)){
            formula_matrix_add <- rbindlist(formula_new)
            formula_matrix <- rbind(formula_matrix_m, formula_matrix_add)
        } else{
            formula_matrix <- formula_matrix_m
        }

        setorder(formula_matrix,-user_mod, model)
        setorder(causal_matrix,-user_mod, model)
        new_ls_theory <- list(causal_matrix=causal_matrix, node_timing=node_timing,
                              formula_matrix=formula_matrix)
        #causal_matrix[, prev_mod:= NULL]

        return(new_ls_theory)
    } else{
        #causal_matrix[, prev_mod:= NULL]
        setorder(causal_matrix, -user_mod, model)
        return(causal_matrix)
    }

}



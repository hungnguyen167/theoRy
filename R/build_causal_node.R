#' Build causal matrix or build node-timing matrix
#'
#' @description
#' `build_causal_node` returns either the causal matrix or the node-timing matrix used in theory comparison
#'
#' @details
#' This function is included in theoRy. Unless computing the causal matrix separately is require, users are encouraged to use
#' theoRy instead.
#'
#'
#' @param nodes the input nodes/variable names, a character vector/list.
#' Should be of the same length and order with types and timing.
#' @param types the input types of nodes, a character vector/list. Takes only three values: "otc" (outcome), "ctr" (control),
#' or "test" (test). Should be of the same length and order with nodes and timing.
#' @param timing the input timing of nodes, a character vector/list. Should be of the same length and order with nodes and types.
#' @param user_mods User-defined model(s), optional. This argument allows users to input their theoretical biases.
#' If the model(s) are found in the matrix, they will be pushed to the top in the orders that they are introduced.
#' If the model(s) are not found in the matrix, the user can choose to add it to the matrix or not.
#' @param include_subsets if TRUE, the matrix will include models where certain nodes (besides outcome and exposure)
#' do not exist. Note that model A where X does not cause anything but still exists is a different theoretical claim than
#' model B where X does not exist entirely. The matrix can get significantly larger when this option is allowed. Default to FALSE.
#' @param return_node if TRUE, returns the node-timing matrix instead of the causal matrix.
#'
#'
#' @returns Either a causal matrix or a node-timing matrix. The causal matrix is the basis for comparing theoretical models. The causal matrix consists of at least 9 columns:
#' from, to, direction, model, component, timing_from, type_from, timing_to, type_to. The direction only takes two values "->" or
#' "<->". "<-" is omitted because X2 <- X1 is similar to X1 -> X2. The column user_mod is introduced when user_mods are provided.
#' In the causal matrix, nodes are called by conventional node names for causal inference (Y, Xtest, X1, X2, etc.) instead of
#' their original variable names. A summary of variable names, timing, and types is provided when the function runs. Users can
#' check this information in the node-timing matrix.
#' @examples
#' nodes <- c("y","xtest","ctr1","ctr2")
#' timing <- c(0,-1,-3,-2)
#' types <- c("otc","test","ctr","ctr")
#' user_mods <- c("y ~ xtest + ctr2; xtest ~ ctr1 + ctr2", "y ~ xtest + ctr1; xtest ~ ctr1 + ctr2")
#' causal_matrix <- build_causal_node(nodes=nodes, types=types, timing=timing, user_mods=user_mods, include_subsets=TRUE, return_node=FALSE)
#'
#' @export









build_causal_node <- function(nodes,
                              types,
                              timing,
                              user_mods=NULL,
                              include_subsets=FALSE,
                              return_node=FALSE){
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


    node_timing <- tibble::tibble(var_name=nodes, timing=timing,
                              type=types) %>%
        dplyr::arrange(timing) %>%
        dplyr::group_by(type) %>%
        dplyr::mutate(
            node_name = dplyr::case_when(
                type == "otc" ~ "Y",
                type == "test" ~ "Xtest",
                type == "ctr" ~ paste0("X", row_number())
            )
        ) %>%
        dplyr::ungroup()
    if (!is.null(user_mods)){
        base_matrix <- tibble::tibble()
        mod_ctr <- 1
        for (mod in user_mods){
            ls_formulas <- strsplit(mod, "\\;")[[1]]
            for (formula in ls_formulas){
                if (grepl("[ A-Za-z]~[ A-Za-z]", formula)){
                    node_to = strsplit(formula, "\\~")[[1]][1]
                    node_froms = strsplit(strsplit(formula, "~")[[1]][2],"\\+")[[1]]
                    for (node_from in node_froms){
                        df_temp <- tibble::tibble(from_var = node_from, to_var=node_to,
                                          direction = "->", model = mod_ctr)
                        base_matrix <- dplyr::bind_rows(base_matrix, df_temp)
                    }
                }
                else if (grepl("[ A-Za-z]~~[ A-Za-z]", formula)){
                    node_from = strsplit(formula, "\\~\\~")[[1]][1]
                    node_to = strsplit(formula, "\\~\\~")[[1]][2]
                    df_temp <- tibble::tibble(from_var = node_from, to_var=node_to,
                                      direction = "<->", model=mod_ctr)
                    base_matrix <- dplyr::bind_rows(base_matrix, df_temp)

                }
            }
            mod_ctr <- mod_ctr+1
        }


        base_matrix <- base_matrix %>%
            dplyr::mutate_at(dplyr::vars(tidyr::all_of(c("from_var","to_var"))), ~ stringr::str_replace_all(., " ", "")) %>%
            dplyr::left_join(node_timing, by=c("from_var"="var_name")) %>%
            dplyr::rename(timing_from = timing, type_from=type, from=node_name) %>%
            dplyr::left_join(node_timing, by=c("to_var"="var_name")) %>%
            dplyr::rename(timing_to = timing, type_to=type, to=node_name) %>%
            dplyr::mutate(
                component = ifelse(from!=to, paste(from,to,sep="_"), from)
            ) %>%
            dplyr::select(from, to, direction, model, component, timing_from, type_from, timing_to, type_to)
        data.table::setDT(base_matrix)
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
        no_Xtest_Y <- node_timing %>% dplyr::filter(!node_name %in% c("Y","Xtest"))
        for (i in 1:nrow(no_Xtest_Y)){
            message(paste0(no_Xtest_Y[i,"node_name"], ". Label = ", no_Xtest_Y[i,"var_name"],
                           ". Timing = ", no_Xtest_Y[i,"timing"]))
        }
        ## create node matrix without var_labels

        ## Create tables for 'from' and 'to' nodes and join with node_timing for timings and types
        from_tbl <- node_timing %>%
            dplyr::select(-var_name) %>%
            tidyr::expand_grid(node_from=node_timing$node_name,node_to=node_timing$node_name) %>%
            dplyr::select(-node_name) %>%
            dplyr::rename(node_name=node_from) %>%
            dplyr::select(node_name) %>%
            dplyr::left_join(dplyr::select(node_timing,-var_name), by="node_name") %>%
            dplyr::rename(
                timing_from = timing,
                node_from = node_name,
                type_from = type
            )
        to_tbl <- node_timing %>%
            dplyr::select(-var_name) %>%
            tidyr::expand_grid(node_from=node_timing$node_name,node_to=node_timing$node_name) %>%
            dplyr::select(-node_name) %>%
            dplyr::rename(node_name=node_to) %>%
            dplyr::select(node_name) %>%
            dplyr::left_join(dplyr::select(node_timing,-var_name), by="node_name") %>%
            dplyr::rename(
                timing_to = timing,
                node_to = node_name,
                type_to = type
            )
        # Combine from_tbl and to_tbl to create pairs and determine the direction of influence

        pairs_tbl <- dplyr::bind_cols(from_tbl, to_tbl) %>%
            dplyr::distinct(node_from, node_to, .keep_all=TRUE) %>%
            dplyr::mutate(
                direction = dplyr::case_when(
                    stringr::str_detect(node_from, "X[0-9A-Za-z]+") & node_to == "Y" & timing_from <= timing_to ~ "->",
                    node_from == "Y" & stringr::str_detect(node_to, "X\\d+") & timing_from <= timing_to ~ "->",
                    node_from == node_to & timing_from == timing_to ~  "->",
                    stringr::str_detect(node_from, "X[0-9A-Za-z]+") & stringr::str_detect(node_to, "X[0-9A-Za-z]+")
                    & timing_from <  timing_to ~ "->",
                    stringr::str_detect(node_from, "X\\d+") & stringr::str_detect(node_to, "X\\d+")
                    & timing_from ==  timing_to ~ "<->",
                    TRUE ~ NA_character_
                )
            ) %>%
            dplyr::mutate(
                pairs = paste(pmin(node_to, node_from), pmax(node_to, node_from), sep="_")
            ) %>%
            dplyr::filter(!is.na(direction))
        pairs_dha <- pairs_tbl %>% dplyr::filter(direction == "~~") %>% dplyr::distinct(pairs, .keep_all = TRUE)
        pairs_other <- pairs_tbl %>% dplyr::filter(direction != "~~")
        pairs_tbl <- dplyr::bind_rows(pairs_dha, pairs_other) %>%
            dplyr::mutate(
                pairs = paste(node_from, node_to, sep="_")
            ) %>%
            dplyr::select(node_from, node_to,pairs, direction)

        # Define two types of options based on the direction
        if (include_subsets == TRUE){
            # Display a warning message
            message("Option include_subsets is set to true.")
            message("The size of the matrix could get exponentially larger with more variables")
            message("Do you want to proceed (yes) or set include_subsets to FALSE (no)?")

            response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))

            if (response == "yes") {
                cat("Continue with include_subsets=TRUE\n")
                two_opt <- pairs_tbl %>%
                    dplyr::filter(pairs != "Xtest_Y")
                one_opt <- pairs_tbl %>%
                    dplyr::filter(pairs == "Xtest_Y")

            } else if (response == "no") {
                cat("Set include_subsets to FALSE.\n")
                two_opt <- pairs_tbl %>%
                    dplyr::filter(!grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
                one_opt <- pairs_tbl %>%
                    dplyr::filter(grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
            } else {
                stop("Invalid response. Please enter 'yes' or 'no'.\n")
            }

        } else{
            two_opt <- pairs_tbl %>%
                dplyr::filter(!grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
            one_opt <- pairs_tbl %>%
                dplyr::filter(grepl("Xtest_Y|^(X\\d+)_\\1$", pairs))
        }

        # Create a unique values list from the options
        unique_values_list <- lapply(seq_len(nrow(two_opt)), function(i) c(two_opt$direction[i], "none"))
        names(unique_values_list) <- two_opt$pairs
        unique_values_list <- c(unique_values_list, setNames(one_opt$direction, one_opt$pairs))

        ## Create all possible combinations
        causal_matrix <- expand.grid(unique_values_list, stringsAsFactors = FALSE) %>%
            dplyr::distinct(.keep_all=TRUE) %>%
            tidyr::pivot_longer(cols=c(1:length(.)), names_to = "from_to", values_to="direction") %>%
            tidyr::separate(from_to, into=c("from","to"), sep="_") %>%
            dplyr::mutate(
                model = rep(
                    1:(nrow(.)/length(unique_values_list)),
                    each = length(unique_values_list)
                ),
                component = ifelse(from!=to, paste(from,to,sep="_"), from)
            ) %>%
            dplyr::filter(direction != "none") %>%
            dplyr::group_by(model) %>%
            dplyr::filter(any(to == "Y" & stringr::str_detect(from, "Xtest"))) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(dplyr::select(node_timing,-var_name), by= dplyr::join_by(from==node_name)) %>%
            dplyr::rename(
                timing_from = timing,
                type_from = type
            ) %>%
            dplyr::left_join(select(node_timing,-var_name), by=dplyr::join_by(to==node_name))%>%
            dplyr::rename(
                timing_to = timing,
                type_to = type
            )
        data.table::setDT(causal_matrix)


        rmv_ls <- lapply(split(causal_matrix, by = "model"), remove_redundant)
        causal_matrix <- data.table::rbindlist(rmv_ls)

        dt_hash <- lapply(split(causal_matrix, by="model"), dt_to_hash)
        unq_dts <- which(!duplicated(dt_hash))
        causal_matrix <- causal_matrix[model %in% unq_dts]

        #dt_strings <- paste0(lapply(split(causal_matrix, by="model"), dt_to_string))
        #unq_dts <- which(!duplicated(dt_strings))
        #causal_matrix <- causal_matrix[model %in% unq_dts]

        causal_matrix[ , model:= .GRP, by=model]
        if (!is.null(user_mods)){
            ls_base <- split(base_matrix,by="model")
            match_res <- list()
            for (i in seq_along(ls_base)){
                b_t <- data.table::copy(ls_base[[i]])
                b_t <- b_t[ ,.(from, to, direction)]
                data.table::setorder(b_t, from, to, direction)
                match_ls_temp <- lapply(split(causal_matrix, by = "model"), match_base,
                                   b_t)
                user_mod_true <- as.numeric(names(match_ls_temp[match_ls_temp==1]))
                match_res_temp <- list(idx = unlist(user_mod_true), user_mod_n = i)
                match_res[[i]] <- match_res_temp
            }

            causal_matrix[, user_mod := 0]
            causal_matrix[, prev_mod := model]
            for (i in seq_along(match_res)){
                if(length(match_res[[i]]$idx) ==0){
                    cat("Model: '", user_mods[i], "' not found in the causal matrix. \n")
                    message("Do you want to add this to the matrix?")
                    response <- tolower(readline(prompt = "Enter 'yes' or 'no': "))
                    if(response=="yes"){
                        message("Added")
                        b_t <- data.table::copy(ls_base[[i]])
                        b_t[, user_mod:=1]
                        b_t[, `:=`(model = i, prev_mod=i)]
                        causal_matrix[, model:= data.table::fifelse(model >= i,
                                                       model+1,
                                                       model)]
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
                    cat("Model: '", user_mods[i], "' found in the matrix. Position:", match_res[[i]]$idx,"\n")


                    if(match_res[[i]]$idx != i){
                        cat("Placed current model (", match_res[[i]]$idx,") to model number",i, " and vice versa. \n")
                        causal_matrix[,model:= data.table::fifelse(model==match_res[[i]]$idx,i,
                                                                   data.table::fifelse(model==i, match_res[[i]]$idx,
                                                              model))]
                        causal_matrix[,user_mod:=fifelse(model==i, 1, user_mod)]
                        for (j in i:length(match_res)){
                            if(length(match_res[[j]]$idx)>0){
                                if (match_res[[j]]$idx == i){
                                    match_res[[j]]$idx <- match_res[[i]]$idx
                                }
                            }
                        }
                        match_res[[i]]$idx <- i

                    } else{
                        cat("Current model is at already at position",i,".\n")
                    }


                }

            }

        }

        data.table::setorder(causal_matrix, -user_mod, model)
        causal_matrix[, prev_mod := NULL]
        return(causal_matrix)
    }


}



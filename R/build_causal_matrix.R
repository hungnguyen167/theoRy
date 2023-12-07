library(tidyverse)
## TO DO: make causal_matrix a class 

build_causal_matrix <- function(outcomes,c, m, ){
    # Check type
    
    # Start of function

    unique_values_list <- list()

    
    inputs <- c(idps, m)
    destinations <- c(outcomes, m)
    ## Xs and Ms must cause Y
    i_d_df <- crossing(inputs, destinations, edge=c("~", "")) %>% 
        filter(inputs!=destinations) %>%
        group_by(destinations, inputs) %>%
        mutate(row=row_number()) %>%
        pivot_wider(names_from=c(inputs,destinations), values_from=edge) %>%
        select(-row) %>%
        ungroup()
    for (col in names(i_d_df)) {
        unique_values_list[[col]] <- append(unique_values_list[[col]], na.omit(unique(i_d_df[[col]])))
    }
    ## Xs can be correlated or not
    if (length(idps)>1){
        i_i_df <- crossing(idps, idps2=idps, edge=c("~~","")) %>% 
            filter(idps!=idps2) 
        i_i_df <- i_i_df[!duplicated(t(apply(i_i_df[c("idps", "idps2", "edge")], 1, sort))), ] %>%
            group_by(idps2, idps) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(idps,idps2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        for (col in names(i_i_df)) {
            unique_values_list[[col]] <- append(unique_values_list[[col]], na.omit(unique(i_i_df[[col]])))
        }
        i_i_c_df <- crossing(idps, idps2=idps, edge=c("~", "")) %>% 
            filter(idps!=idps2) %>%
            group_by(idps2, idps) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(idps,idps2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        for (col in names(i_i_c_df)) {
            unique_values_list[[col]] <- append(unique_values_list[[col]], na.omit(unique(i_i_c_df[[col]])))
        }
    }
    ## Ms can be correlated or not
    if (length(m)>1){
        m_m_df <- crossing(m, m2=m, edge=c("~~","")) %>% 
            filter(m!=m2) 
        m_m_df <- m_m_df[!duplicated(t(apply(m_m_df[c("m", "m2", "edge")], 1, sort))), ] %>%
            group_by(m, m2) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(m,m2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        
        for (col in names(m_m_df)) {
            unique_values_list[[col]] <- append(unique_values_list[[col]], na.omit(unique(m_m_df[[col]])))
        }

    }
    
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
            pair = paste(pmin(to, from), pmax(to, from), sep="_")
        ) %>%
        filter(direction != "") %>%
        group_by(model) %>%
        filter(any(to == "Y" & str_detect(from, "X\\d+"))) %>% ## keep only formulas that have Y caused by at least one X
        filter(
            any(str_detect(to, "M\\d+") & str_detect(from, "X\\d+")) |
            all(!str_detect(to, "M\\d+") & !str_detect(from, "M\\d+"))
        ) %>% ## if there is M in the formula, then it must be caused by at least one X
        distinct(pair, .keep_all = TRUE) %>%
        ungroup() %>%
        mutate(
            unq_cmb = paste(pair, direction, "_")
        ) %>%
        group_by(model) %>%
        distinct(unq_cmb, .keep_all=TRUE) %>%
        ungroup()
    
    ## Remove formulas that are identical (different ordering, same formulas)
    collapsed_matrix <- causal_matrix %>%
        group_by(model) %>%
        summarise(
            col_formula = paste(from, to, direction, collapse=" ")
        ) %>%
        distinct(col_formula, .keep_all=TRUE)
    
    causal_matrix <- causal_matrix %>%
        filter(model %in% collapsed_matrix$model) 
    return(causal_matrix)

}





## Example: 
outcomes <- "Y"
idps <- c("X1", "X2", "X3")
m <- c("M1")

causal_matrix <- build_causal_matrix(outcomes, idps, m)




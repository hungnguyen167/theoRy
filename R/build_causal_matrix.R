library(tidyverse)
build_causal_matrix <- function(outcomes,idps, m){
    ## Check type
    
    ## Create combinations

    unique_values_list <- list()
    
    destinations <- c(outcomes, m)
    inputs <- c(idps, m)
    i_d_df <- crossing(inputs, destinations, edge=c("~", "none")) %>% 
        filter(inputs!=destinations) %>%
        group_by(destinations, inputs) %>%
        mutate(row=row_number()) %>%
        pivot_wider(names_from=c(inputs,destinations), values_from=edge) %>%
        select(-row) %>%
        ungroup()
    for (col in names(i_d_df)) {
        unique_values_list[[col]] <- na.omit(unique(i_d_df[[col]]))
    }
    if (length(idps)>1){
        i_i_df <- crossing(idps, idps2=idps, edge=c("~~","none")) %>% 
            filter(idps!=idps2) 
        i_i_df <- i_i_df[!duplicated(t(apply(i_i_df[c("idps", "idps2", "edge")], 1, sort))), ] %>%
            group_by(idps2, idps) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(idps,idps2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        for (col in names(i_i_df)) {
            unique_values_list[[col]] <- na.omit(unique(i_i_df[[col]]))
        }
        i_i_c_df <- crossing(idps, idps2=idps, edge=c("~", "none")) %>% 
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
    
    if (length(m)>1){
        m_m_df <- crossing(m, m2=m, edge=c("~~","none")) %>% 
            filter(m!=m2) 
        m_m_df <- m_m_df[!duplicated(t(apply(m_m_df[c("m", "m2", "edge")], 1, sort))), ] %>%
            group_by(m, m2) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(m,m2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        
        for (col in names(m_m_df)) {
            unique_values_list[[col]] <- na.omit(unique(m_m_df[[col]]))
        }
        m_m_c_df <- crossing(m, m2=m, edge=c("~", "none")) %>% 
            filter(m!=m2) %>%
            group_by(m, m2) %>%
            mutate(row=row_number()) %>%
            pivot_wider(names_from=c(m,m2), values_from=edge) %>%
            select(-row) %>%
            ungroup()
        for (col in names(m_m_c_df)) {
            unique_values_list[[col]] <- append(unique_values_list[[col]], na.omit(unique(m_m_c_df[[col]])))
        }
    }
    
    
    causal_matrix <- expand.grid(unique_values_list, stringsAsFactors = FALSE) %>%
        pivot_longer(cols=c(1:length(.)), names_to = "from_to", values_to="direction") %>%
        separate(from_to, into=c("from","to"), sep="_") %>%
        mutate(
            model = rep(
                1:(nrow(.)/length(unique_values_list)),
                each = length(unique_values_list)
            )
        )
    
    

    return(causal_matrix)

}

## Example: 
outcomes <- "Y"
idps <- c("X1","X2")
m <- "M1"

causal_matrix <- build_causal_matrix(outcomes, idps, m)


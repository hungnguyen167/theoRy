# build a matrix ready for set theory analysis out of the causal matrix

build_set_matrix <- function(causal_matrix,
                             outcome_var = "test_compatible",
                             outcome_positive = "compatible", # choose the string that indicates a positive outcome
                             cmp_matrix_name = cmp_matrix) {
    
    causal_matrix_t <- causal_matrix %>%
        as.data.frame() %>%
        subset(select = c(model,component))
    
    for (v in causal_matrix_t$component) {
        causal_matrix_t[,paste0(v)] <- ifelse(causal_matrix_t$component == v, 1, 0)
    }
    
    # get each component as a variable
    # this leaves single variables at 0, this may be enough info
    causal_matrix_t <- causal_matrix_t %>%
        subset(select = -component) %>%
        group_by(model) %>%
        summarise_all(max, na.rm = T)
    
    # add formula results
    formula_matrix_t <- as.data.frame(cmp_matrix_name) %>% dplyr::select(model, {{outcome_var}})
   
    ref_mod = cmp_matrix_name[test_compatible == "reference model", c(model, formula, mas)]
    
    formula_matrix_t <- subset(as.data.frame(formula_matrix_t), model != as.numeric(ref_mod[1,1])) %>%
        mutate(outcome = ifelse({{outcome_var}} == outcome_positive, 1, 0)) %>%
        dplyr::select(-{{outcome_var}})
    
    causal_matrix_t <- causal_matrix_t %>%
        subset(model != as.numeric(ref_mod[1,1])) %>%
        left_join(formula_matrix_t, by = "model")
    
    print(paste("Reference Model NUMBER = ", ref_mod[1,1], sep = ""))
    print(paste("Reference Model FORMULA is ", ref_mod[1,2], sep = ""))
    print(paste("Reference Model MAS = ", ref_mod[1,3], sep = ""))
    print(paste("Set Matrix OUTCOME set to ", outcome_var, " = ", outcome_positive))
    return(as.data.frame(causal_matrix_t))
    
}

message("function build_set_matrix loaded")

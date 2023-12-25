library(ggdag)
library(dagitty)

identify_compatible <- function(formula_matrix, effect="direct",
                                ref_mod=NULL){
    additional_args <- list(
        exposure="Xtest",
        outcome="Y"
    )
    if (is.null(effect)){
        effect = "direct"
        warning("Effect not given, use default (direct) instead.")
    }
    formulas <- formula_matrix$formula
    adj_ls <- list()
    for (i in seq_along(formulas)){
        formulas_vector <- strsplit(formulas[i], ",")[[1]]
        dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))
        adjsets <- adjustmentSets(dag, "Xtest","Y",type = "minimal", effect="direct")
        names(adjsets) <- NULL
        adj_ls <- append(adj_ls, list(adjsets))
    }
    adj_ls <- lapply(adj_ls, function(inner_list){
        c(inner_list)
    })

    if (is.null(ref_mod)){
        ref_mod = 1
        warning("User did not choose a reference model, default to 1")
    }

    ref_adj <- adj_ls[[ref_mod]]
    cmp_adj <- adj_ls[-ref_mod]
    ls_cmp <- list()
    ctr <- 1
    for (i in seq_along(cmp_adj)){
        if (ref_mod==ctr){
            ctr = ctr+1
        }
        identical <- lapply(cmp_adj[[i]], function(x) any(identical(x, unlist(ref_adj))))
        single_excl <-  lapply(cmp_adj[[i]], function(x) any(ref_adj %in% x))

        if(any(identical==TRUE)){
            ls_temp <- list(list(cat="compatible", model=ctr))
            ls_cmp <- append(ls_cmp, ls_temp)
        } else {
            if(any(single_excl==TRUE)) {
                ls_temp <- list(list(cat="singularly exclusive", model=ctr))
                ls_cmp <- append(ls_cmp, ls_temp)
            }
            else{
                ls_temp <- list(list(cat="mutually exclusive", model=ctr))
                ls_cmp <- append(ls_cmp, ls_temp)
            }
        }
        ctr <- ctr+1
    }
    ref_matrix <- data.frame(
        model = ref_mod,
        category = "reference model"
    )
    cmp_matrix <- data.frame(
        model = unlist(lapply(ls_cmp, function(sublist) sublist$model)),
        category = unlist(lapply(ls_cmp, function(sublist) sublist$cat))
    )
    comp_matrix <- rbind(ref_matrix, cmp_matrix) %>%
        arrange(model)
    
    return(comp_matrix)
}

message("function identify_compatible loaded")
#comp_matrix <- identify_compatible(full_matrix, ref_mod = 1)



#formulas_vector <- strsplit(unlist(formula_matrix[9,1]), ",")[[1]]
#dag <- do.call(dagify, c(lapply(formulas_vector, as.formula), additional_args))



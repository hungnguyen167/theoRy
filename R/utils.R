match_base <- function(nested_dt, nested_base) {
    temp <- copy(nested_dt)
    temp <- temp[, .(from, to, direction)]
    setorder(temp, from, to, direction)
    ident <- ifelse(identical(temp, nested_base),1,0)
    return(ident)
}
remove_redundant <- function(nested_dt){
    unique_from <- nested_dt[, .(UniqueValues = names(table(from)[table(from) == 1]))]
    unique_to <- nested_dt[, .(UniqueValues = names(table(to)[table(to) == 1]))]
    common_unq <- intersect(unique_from$UniqueValues, unique_to$UniqueValues)
    common_unq <- unique(common_unq)
    nested_dt <- nested_dt[!(from == to & !(from %in% common_unq))]
    return(nested_dt)

}


dt_to_string <- function(dt) {
    setorder(dt, from, to)
    dt[,model:=NULL]
    return(dt)
}

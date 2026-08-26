#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import data.table
#' @importFrom dplyr arrange group_by mutate ungroup bind_rows rename mutate_at left_join select vars filter
#' join_by bind_cols row_number
#' @importFrom tidyr expand_grid all_of separate pivot_longer pivot_wider
#' @importFrom stringr str_replace_all str_detect
#' @importFrom ggplot2 aes annotate ggplot
#' @importFrom magrittr %>%
#' @importFrom digest digest
#' @importFrom ggdag dagify geom_dag_point geom_dag_edges geom_dag_text theme_dag
#' @importFrom ragg agg_png
#' @importFrom tibble as_tibble add_row tibble
#' @importFrom grDevices dev.off
#' @importFrom stats as.formula setNames
## usethis namespace: end
NULL

utils::globalVariables(c(
  "alter_id", "best_resolution", "cluster_id", "comp_id",
  "compatibility", "compatibility_rate", "consensus_illusion_gap", "delta",
  "delta_u", "ego_id", "internal_compatibility", "label",
  "mean_similarity_rate", "metric", "model_count", "model_id", "pos",
  "prior_compatibility", "score", "stage", "status", "step", "value",
  "x", "xend", "y", "yend"
))

#' Plot a component-status heatmap (models x components)
#'
#' Displays the full model-by-component state matrix as a heatmap, making
#' it easy to see which components are shared, unresolved (unknown), or
#' structurally divergent across the model multiverse.
#'
#' @param result Optional simulation result list from
#'   \code{\link{run_simulation}}.  When supplied, \code{state_data} and
#'   \code{registry_data} are extracted from \code{result$artifacts}.
#' @param states Optional data frame of model-state records.  Must contain
#'   columns \code{model_id}, \code{comp_id}, and \code{status}.  Used when
#'   \code{result} is not supplied.
#' @param registry Optional data frame of component registry records.
#'   Used to determine component type for filtering.
#' @param component_type Which components to show: \code{"edge"},
#'   \code{"node"}, or \code{"all"} (default: \code{"edge"}).
#' @param max_models Maximum number of models to display (default 100).
#'   When exceeded, models are sampled deterministically.
#' @param max_components Maximum number of components to display (default 80).
#'   Components with the highest unknown rate are preferred.
#' @param highlight_components Optional character vector of component IDs
#'   to highlight with a border or marker.
#' @param order_models_by How to order model columns. \code{"input"}
#'   (default) preserves the order in \code{model_ids}.  \code{"status"}
#'   groups by dominant status.  \code{"cluster"} uses cluster assignment
#'   if available.
#' @param order_components_by How to order component rows. \code{"type"}
#'   (default) groups by type then orders by unknown rate descending.
#'   \code{"unknown_rate"} orders all components by unknown rate.
#'   \code{"input"} preserves registry order.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' consensus <- run_simulation("consensus_illusion", n_models = 100)
#' plot_component_status_heatmap(consensus)
#'
#' plot_component_status_heatmap(consensus, component_type = "all")
#' }
#'
#' @export
plot_component_status_heatmap <- function(
    result = NULL,
    states = NULL,
    registry = NULL,
    component_type = c("edge", "all", "node"),
    max_models = 100L,
    max_components = 80L,
    highlight_components = NULL,
    order_models_by = c("input", "status", "cluster"),
    order_components_by = c("type", "unknown_rate", "input"),
    ...) {
  component_type <- match.arg(component_type)
  order_models_by <- match.arg(order_models_by)
  order_components_by <- match.arg(order_components_by)

  state_df <- .resolve_states(result, states)
  registry_df <- .resolve_registry(result, registry, state_df)

  if (!.is_positive_integer_scalar(max_models)) {
    stop("max_models must be a positive integer.", call. = FALSE)
  }
  if (!.is_positive_integer_scalar(max_components)) {
    stop("max_components must be a positive integer.", call. = FALSE)
  }

  if (is.null(highlight_components)) {
    highlight_components <- .auto_highlight_components(result)
  }

  required <- c("model_id", "comp_id", "status")
  missing_cols <- setdiff(required, names(state_df))
  if (length(missing_cols) > 0) {
    stop("states is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (component_type != "all" && !is.null(registry_df) &&
      "type" %in% names(registry_df)) {
    valid_comps <- registry_df$comp_id[registry_df$type == component_type]
    state_df <- state_df[state_df$comp_id %in% valid_comps, , drop = FALSE]
  }

  if (nrow(state_df) == 0) {
    stop("No state rows match component_type = '", component_type, "'.",
         call. = FALSE)
  }

  all_models <- .ordered_models(result, state_df)
  all_comps <- .ordered_components(registry_df, state_df)

  n_models <- length(all_models)
  n_comps <- length(all_comps)

  if (n_models < 2) stop("At least two models are required.", call. = FALSE)

  trunc_models <- if (n_models > max_models) max_models else n_models
  trunc_comps <- if (n_comps > max_components) max_components else n_comps

  if (n_models > max_models) {
    set.seed(42)
    all_models <- sort(sample(all_models, max_models))
  }

  if (n_comps > max_components) {
    unknown_rates <- vapply(all_comps, function(cid) {
      rows <- state_df[state_df$comp_id == cid, , drop = FALSE]
      sum(rows$status == "unknown") / max(1, nrow(rows))
    }, numeric(1))
    names(unknown_rates) <- all_comps
    all_comps <- names(sort(unknown_rates, decreasing = TRUE))[seq_len(max_components)]
  }

  state_df <- state_df[state_df$model_id %in% all_models &
                        state_df$comp_id %in% all_comps, , drop = FALSE]

  if (order_models_by == "input") {
    model_levels <- all_models
  } else if (order_models_by == "status") {
    unknown_rate <- vapply(all_models, function(mid) {
      rows <- state_df[state_df$model_id == mid, , drop = FALSE]
      sum(rows$status == "unknown") / max(1, nrow(rows))
    }, numeric(1))
    model_levels <- all_models[order(unknown_rate, decreasing = TRUE)]
  } else {
    model_levels <- .order_models_by_assignment(result, all_models)
  }

  if (order_components_by == "input") {
    comp_levels <- all_comps
  } else if (order_components_by == "unknown_rate") {
    unknown_rate <- vapply(all_comps, function(cid) {
      rows <- state_df[state_df$comp_id == cid, , drop = FALSE]
      sum(rows$status == "unknown") / max(1, nrow(rows))
    }, numeric(1))
    comp_levels <- all_comps[order(unknown_rate, decreasing = TRUE)]
  } else {
    comp_levels <- .order_components_by_type(registry_df, state_df, all_comps)
  }

  state_df$model_id <- factor(state_df$model_id, levels = model_levels)
  state_df$comp_id <- factor(state_df$comp_id, levels = comp_levels)
  state_df$status <- as.character(state_df$status)

  status_colors <- c(
    causal = "#4393c3",
    present = "#4daf4a",
    unknown = "#d6604d",
    `non-causal` = "grey85",
    absent = "grey95"
  )
  state_df$status_color <- status_colors[state_df$status]
  state_df$status_color[is.na(state_df$status_color)] <- "grey80"

  p <- ggplot2::ggplot(state_df,
    ggplot2::aes(x = model_id, y = comp_id, fill = status)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.1) +
    ggplot2::scale_fill_manual(
      values = status_colors,
      drop = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Model", y = "Component",
      fill = "Status"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 6),
      panel.grid = ggplot2::element_blank()
    )

  if (!is.null(highlight_components) && length(highlight_components) > 0) {
    hl_rows <- state_df[state_df$comp_id %in% highlight_components, ,
                        drop = FALSE]
    if (nrow(hl_rows) > 0) {
      hl_rows <- unique(hl_rows[, c("comp_id", "model_id"), drop = FALSE])
      p <- p + ggplot2::geom_tile(
        data = hl_rows,
        ggplot2::aes(x = model_id, y = comp_id),
        fill = NA, color = "#9970ab", linewidth = 1.0
      )
    }
  }

  subtitle <- NULL
  if (trunc_models < n_models) {
    subtitle <- paste("Showing", trunc_models, "of", n_models, "models")
  }
  if (trunc_comps < n_comps) {
    suffix <- if (is.null(subtitle)) "" else "; "
    subtitle <- paste0(subtitle %||% "",
                       suffix,
                       trunc_comps, " of ", n_comps, " components ",
                       "(highest unknown rate)")
  }
  if (!is.null(subtitle)) {
    p <- p + ggplot2::labs(subtitle = subtitle)
  }

  p
}


.resolve_states <- function(result, states) {
  if (!is.null(states)) {
    if (!is.data.frame(states)) stop("states must be a data frame.", call. = FALSE)
    return(states)
  }
  if (!is.null(result)) {
    if (!is.list(result)) stop("result must be a list.", call. = FALSE)
    s <- result$artifacts$state_data
    if (!is.null(s) && is.data.frame(s)) return(s)
    stop("result does not contain artifacts$state_data.", call. = FALSE)
  }
  stop("Either result or states must be supplied.", call. = FALSE)
}


.resolve_registry <- function(result, registry, state_df) {
  if (!is.null(registry)) {
    if (!is.data.frame(registry)) stop("registry must be a data frame.", call. = FALSE)
    return(registry)
  }
  if (!is.null(result)) {
    if (is.list(result)) {
      r <- result$artifacts$registry_data
      if (!is.null(r) && is.data.frame(r)) return(r)
    }
  }
  comp_ids <- unique(state_df$comp_id)
  data.frame(
    comp_id = comp_ids,
    type = rep("edge", length(comp_ids)),
    stringsAsFactors = FALSE
  )
}


.is_positive_integer_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1 &&
    x == as.integer(x)
}


.auto_highlight_components <- function(result) {
  if (is.null(result) || !is.list(result) || is.null(result$results)) {
    return(NULL)
  }

  if (identical(result$scenario, "lynchpin_of_certainty")) {
    return(result$results$lynchpin_component_id %||% NULL)
  }

  NULL
}


.ordered_models <- function(result, state_df) {
  ids <- NULL
  if (!is.null(result) && is.list(result)) {
    ids <- result$artifacts$model_ids %||% NULL
  }
  if (!is.null(ids)) {
    ids <- as.character(ids)
    ids <- ids[ids %in% state_df$model_id]
  }
  if (is.null(ids) || length(ids) == 0) {
    ids <- unique(as.character(state_df$model_id))
  }
  unique(ids)
}


.ordered_components <- function(registry_df, state_df) {
  ids <- NULL
  if (!is.null(registry_df) && is.data.frame(registry_df) &&
      "comp_id" %in% names(registry_df)) {
    ids <- as.character(registry_df$comp_id)
    ids <- ids[ids %in% state_df$comp_id]
  }
  if (is.null(ids) || length(ids) == 0) {
    ids <- unique(as.character(state_df$comp_id))
  }
  unique(ids)
}


.order_models_by_assignment <- function(result, model_ids) {
  assignments <- NULL
  if (!is.null(result) && is.list(result)) {
    assignments <- result$artifacts$cluster_assignments %||% NULL
  }
  if (is.null(assignments) || !is.data.frame(assignments) ||
      !all(c("model_id", "cluster_id") %in% names(assignments))) {
    return(model_ids)
  }

  assign_map <- stats::setNames(as.character(assignments$cluster_id),
                                as.character(assignments$model_id))
  clusters <- assign_map[model_ids]
  clusters[is.na(clusters)] <- "noise"
  model_ids[order(clusters, model_ids)]
}


.order_components_by_type <- function(registry_df, state_df, comp_ids) {
  unknown_rate <- vapply(comp_ids, function(cid) {
    rows <- state_df[state_df$comp_id == cid, , drop = FALSE]
    sum(rows$status == "unknown") / max(1, nrow(rows))
  }, numeric(1))

  if (is.null(registry_df) || !is.data.frame(registry_df) ||
      !all(c("comp_id", "type") %in% names(registry_df))) {
    return(comp_ids[order(unknown_rate, decreasing = TRUE)])
  }

  type_map <- stats::setNames(as.character(registry_df$type),
                              as.character(registry_df$comp_id))
  type_order <- c("node", "edge")
  type_factor <- factor(type_map[comp_ids], levels = type_order)
  comp_ids[order(type_factor, -unknown_rate, comp_ids)]
}

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Plot a cluster-sorted dyad heatmap
#'
#' Creates a dyad similarity heatmap with models ordered by cluster
#' assignment, revealing block structure that validates detected clusters.
#' Ghost, mainstream, fragmented, and noise clusters are visually
#' separated.
#'
#' @param result Optional simulation result from
#'   \code{run_simulation("ghost_discovery")} created with
#'   \code{include_plot_data = TRUE}.  Uses
#'   \code{result$artifacts$plot_data$dyad_heatmap}.
#' @param dyads Optional dyad data frame.  Must contain \code{ego_id},
#'   \code{alter_id}, and a score column.  Used when \code{result} is
#'   not supplied.
#' @param cluster_assignments Optional data frame with \code{model_id} and
#'   \code{cluster_id} columns.  Required when \code{dyads} is supplied
#'   without \code{result}.
#' @param cluster_labels Optional data frame with \code{cluster_id} and
#'   \code{label} columns.
#' @param score_field Character.  Column name for the heatmap color
#'   score.  Default \code{"similarity_rate"}; simulation plot data may
#'   also supply a precomputed \code{"score"} column.
#' @param max_models Maximum number of models to display (default 200).
#' @param show_labels Logical.  Whether to show model ID labels on axes.
#'   Default \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' ghost <- run_simulation("ghost_discovery",
#'   n_models = 150, include_plot_data = TRUE)
#' plot_clustered_dyad_heatmap(ghost)
#'
#' dyads <- build_dyad_matrix(reg, states, mode = "basic")
#' clusters <- detect_ghost_clusters(dyads, prior_model = "M0001")
#' plot_clustered_dyad_heatmap(
#'   dyads = dyads,
#'   cluster_assignments = clusters$cluster_assignments
#' )
#' }
#'
#' @export
plot_clustered_dyad_heatmap <- function(
    result = NULL,
    dyads = NULL,
    cluster_assignments = NULL,
    cluster_labels = NULL,
    score_field = "similarity_rate",
    max_models = 200L,
    show_labels = FALSE,
    ...) {
  score_field <- match.arg(
    score_field,
    c("similarity_rate", "mas_compatible", "identified_compatible")
  )
  if (!is.null(result) && is.null(dyads)) {
    score_field <- result$results$compatibility_metric %||% score_field
  }
  if (!is.numeric(max_models) || length(max_models) != 1L || is.na(max_models) ||
      max_models < 2 || max_models != as.integer(max_models)) {
    stop("max_models must be an integer of at least 2.", call. = FALSE)
  }

  dyad_df <- .resolve_clustered_dyads(result, dyads, cluster_assignments,
                                        cluster_labels, score_field)

  required <- c("ego_id", "alter_id", "score")
  missing <- setdiff(required, names(dyad_df))
  if (length(missing) > 0) {
    stop("dyad data is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  model_ids <- sort(unique(c(as.character(dyad_df$ego_id),
                             as.character(dyad_df$alter_id))))
  n_models <- length(model_ids)
  if (n_models < 2) {
    stop("At least two models are required.", call. = FALSE)
  }

  if (n_models > max_models) {
    set.seed(42)
    model_ids <- sort(sample(model_ids, max_models))
    dyad_df <- dyad_df[as.character(dyad_df$ego_id) %in% model_ids &
                         as.character(dyad_df$alter_id) %in% model_ids,
                       , drop = FALSE]
    n_models <- length(model_ids)
    if (nrow(dyad_df) == 0) {
      stop("No dyad rows remain after applying max_models.", call. = FALSE)
    }
  }

  ordered <- .order_models_by_cluster(dyad_df, model_ids)
  model_levels <- ordered$model_levels

  dyad_df$ego_id <- factor(as.character(dyad_df$ego_id), levels = model_levels)
  dyad_df$alter_id <- factor(as.character(dyad_df$alter_id), levels = model_levels)

  p <- ggplot2::ggplot(dyad_df,
    ggplot2::aes(x = alter_id, y = ego_id, fill = score)) +
    ggplot2::geom_tile(na.rm = FALSE) +
    ggplot2::scale_fill_gradient(
      low = "#d6604d", high = "#4393c3", na.value = "grey90"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Alter Model", y = "Ego Model",
      fill = score_field
    ) +
    ggplot2::coord_fixed()

  if (!show_labels) {
    p <- p + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1,
                                          size = 5),
      axis.text.y = ggplot2::element_text(size = 5)
    )
  }

  clusters <- ordered$clusters
  if (length(clusters) > 0) {
    breaks <- ordered$cluster_breaks
    if (length(breaks) > 0 && length(breaks) < 30) {
      bdf <- data.frame(
        pos = breaks,
        stringsAsFactors = FALSE
      )
      p <- p + ggplot2::geom_hline(
        data = bdf,
        ggplot2::aes(yintercept = pos),
        color = "white", linewidth = 1.2
      ) + ggplot2::geom_vline(
        data = bdf,
        ggplot2::aes(xintercept = pos),
        color = "white", linewidth = 1.2
      )
    }
  }

  p
}


.order_models_by_cluster <- function(dyad_df, model_ids) {
  has_clusters <- "ego_cluster_id" %in% names(dyad_df) &&
    "alter_cluster_id" %in% names(dyad_df) &&
    any(!is.na(dyad_df$ego_cluster_id) | !is.na(dyad_df$alter_cluster_id))

  if (has_clusters) {
    model_assign <- unique(rbind(
      data.frame(
        model_id = as.character(dyad_df$ego_id),
        cluster_id = as.character(dyad_df$ego_cluster_id),
        label = as.character(dyad_df$ego_label %||% dyad_df$ego_cluster_id),
        stringsAsFactors = FALSE
      ),
      data.frame(
        model_id = as.character(dyad_df$alter_id),
        cluster_id = as.character(dyad_df$alter_cluster_id),
        label = as.character(dyad_df$alter_label %||% dyad_df$alter_cluster_id),
        stringsAsFactors = FALSE
      )
    ))
    model_assign <- model_assign[!duplicated(model_assign$model_id), ,
                                 drop = FALSE]

    label_order <- c("ghost", "mainstream", "fragmented", "noise", "unknown")
    model_assign$sort_label <- factor(model_assign$label,
                                       levels = label_order)
    model_assign <- model_assign[order(model_assign$sort_label,
                                        model_assign$cluster_id,
                                        model_assign$model_id), , drop = FALSE]

    model_levels <- model_assign$model_id

    cluster_ids <- unique(model_assign$cluster_id[!is.na(model_assign$cluster_id)])
    cluster_breaks <- numeric(0)
    for (cid in cluster_ids) {
      last_idx <- max(which(model_assign$cluster_id == cid))
      if (last_idx < length(model_levels)) {
        cluster_breaks <- c(cluster_breaks, last_idx + 0.5)
      }
    }
  } else {
    model_levels <- model_ids
    cluster_breaks <- numeric(0)
    model_assign <- data.frame(
      model_id = model_ids,
      cluster_id = NA_character_,
      label = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  list(
    model_levels = model_levels,
    clusters = model_assign,
    cluster_breaks = cluster_breaks
  )
}


.resolve_clustered_dyads <- function(result, dyads, cluster_assignments,
                                       cluster_labels, score_field) {
  if (!is.null(dyads) && is.data.frame(dyads)) {
    required <- c("ego_id", "alter_id")
    missing <- setdiff(required, names(dyads))
    if (length(missing) > 0) {
      stop("dyads is missing required column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    if (!score_field %in% names(dyads) && !"score" %in% names(dyads)) {
      stop("dyads is missing score column '", score_field,
           "' and fallback column 'score'.", call. = FALSE)
    }
    df <- dyads
    df$score <- if (score_field %in% names(df)) df[[score_field]] else df$score
    df$ego_cluster_id <- NA_character_
    df$alter_cluster_id <- NA_character_
    df$ego_label <- NA_character_
    df$alter_label <- NA_character_

    if (!is.null(cluster_assignments)) {
      if (is.data.frame(cluster_assignments)) {
        if (!all(c("model_id", "cluster_id") %in% names(cluster_assignments))) {
          stop("cluster_assignments must contain model_id and cluster_id.",
               call. = FALSE)
        }
        assign_map <- stats::setNames(
          as.character(cluster_assignments$cluster_id),
          as.character(cluster_assignments$model_id)
        )
        df$ego_cluster_id <- assign_map[as.character(df$ego_id)]
        df$alter_cluster_id <- assign_map[as.character(df$alter_id)]
        df$ego_label <- df$ego_cluster_id
        df$alter_label <- df$alter_cluster_id
      }
    }
    if (!is.null(cluster_labels) && is.data.frame(cluster_labels) &&
        "cluster_id" %in% names(cluster_labels) &&
        "label" %in% names(cluster_labels)) {
      label_map <- stats::setNames(
        as.character(cluster_labels$label),
        as.character(cluster_labels$cluster_id)
      )
      df$ego_label <- label_map[df$ego_cluster_id]
      df$alter_label <- label_map[df$alter_cluster_id]
    }
    return(df)
  }

  if (!is.null(result)) {
    if (!is.list(result)) {
      stop("result must be a list.", call. = FALSE)
    }
    pd <- result$artifacts$plot_data
    if (!is.null(pd$dyad_heatmap) && is.data.frame(pd$dyad_heatmap) &&
        nrow(pd$dyad_heatmap) > 0) {
      df <- pd$dyad_heatmap
      if (!"score" %in% names(df)) {
        if (score_field %in% names(df)) {
          df$score <- df[[score_field]]
        } else {
          stop("dyad_heatmap has no 'score' or '", score_field, "' column.",
               call. = FALSE)
        }
      }
      return(df)
    }
    stop("No dyad_heatmap found. ",
         "Rerun run_simulation('ghost_discovery', ..., ",
         "include_plot_data = TRUE).", call. = FALSE)
  }

  stop("Either result or dyads must be supplied.", call. = FALSE)
}

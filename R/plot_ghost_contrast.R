#' Plot ghost cluster contrast (internal vs prior compatibility)
#'
#' Creates a bubble chart showing where each detected cluster falls in the
#' internal-compatibility vs prior-compatibility plane.  Ghost clusters
#' appear in the upper-left region (high internal, low prior) while
#' mainstream clusters occupy the upper-right.
#'
#' @param result A simulation result list from
#'   \code{run_simulation("ghost_discovery")}.  Uses
#'   \code{result$results$ghost_clusters}.
#' @param internal_threshold Optional numeric threshold for internal
#'   compatibility.  When supplied, a horizontal dashed line is drawn.
#' @param prior_threshold Optional numeric threshold for prior compatibility.
#'   When supplied, a vertical dashed line is drawn.
#' @param label_clusters Logical.  When \code{TRUE} (default), annotate
#'   cluster points with their \code{cluster_id}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' ghost <- run_simulation("ghost_discovery", n_models = 150)
#' plot_ghost_contrast(ghost)
#' plot_ghost_contrast(ghost, internal_threshold = 0.6,
#'   prior_threshold = 0.4)
#' }
#'
#' @export
plot_ghost_contrast <- function(result,
                                 internal_threshold = NULL,
                                 prior_threshold = NULL,
                                 label_clusters = TRUE,
                                 ...) {
  clusters <- .extract_ghost_clusters(result)

  required <- c("cluster_id", "model_count", "internal_compatibility",
                "prior_compatibility", "label")
  missing <- setdiff(required, names(clusters))
  if (length(missing) > 0) {
    stop("ghost_clusters is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  clusters$label <- as.character(clusters$label)
  clusters$label[is.na(clusters$label)] <- "noise"
  clusters$model_count <- as.numeric(clusters$model_count)
  clusters$internal_compatibility <- as.numeric(clusters$internal_compatibility)
  clusters$prior_compatibility <- as.numeric(clusters$prior_compatibility)

  has_labels <- any(c("ghost", "mainstream", "fragmented") %in% clusters$label)

  p <- ggplot2::ggplot(clusters,
    ggplot2::aes(x = prior_compatibility, y = internal_compatibility,
                 size = model_count, color = label))

  if (has_labels) {
    color_values <- c(
      ghost = "#d6604d",
      mainstream = "#4393c3",
      fragmented = "#f4a582",
      noise = "grey80"
    )
    extra_labels <- setdiff(unique(clusters$label), names(color_values))
    for (extra in extra_labels) {
      color_values[[extra]] <- "grey60"
    }
    p <- p + ggplot2::scale_color_manual(values = color_values, drop = FALSE)
  }

  p <- p +
    ggplot2::geom_point(alpha = 0.85) +
    ggplot2::scale_size_continuous(range = c(3, 12)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Prior Compatibility",
      y = "Internal Compatibility",
      size = "Model Count",
      color = "Cluster Type"
    )

  if (!is.null(internal_threshold) && is.numeric(internal_threshold)) {
    p <- p + ggplot2::geom_hline(
      yintercept = internal_threshold,
      linetype = "dashed", color = "grey50", linewidth = 0.5
    )
  }

  if (!is.null(prior_threshold) && is.numeric(prior_threshold)) {
    p <- p + ggplot2::geom_vline(
      xintercept = prior_threshold,
      linetype = "dashed", color = "grey50", linewidth = 0.5
    )
  }

  if (isTRUE(label_clusters)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = cluster_id),
      size = 3, vjust = -1.2, show.legend = FALSE
    )
  }

  p
}


.extract_ghost_clusters <- function(result) {
  if (is.data.frame(result)) {
    if (all(c("cluster_id", "model_count") %in% names(result))) {
      return(result)
    }
    stop("Data frame must be ghost_clusters from a ghost_discovery result.",
         call. = FALSE)
  }

  if (!is.list(result)) {
    stop("result must be a simulation result list or a data frame.",
         call. = FALSE)
  }

  pd <- result$artifacts$plot_data
  clusters <- pd$cluster_contrast %||% NULL
  if (!is.null(clusters) && is.data.frame(clusters) && nrow(clusters) > 0) {
    return(clusters)
  }

  clusters <- result$artifacts$contrast_analysis %||% NULL
  if (!is.null(clusters) && is.data.frame(clusters) && nrow(clusters) > 0) {
    return(clusters)
  }

  clusters <- result$results$ghost_clusters
  if (!is.null(clusters) && is.data.frame(clusters) && nrow(clusters) > 0) {
    return(clusters)
  }

  clusters <- result$ghost_clusters
  if (!is.null(clusters) && is.data.frame(clusters) && nrow(clusters) > 0) {
    return(clusters)
  }

  stop("No ghost_clusters found. Pass a ghost_discovery simulation result.",
       call. = FALSE)
}

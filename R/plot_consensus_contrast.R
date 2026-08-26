#' Plot model-level structural similarity vs causal compatibility
#'
#' Scatter plot showing each model's mean structural similarity to the field
#' versus its selected causal compatibility rate. Models far below the
#' equality line have the largest consensus illusion gap.
#'
#' @param result A simulation result from
#'   \code{run_simulation("consensus_illusion")} created with
#'   \code{include_plot_data = TRUE}.  Uses
#'   \code{result$artifacts$plot_data$model_metrics}, whose columns are
#'   \code{model_id}, \code{mean_similarity_rate},
#'   \code{compatibility_rate}, \code{consensus_illusion_gap}, and
#'   \code{compatibility_metric}.
#' @param label_outliers Logical.  When \code{TRUE} (default), label models
#'   with the largest consensus illusion gap.
#' @param gap_threshold Optional numeric threshold in \code{(0, 1)}. When
#'   supplied, a line marks where mean structural similarity exceeds selected
#'   compatibility by that amount.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' consensus <- run_simulation("consensus_illusion",
#'   n_models = 100, include_plot_data = TRUE)
#' plot_consensus_contrast(consensus)
#' }
#'
#' @export
plot_consensus_contrast <- function(result,
                                     label_outliers = TRUE,
                                     gap_threshold = NULL,
                                     ...) {
  metrics <- .extract_model_metrics(result)

  required <- c(
    "model_id", "mean_similarity_rate", "compatibility_rate",
    "consensus_illusion_gap", "compatibility_metric"
  )
  missing <- setdiff(required, names(metrics))
  if (length(missing) > 0) {
    stop("model_metrics is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  metrics$model_id <- as.character(metrics$model_id)
  metrics$mean_similarity_rate <- as.numeric(metrics$mean_similarity_rate)
  metrics$compatibility_rate <- as.numeric(metrics$compatibility_rate)
  metrics$consensus_illusion_gap <- as.numeric(
    metrics$consensus_illusion_gap
  )
  metric_names <- unique(as.character(metrics$compatibility_metric))
  metric_names <- metric_names[!is.na(metric_names) & nzchar(metric_names)]
  if (length(metric_names) != 1L) {
    stop("model_metrics must contain exactly one compatibility_metric.",
         call. = FALSE)
  }

  p <- ggplot2::ggplot(metrics,
    ggplot2::aes(x = mean_similarity_rate, y = compatibility_rate,
                 color = consensus_illusion_gap), ...) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_color_gradient2(
      low = "#4393c3", mid = "grey85", high = "#d6604d",
      midpoint = 0, name = "Consensus Illusion Gap"
    ) +
    ggplot2::geom_abline(linetype = "dashed", color = "grey50", linewidth = 0.6) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Mean Structural Similarity",
      y = paste(.simulation_metric_label(metric_names), "Rate"),
      color = "Consensus Illusion Gap"
    )

  if (!is.null(gap_threshold) && is.numeric(gap_threshold) &&
      gap_threshold > 0 && gap_threshold < 1) {
    p <- p + ggplot2::geom_abline(
      intercept = -gap_threshold, slope = 1,
      linetype = "dotted", color = "#d6604d", linewidth = 0.6
    ) + ggplot2::annotate(
      "text",
      x = 0.98, y = 0.98 - gap_threshold,
      label = sprintf("Gap = %.2f", gap_threshold),
      hjust = 1, vjust = 1, size = 3.5,
      color = "#d6604d", fontface = "italic"
    )
  }

  if (isTRUE(label_outliers)) {
    finite <- is.finite(metrics$consensus_illusion_gap)
    if (any(finite)) {
      cutoff <- stats::quantile(
        metrics$consensus_illusion_gap[finite], 0.9,
        na.rm = TRUE, names = FALSE
      )
      outliers <- metrics[
        finite & metrics$consensus_illusion_gap >= cutoff, , drop = FALSE
      ]
      outliers <- outliers[
        order(outliers$consensus_illusion_gap, decreasing = TRUE), ,
        drop = FALSE
      ]
      outliers <- utils::head(outliers, 15L)
      if (nrow(outliers) > 0) {
        p <- p + ggplot2::geom_text(
          data = outliers,
          ggplot2::aes(label = model_id),
          size = 2.8, vjust = -1, show.legend = FALSE
        )
      }
    }
  }

  p
}


.extract_model_metrics <- function(result) {
  if (is.data.frame(result)) {
    return(result)
  }

  if (!is.list(result)) {
    stop("result must be a simulation result list or a data frame.",
         call. = FALSE)
  }

  pd <- result$artifacts$plot_data
  if (!is.null(pd$model_metrics) && is.data.frame(pd$model_metrics) &&
      nrow(pd$model_metrics) > 0) {
    return(pd$model_metrics)
  }

  stop("No model_metrics found. ",
       "Rerun run_simulation('consensus_illusion', ..., ",
       "include_plot_data = TRUE).", call. = FALSE)
}

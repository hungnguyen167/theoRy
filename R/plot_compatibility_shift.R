`%||%` <- function(x, y) if (is.null(x)) y else x


#' Plot pre/post lynchpin compatibility shift
#'
#' Shows the distributional phase transition caused by resolving the
#' lynchpin component.  Instead of only a two-point timeline of means,
#' this plot reveals how individual dyad compatibility scores shift.
#'
#' @param result A simulation result from
#'   \code{run_simulation("lynchpin_of_certainty")} created with
#'   \code{include_plot_data = TRUE}.  Uses
#'   \code{result$artifacts$plot_data$pairwise_shift}.
#' @param geom Type of plot: \code{"density"} (default), \code{"violin"},
#'   or \code{"histogram"}.
#' @param show_delta Logical.  When \code{TRUE} (default), annotate the
#'   phase transition score and lynchpin component ID.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' lynchpin <- run_simulation("lynchpin_of_certainty",
#'   n_models = 200, include_plot_data = TRUE)
#' plot_compatibility_shift(lynchpin)
#' plot_compatibility_shift(lynchpin, geom = "violin")
#' }
#'
#' @export
plot_compatibility_shift <- function(result,
                                      geom = c("density", "violin", "histogram"),
                                      show_delta = TRUE,
                                      ...) {
  geom <- match.arg(geom)
  shift <- .extract_pairwise_shift(result)
  metric_name <- result$results$compatibility_metric %||% "similarity_rate"
  metric_label <- gsub("_", " ", metric_name, fixed = TRUE)

  required <- c("ego_id", "alter_id", "baseline_score", "post_score", "delta")
  missing <- setdiff(required, names(shift))
  if (length(missing) > 0) {
    stop("pairwise_shift is missing required column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  shift$baseline_score <- as.numeric(shift$baseline_score)
  shift$post_score <- as.numeric(shift$post_score)
  shift$delta <- as.numeric(shift$delta)

  long <- data.frame(
    score = c(shift$baseline_score, shift$post_score),
    stage = factor(
      rep(c("Baseline", "Post-Resolution"), each = nrow(shift)),
      levels = c("Baseline", "Post-Resolution")
    ),
    stringsAsFactors = FALSE
  )

  if (identical(geom, "density")) {
    p <- ggplot2::ggplot(long,
      ggplot2::aes(x = score, fill = stage, color = stage)) +
      ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
      ggplot2::scale_fill_manual(
        values = c(Baseline = "#d6604d", `Post-Resolution` = "#4393c3")
      ) +
      ggplot2::scale_color_manual(
        values = c(Baseline = "#d6604d", `Post-Resolution` = "#4393c3")
      ) +
      ggplot2::xlim(0, 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = paste(metric_label, "score"), y = "Density",
        fill = "Stage", color = "Stage"
      )
  } else if (identical(geom, "violin")) {
    p <- ggplot2::ggplot(long,
      ggplot2::aes(x = stage, y = score, fill = stage)) +
      ggplot2::geom_violin(alpha = 0.7, draw_quantiles = 0.5) +
      ggplot2::scale_fill_manual(
        values = c(Baseline = "#d6604d", `Post-Resolution` = "#4393c3")
      ) +
      ggplot2::ylim(0, 1) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "", y = paste(metric_label, "score"),
        fill = "Stage"
      )
  } else {
    p <- ggplot2::ggplot(shift,
      ggplot2::aes(x = delta)) +
      ggplot2::geom_histogram(
        bins = 40, fill = "#4393c3", alpha = 0.7,
        color = "white", linewidth = 0.2
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                          color = "grey50") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = "Compatibility Delta (Post - Baseline)",
        y = "Count"
      )
  }

  if (isTRUE(show_delta)) {
    lynchpin_id <- result$results$lynchpin_component_id
    phase_score <- result$results$phase_transition_score

    if (!is.null(phase_score) && is.numeric(phase_score)) {
      label <- paste0("Lynchpin: ", lynchpin_id %||% "?",
                      "\nPhase transition: ", round(phase_score, 3))
      p <- p + ggplot2::annotate(
        "text",
        label = label,
        x = Inf, y = Inf, hjust = 1.05, vjust = 1.2,
        size = 3.2, color = "#d6604d", fontface = "bold"
      )
    }
  }

  p
}


.extract_pairwise_shift <- function(result) {
  if (is.data.frame(result)) {
    if (all(c("baseline_score", "post_score") %in% names(result))) {
      return(result)
    }
    stop("Data frame must be pairwise_shift from a lynchpin simulation result.",
         call. = FALSE)
  }

  if (!is.list(result)) {
    stop("result must be a simulation result list or a data frame.",
         call. = FALSE)
  }

  pd <- result$artifacts$plot_data
  if (!is.null(pd$pairwise_shift) && is.data.frame(pd$pairwise_shift) &&
      nrow(pd$pairwise_shift) > 0) {
    return(pd$pairwise_shift)
  }

  stop("No pairwise_shift found. ",
       "Rerun run_simulation('lynchpin_of_certainty', ..., ",
       "include_plot_data = TRUE).", call. = FALSE)
}

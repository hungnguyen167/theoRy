#' Plot a compatibility timeline (phase transition)
#'
#' Creates a line-and-point chart showing global compatibility at each
#' resolution step. Used to visualize phase transitions where resolving a
#' single component dramatically increases compatibility.
#'
#' @param result A simulation result list from
#'   \code{\link{run_simulation}("lynchpin_of_certainty")} (in which case the
#'   compatibility timeline is extracted from
#'   \code{result$results$compatibility_timeline} and the phase transition
#'   score from \code{result$results$phase_transition_score}), or a bare
#'   data frame with columns \code{step} and \code{compatibility}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' lynchpin <- run_simulation("lynchpin_of_certainty", n_models = 200)
#' plot_compatibility_timeline(lynchpin)
#'
#' # Bare data frame also works
#' df <- data.frame(step = c("baseline", "resolve_C0003", "post"),
#'                  compatibility = c(0.42, 0.61, 0.88))
#' plot_compatibility_timeline(df)
#' }
#'
#' @export
plot_compatibility_timeline <- function(result,
                                         ...) {
  timeline <- NULL
  phase_score <- NULL

  if (is.data.frame(result)) {
    if (!all(c("step", "compatibility") %in% names(result))) {
      stop("When result is a data frame, it must have 'step' and ",
           "'compatibility' columns.", call. = FALSE)
    }
    timeline <- result
  } else if (is.list(result)) {
    if (!is.null(result$results$compatibility_timeline) &&
        is.data.frame(result$results$compatibility_timeline)) {
      timeline <- result$results$compatibility_timeline
    } else if (!is.null(result$compatibility_timeline) &&
               is.data.frame(result$compatibility_timeline)) {
      timeline <- result$compatibility_timeline
    }

    if (is.null(timeline)) {
      stop("Could not find compatibility_timeline in result. Pass a ",
           "simulation result from run_simulation('lynchpin_of_certainty') ",
           "or a data frame with 'step' and 'compatibility' columns.",
           call. = FALSE)
    }

    phase_score <- result$results$phase_transition_score %||%
      result$phase_transition_score %||% NULL
  } else {
    stop("result must be a data frame or a simulation result list.",
         call. = FALSE)
  }

  if (nrow(timeline) == 0) {
    stop("compatibility_timeline has no rows to plot.", call. = FALSE)
  }

  # Preserve appearance order (not alphabetical) for the step factor
  timeline$step <- factor(timeline$step, levels = unique(timeline$step))
  timeline$compatibility <- as.numeric(timeline$compatibility)

  p <- ggplot2::ggplot(timeline,
                       ggplot2::aes(step, compatibility, group = 1)) +
    ggplot2::geom_line(color = "#4393c3", linewidth = 0.8) +
    ggplot2::geom_point(size = 3, color = "#4393c3") +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", compatibility)),
      vjust = -1, size = 3.2
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Resolution Step", y = "Global Compatibility"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )

  if (!is.null(phase_score) && is.numeric(phase_score) && length(phase_score) == 1) {
    p <- p + ggplot2::annotate(
      "text",
      label = paste0("Phase transition: ", round(phase_score, 3)),
      x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5,
      color = "#d6604d", fontface = "bold"
    )
  }

  p
}

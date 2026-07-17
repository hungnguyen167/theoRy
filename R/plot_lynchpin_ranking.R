`%||%` <- function(x, y) if (is.null(x)) y else x


#' Plot a lynchpin component ranking bar chart
#'
#' Creates a horizontal bar chart of Delta-U scores showing which theoretical
#' components, if resolved, would maximize global compatibility. Bars are
#' ordered by \code{delta_u} descending and colored by \code{best_resolution}.
#'
#' @param rankings A data frame from \code{\link{compute_delta_u}} or
#'   a Delta-U result or a simulation result that provides a lynchpin ranking.
#'   Must contain columns \code{component_id}, \code{delta_u}, and
#'   \code{best_resolution}; \code{type}, \code{source}, and \code{target}
#'   are used for bar labels when available.
#' @param top_n Integer or \code{NULL}. When provided, show only the top N
#'   components by \code{delta_u} descending. When \code{NULL} (default),
#'   show all components (defaulting to 20 with a message when more than 20
#'   are present).
#' @param synergistic_sets Optional data frame from
#'   \code{\link{compute_delta_u}(..., synergistic_set_size = ...)$synergistic_sets}.
#'   Must contain \code{components}, \code{delta_u_combined}, and
#'   \code{label}. When supplied, synergistic set bars are appended below
#'   the individual bars with reduced alpha.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' reg <- build_component_registry(c("X", "Y", "Z"), timing = c(1, 2, 3))
#' states <- expand_model_states(reg, mode = "exhaustive")
#' dyads <- build_dyad_matrix(reg, states, mode = "basic")
#' rankings <- compute_delta_u(dyads, top_k = 10)
#' plot_lynchpin_ranking(rankings)
#' plot_lynchpin_ranking(rankings, top_n = 5)
#' }
#'
#' @export
plot_lynchpin_ranking <- function(rankings,
                                   top_n = NULL,
                                   synergistic_sets = NULL,
                                   ...) {
  if (!is.data.frame(rankings)) {
    stop("rankings must be a data frame from compute_delta_u() or ",
         "a Delta-U or lynchpin simulation result.", call. = FALSE)
  }

  required <- c("component_id", "delta_u", "best_resolution")
  missing_cols <- setdiff(required, names(rankings))
  if (length(missing_cols) > 0) {
    stop("rankings is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  n_total <- nrow(rankings)
  if (n_total == 0) {
    stop("rankings has no rows to plot.", call. = FALSE)
  }

  # Apply top_n filtering
  if (is.null(top_n)) {
    if (n_total > 20) {
      top_n <- 20L
      message("rankings has ", n_total,
              " components; showing top 20. Pass top_n to override.")
    } else {
      top_n <- n_total
    }
  } else {
    if (!is.numeric(top_n) || top_n <= 0) {
      stop("top_n must be a positive integer or NULL.", call. = FALSE)
    }
    top_n <- as.integer(top_n)
  }

  # Sort by delta_u descending then take top_n
  rankings <- rankings[order(-rankings$delta_u), , drop = FALSE]
  rankings <- rankings[seq_len(min(top_n, nrow(rankings))), , drop = FALSE]

  # Build human-readable labels
  rankings$label <- .format_component_label(rankings)

  # Ensure best_resolution is a factor with consistent ordering
  resolutions <- as.character(rankings$best_resolution)
  resolutions[is.na(resolutions)] <- "none"
  rankings$best_resolution <- factor(
    resolutions, levels = c("positive", "negative", "none", "synergy")
  )

  # Order labels by delta_u ascending so highest is at top after coord_flip
  rankings$label <- factor(rankings$label, levels = rankings$label[order(rankings$delta_u)])

  subtitle <- NULL
  if (top_n < n_total) {
    subtitle <- paste("Showing top", top_n, "of", n_total, "components")
  }

  p <- ggplot2::ggplot(rankings,
                       ggplot2::aes(x = label, y = delta_u, fill = best_resolution)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(positive = "#4393c3", negative = "#d6604d",
                 none = "grey70", synergy = "#9970ab"),
      drop = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "", y = "Delta-U (compatibility improvement)",
      fill = "Resolution",
      subtitle = subtitle
    )

  # Append synergistic set bars if provided
  if (!is.null(synergistic_sets) && is.data.frame(synergistic_sets) &&
      nrow(synergistic_sets) > 0) {
    syn_required <- c("components", "delta_u_combined", "label")
    syn_missing <- setdiff(syn_required, names(synergistic_sets))
    if (length(syn_missing) > 0) {
      warning("synergistic_sets is missing column(s): ",
              paste(syn_missing, collapse = ", "),
              "; ignoring synergistic bars.", call. = FALSE)
    } else {
      syn_df <- synergistic_sets
      syn_df$best_resolution <- factor(
        "synergy", levels = c("positive", "negative", "none", "synergy")
      )
      syn_df$delta_u <- syn_df$delta_u_combined
      # Order after individual bars
      syn_df$label <- factor(
        as.character(syn_df$label),
        levels = c(levels(rankings$label), as.character(syn_df$label))
      )
      p <- p + ggplot2::geom_col(
        data = syn_df,
        mapping = ggplot2::aes(x = label, y = delta_u, fill = best_resolution),
        alpha = 0.5
      )
    }
  }

  p
}


.format_component_label <- function(rankings) {
  comp_id <- as.character(rankings$component_id)
  source <- if ("source" %in% names(rankings)) as.character(rankings$source) else rep("", nrow(rankings))
  target <- if ("target" %in% names(rankings)) as.character(rankings$target) else rep(NA, nrow(rankings))
  type <- if ("type" %in% names(rankings)) as.character(rankings$type) else rep("edge", nrow(rankings))

  labels <- vapply(seq_len(nrow(rankings)), function(i) {
    if (!is.na(target[i]) && nzchar(target[i])) {
      paste0(comp_id[i], " (", source[i], " \u2192 ", target[i], ")")
    } else if (!is.na(source[i]) && nzchar(source[i])) {
      paste0(comp_id[i], " (", source[i], ")")
    } else {
      comp_id[i]
    }
  }, character(1), USE.NAMES = FALSE)

  labels
}

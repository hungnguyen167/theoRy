#' Plot a dyad similarity heatmap
#'
#' Creates an M x M heatmap of dyadic comparison scores from a dyad matrix
#' returned by \code{\link{build_dyad_matrix}}. The heatmap is colored by the
#' chosen score field (default \code{similarity_rate}) and can be used to
#' quickly identify clusters of similar or compatible theories.
#'
#' @param dyads A data frame returned by \code{\link{build_dyad_matrix}}.
#' @param score_field Character. Column name to use for the heatmap color. Must
#'   be exactly one of \code{"similarity_rate"}, \code{"mas_compatible"}, or
#'   \code{"identified_compatible"}. Defaults to \code{"similarity_rate"}.
#' @param show_labels Logical. Whether to show model ID labels on axes.
#'   Defaults to \code{TRUE}. Automatically set to \code{FALSE} when
#'   the multiverse has more than 100 models.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' reg <- build_component_registry(c("X", "Y", "Z"), timing = c(1, 2, 3))
#' states <- expand_model_states(reg, mode = "exhaustive",
#'   edge_statuses = c("causal", "unknown"))
#' dyads <- build_dyad_matrix(reg, states, mode = "full",
#'   exposure = "X", outcome = "Z")
#' plot_dyad_heatmap(dyads)
#' plot_dyad_heatmap(dyads, score_field = "identified_compatible")
#' }
#'
#' @export
plot_dyad_heatmap <- function(dyads,
                               score_field = "similarity_rate",
                               show_labels = TRUE,
                               ...) {
  if (!is.data.frame(dyads)) {
    stop("dyads must be a data frame returned by build_dyad_matrix().",
         call. = FALSE)
  }

  valid_score_fields <- c(
    "similarity_rate", "mas_compatible", "identified_compatible"
  )
  if (!is.character(score_field) || length(score_field) != 1L ||
      is.na(score_field) || !score_field %in% valid_score_fields) {
    stop(
      "score_field must be one of: ",
      paste(valid_score_fields, collapse = ", "), ".",
      call. = FALSE
    )
  }

  required <- c("ego_id", "alter_id", score_field)
  missing_cols <- setdiff(required, names(dyads))
  if (length(missing_cols) > 0) {
    stop("dyads is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  model_ids <- sort(unique(c(as.character(dyads$ego_id),
                             as.character(dyads$alter_id))))
  n_models <- length(model_ids)

  if (n_models < 2) {
    stop("At least two models are required to build a heatmap.",
         call. = FALSE)
  }

  # Auto-hide labels for large multiverses unless explicitly requested
  labels_explicit <- !missing(show_labels)
  if (n_models > 100 && (!labels_explicit || !isTRUE(show_labels))) {
    show_labels <- FALSE
    message("Multiverse has ", n_models,
            " models; hiding axis labels. Set show_labels = TRUE to override.")
  }

  # Build long-format data frame for geom_tile()
  # Diagonal (self-pairs) are NA and will be rendered grey.
  long_df <- data.frame(
    ego_id = factor(as.character(dyads$ego_id), levels = model_ids),
    alter_id = factor(as.character(dyads$alter_id), levels = model_ids),
    score = dyads[[score_field]],
    stringsAsFactors = FALSE
  )

  # Determine if score field is boolean-valued -> discrete scale
  vals <- long_df$score
  is_boolean <- is.logical(vals) ||
    (is.numeric(vals) && all(vals %in% c(0, 1, NA, TRUE, FALSE)))

  p <- ggplot2::ggplot(long_df,
                       ggplot2::aes(x = alter_id, y = ego_id, fill = score))

  if (is_boolean) {
    # Normalize to logical for consistent scale
    if (is.numeric(vals)) {
      long_df$score <- as.logical(long_df$score)
    }
    p <- ggplot2::ggplot(long_df,
                         ggplot2::aes(x = alter_id, y = ego_id, fill = score))
    p <- p + ggplot2::geom_tile(na.rm = FALSE) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#4393c3", "FALSE" = "#d6604d"),
        na.value = "grey90",
        labels = c("TRUE" = "TRUE", "FALSE" = "FALSE"),
        drop = FALSE
      )
  } else {
    p <- p + ggplot2::geom_tile(na.rm = FALSE) +
      ggplot2::scale_fill_gradient(
        low = "#d6604d", high = "#4393c3", na.value = "grey90"
      )
  }

  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Alter Model", y = "Ego Model",
      fill = score_field
    )

  if (!show_labels) {
    p <- p + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  }

  # Fix aspect ratio so the heatmap is square
  p <- p + ggplot2::coord_fixed()

  p
}

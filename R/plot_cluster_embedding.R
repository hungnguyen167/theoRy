`%||%` <- function(x, y) if (is.null(x)) y else x


#' Plot a UMAP cluster embedding
#'
#' Creates a 2D scatter plot of model embeddings produced by
#' \code{\link{detect_ghost_clusters}}, with points colored by cluster label
#' (ghost, mainstream, fragmented, or noise). Optionally highlights a user
#' prior model.
#'
#' @param cluster_result A list returned by
#'   \code{\link{detect_ghost_clusters}}, containing \code{embedding_2d}
#'   (data frame with \code{model_id}, \code{x}, \code{y}) and
#'   \code{cluster_assignments} (data frame with \code{model_id},
#'   \code{cluster_id}). May also contain \code{ghost_clusters} with a
#'   \code{label} column.
#' @param show_labels Logical. When \code{TRUE}, annotate cluster centroids
#'   with their cluster ID and label. Defaults to \code{FALSE}.
#' @param highlight_prior Logical. When \code{TRUE}, mark the prior model
#'   with a diamond marker. Requires \code{prior_model_id} to be supplied
#'   or discoverable on \code{cluster_result}.
#' @param prior_model_id Optional model ID (e.g. \code{"M0001"}) to mark as
#'   the user prior. Falls back to \code{cluster_result$prior_model_id}
#'   when available.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{ggplot}}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' reg <- build_component_registry(c("X", "Y", "Z"), timing = c(1, 2, 3))
#' states <- expand_model_states(reg, mode = "sampled", n_models = 100)
#' dyads <- build_dyad_matrix(reg, states, mode = "basic")
#' ghosts <- detect_ghost_clusters(dyads, prior_model = "M0001")
#' plot_cluster_embedding(ghosts)
#' plot_cluster_embedding(ghosts, show_labels = TRUE,
#'   highlight_prior = TRUE, prior_model_id = "M0001")
#' }
#'
#' @export
plot_cluster_embedding <- function(cluster_result,
                                    show_labels = FALSE,
                                    highlight_prior = FALSE,
                                    prior_model_id = NULL,
                                    ...) {
  if (!is.list(cluster_result)) {
    stop("cluster_result must be a list returned by detect_ghost_clusters().",
         call. = FALSE)
  }

  embedding <- cluster_result$embedding_2d
  if (is.null(embedding) || !is.data.frame(embedding) || nrow(embedding) == 0) {
    stop("No embedding data. Run detect_ghost_clusters() first.",
         call. = FALSE)
  }

  assignments <- cluster_result$cluster_assignments
  if (is.null(assignments) || !is.data.frame(assignments)) {
    stop("cluster_result is missing cluster_assignments.",
         call. = FALSE)
  }

  # Merge embedding with cluster assignments
  df <- merge(embedding, assignments, by = "model_id", all.x = TRUE)

  # Attach ghost cluster labels if available
  ghost_clusters <- cluster_result$ghost_clusters
  has_labels <- !is.null(ghost_clusters) && is.data.frame(ghost_clusters) &&
    "label" %in% names(ghost_clusters) && "cluster_id" %in% names(ghost_clusters)

  if (has_labels) {
    label_df <- ghost_clusters[, c("cluster_id", "label")]
    df <- merge(df, label_df, by = "cluster_id", all.x = TRUE)
  } else {
    df$label <- df$cluster_id
  }

  # Detect all-NA cluster assignments
  if (all(is.na(df$cluster_id))) {
    message("No clusters detected - all models are noise.")
    df$label <- "noise"
  }

  # Resolve prior model ID
  prior_id <- prior_model_id %||% cluster_result$prior_model_id %||% NULL

  # Default label for NA clusters
  df$label <- as.character(df$label)
  df$label[is.na(df$label)] <- "noise"

  # Build color palette
  if (has_labels) {
    color_values <- c(
      ghost = "#d6604d",
      mainstream = "#4393c3",
      fragmented = "#f4a582",
      noise = "grey80"
    )
    # Ensure any label not in the canonical set gets a fallback color
    unique_labels <- unique(df$label)
    extra_labels <- setdiff(unique_labels, names(color_values))
    for (extra in extra_labels) {
      color_values[extra] <- "grey80"
    }
  } else {
    # Qualitative palette indexed by cluster_id
    unique_clusters <- sort(unique(df$cluster_id))
    qualitative <- c("#4393c3", "#d6604d", "#f4a582", "#92c5de",
                     "#fddbc7", "#2166ac", "#b2182b", "#f7f7f7")
    color_values <- stats::setNames(
      rep(qualitative, length.out = max(1, length(unique_clusters))),
      unique_clusters
    )
    df$label <- factor(df$label, levels = unique_clusters)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = label)) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "UMAP 1", y = "UMAP 2",
      color = "Cluster Type"
    )

  # Highlight prior model
  if (isTRUE(highlight_prior) && !is.null(prior_id)) {
    prior_df <- df[df$model_id == prior_id, , drop = FALSE]
    if (nrow(prior_df) > 0) {
      p <- p + ggplot2::geom_point(
        data = prior_df,
        ggplot2::aes(x = x, y = y),
        shape = 18, size = 5, color = "black", inherit.aes = FALSE
      )
    }
  }

  # Annotate cluster centroids
  if (isTRUE(show_labels)) {
    centroids <- stats::aggregate(
      cbind(x, y) ~ label, data = df, FUN = mean, na.rm = TRUE
    )
    if (nrow(centroids) > 0) {
      p <- p + ggplot2::geom_text(
        data = centroids,
        ggplot2::aes(x = x, y = y, label = label),
        inherit.aes = FALSE, size = 3.5, fontface = "bold",
        vjust = -1.2, color = "black"
      )
    }
  }

  p
}

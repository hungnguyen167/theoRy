#' Plot simulation results
#'
#' Scenario-aware wrapper that dispatches to the appropriate plot functions
#' based on the simulation scenario. Returns a named list of \code{ggplot}
#' objects.
#'
#' @param result A list returned by \code{\link{run_simulation}}. The
#'   \code{$scenario} field determines which plots are produced.
#' @param ... Additional arguments passed to the underlying plot functions.
#'
#' @return A named list of \code{ggplot} objects. Element names depend on
#'   the scenario:
#'   \describe{
#'     \item{\code{consensus_illusion}}{\code{consensus_comparison}}
#'       (mean structural similarity vs selected causal compatibility)
#'     \item{\code{lynchpin_of_certainty}}{\code{compatibility_timeline},
#'       \code{lynchpin_ranking}}
#'     \item{\code{ghost_discovery}}{\code{cluster_sizes},
#'       \code{cluster_embedding} (when embedding data is available)}
#'   }
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' consensus <- run_simulation("consensus_illusion", n_models = 100)
#' plots <- plot_simulation(consensus)
#' for (p in plots) print(p)
#'
#' lynchpin <- run_simulation("lynchpin_of_certainty", n_models = 200)
#' plots <- plot_simulation(lynchpin)
#'
#' ghost <- run_simulation("ghost_discovery", n_models = 150)
#' plots <- plot_simulation(ghost)
#' }
#'
#' @export
plot_simulation <- function(result, ...) {
  if (!is.list(result) || is.null(result$scenario)) {
    stop("result must be a simulation result list from run_simulation().",
         call. = FALSE)
  }

  scenario <- result$scenario

  if (identical(scenario, "consensus_illusion")) {
    .plot_simulation_consensus(result)
  } else if (scenario %in% c("lynchpin_of_certainty", "crux_of_certainty")) {
    .plot_simulation_lynchpin(result)
  } else if (identical(scenario, "ghost_discovery")) {
    .plot_simulation_ghost(result)
  } else {
    stop("Unrecognized simulation result: scenario = '",
         scenario, "'. Expected one of: consensus_illusion, ",
         "lynchpin_of_certainty, crux_of_certainty, ghost_discovery.",
         call. = FALSE)
  }
}


.plot_simulation_consensus <- function(result) {
  plots <- list()

  similarity <- result$results$mean_similarity_rate
  compatibility <- result$results$compatibility_rate
  metric_name <- result$results$compatibility_metric
  if (!is.null(similarity) && !is.null(compatibility) &&
      !is.null(metric_name)) {
    df <- data.frame(
      metric = c(
        "Mean Structural Similarity",
        .simulation_metric_label(metric_name)
      ),
      value = c(similarity, compatibility),
      stringsAsFactors = FALSE
    )
    plots$consensus_comparison <- ggplot2::ggplot(
      df, ggplot2::aes(x = metric, y = value, fill = metric)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_y_continuous(limits = c(0, 1)) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(c("#d6604d", "#4393c3"), df$metric)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        x = NULL, y = "Rate", fill = "Metric",
        title = "Consensus Illusion"
      ) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 15, hjust = 1)
      )
  }

  plots
}


.plot_simulation_lynchpin <- function(result) {
  plots <- list()

  # Compatibility timeline
  plots$compatibility_timeline <- plot_compatibility_timeline(result)

  rankings <- result$artifacts$rankings
  if (is.data.frame(rankings) && nrow(rankings) > 0) {
    plots$lynchpin_ranking <- plot_lynchpin_ranking(rankings)
  } else {
    # Older responses expose only the top-ranked component.
    lynchpin_id <- result$results$lynchpin_component_id
    phase_score <- result$results$phase_transition_score
    if (is.null(lynchpin_id) || is.null(phase_score)) {
      return(plots)
    }
    one_row <- data.frame(
      rank = 1L,
      component_id = lynchpin_id,
      type = "edge",
      source = "",
      target = "",
      delta_u = phase_score,
      best_resolution = "causal",
      dyads_improved = 0L,
      dyads_worsened = 0L,
      stringsAsFactors = FALSE
    )
    plots$lynchpin_ranking <- plot_lynchpin_ranking(one_row)
  }

  plots
}


.plot_simulation_ghost <- function(result) {
  plots <- list()

  ghost_clusters <- result$results$ghost_clusters

  # Cluster sizes bar chart
  if (!is.null(ghost_clusters) && is.data.frame(ghost_clusters) &&
      nrow(ghost_clusters) > 0) {
    df <- ghost_clusters
    df$label <- as.character(df$label)
    plots$cluster_sizes <- ggplot2::ggplot(
      df, ggplot2::aes(x = cluster_id, y = model_count, fill = label)
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(
        values = c(ghost = "#d6604d", mainstream = "#4393c3",
                   fragmented = "#f4a582", noise = "grey80"),
        na.value = "grey80"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Cluster", y = "Model Count", fill = "Cluster Type") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
      )
  }

  # Cluster embedding - only if embedding data is available
  cluster_result <- .build_cluster_result_from_simulation(result)
  if (!is.null(cluster_result)) {
    plots$cluster_embedding <- plot_cluster_embedding(cluster_result)
  } else {
    message("Cluster embedding not available for this simulation result. ",
            "Use detect_ghost_clusters() directly for UMAP embeddings.")
  }

  plots
}


.build_cluster_result_from_simulation <- function(result) {
  # The simulation result may expose embedding/assignment data in various
  # locations depending on the backend version. Check all known locations.
  embedding <- NULL
  assignments <- NULL
  ghost_clusters <- result$results$ghost_clusters

  # Check artifacts for embedding and cluster assignments
  if (!is.null(result$artifacts)) {
    artifacts <- result$artifacts

    # embedding_2d may be a list with model_ids, x, y
    emb <- artifacts$embedding_2d %||% NULL
    if (!is.null(emb)) {
      if (is.data.frame(emb)) {
        embedding <- emb
      } else if (is.list(emb)) {
        model_ids <- unlist(emb$model_ids %||% emb$model_id %||% NULL)
        xs <- unlist(emb$x %||% NULL)
        ys <- unlist(emb$y %||% NULL)
        if (!is.null(model_ids) && !is.null(xs) && !is.null(ys) &&
            length(model_ids) == length(xs) && length(xs) == length(ys)) {
          embedding <- data.frame(
            model_id = as.character(model_ids),
            x = as.numeric(xs),
            y = as.numeric(ys),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # cluster_assignments may be a list of records or a data frame
    ca <- artifacts$cluster_assignments %||% NULL
    if (!is.null(ca)) {
      if (is.data.frame(ca)) {
        assignments <- ca
      } else if (is.list(ca) && length(ca) > 0) {
        assignments <- tryCatch(
          data.frame(
            model_id = vapply(ca, function(x) as.character(x$model_id),
                              character(1)),
            cluster_id = vapply(ca, function(x) as.character(x$cluster_id),
                                character(1)),
            stringsAsFactors = FALSE
          ),
          error = function(e) NULL
        )
      }
    }
  }

  if (is.null(embedding) || nrow(embedding) == 0) {
    return(NULL)
  }

  if (is.null(assignments)) {
    assignments <- data.frame(
      model_id = embedding$model_id,
      cluster_id = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  list(
    embedding_2d = embedding,
    cluster_assignments = assignments,
    ghost_clusters = ghost_clusters
  )
}

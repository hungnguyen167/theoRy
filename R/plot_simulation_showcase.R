#' Plot simulation showcase figures
#'
#' Scenario-aware wrapper that produces additional explanatory figures
#' beyond the default \code{\link{plot_simulation}} output.  These showcase
#' plots are designed for manuscripts, demos, and internal validation.
#'
#' Low-cost plots (component-status heatmap, ghost contrast, cluster
#' embedding) work from existing simulation artifacts.  High-cost plots
#' (consensus contrast, compatibility shift, clustered dyad heatmap)
#' require \code{run_simulation(..., include_plot_data = TRUE)}.
#'
#' @param result A list returned by \code{\link{run_simulation}}.
#' @param ... Additional arguments passed to underlying plot functions.
#' @param strict Logical.  When \code{FALSE} (default), unavailable
#'   optional plots are omitted with a message.  When \code{TRUE},
#'   missing required plot data causes an error.
#'
#' @return A named list of \code{ggplot} objects.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' ghost <- run_simulation("ghost_discovery", n_models = 150)
#' plots <- plot_simulation_showcase(ghost)
#' for (p in plots) print(p)
#'
#' consensus <- run_simulation("consensus_illusion",
#'   n_models = 100, include_plot_data = TRUE)
#' plots <- plot_simulation_showcase(consensus)
#' }
#'
#' @export
plot_simulation_showcase <- function(result, ..., strict = FALSE) {
  if (!is.list(result) || is.null(result$scenario)) {
    stop("result must be a simulation result list from run_simulation().",
         call. = FALSE)
  }

  scenario <- result$scenario

  if (identical(scenario, "consensus_illusion")) {
    .showcase_consensus(result, ..., strict = strict)
  } else if (scenario %in% c("lynchpin_of_certainty", "crux_of_certainty")) {
    .showcase_lynchpin(result, ..., strict = strict)
  } else if (identical(scenario, "ghost_discovery")) {
    .showcase_ghost(result, ..., strict = strict)
  } else {
    stop("Unrecognized simulation result: scenario = '",
         scenario, "'. Expected one of: consensus_illusion, ",
         "lynchpin_of_certainty, crux_of_certainty, ghost_discovery.",
         call. = FALSE)
  }
}


.showcase_consensus <- function(result, ..., strict = FALSE) {
  plots <- list()

  plots$component_status_heatmap <- tryCatch(
    plot_component_status_heatmap(result, ...),
    error = function(e) {
      message("component_status_heatmap skipped: ", e$message)
      NULL
    }
  )

  pd <- result$artifacts$plot_data
  if (!is.null(pd$model_metrics) && is.data.frame(pd$model_metrics)) {
    plots$consensus_contrast <- tryCatch(
      plot_consensus_contrast(result, ...),
      error = function(e) {
        message("consensus_contrast skipped: ", e$message)
        NULL
      }
    )
  } else if (isTRUE(strict)) {
    stop("consensus_contrast requires plot data. ",
         "Rerun run_simulation(..., include_plot_data = TRUE).",
         call. = FALSE)
  } else {
    message("Optional showcase plot 'consensus_contrast' requires ",
            "plot data. Rerun run_simulation(..., include_plot_data = TRUE).")
  }

  Filter(Negate(is.null), plots)
}


.showcase_lynchpin <- function(result, ..., strict = FALSE) {
  plots <- list()

  plots$component_status_heatmap <- tryCatch(
    plot_component_status_heatmap(result, ...),
    error = function(e) {
      message("component_status_heatmap skipped: ", e$message)
      NULL
    }
  )

  pd <- result$artifacts$plot_data
  if (!is.null(pd$pairwise_shift) && is.data.frame(pd$pairwise_shift)) {
    plots$compatibility_shift <- tryCatch(
      plot_compatibility_shift(result, ...),
      error = function(e) {
        message("compatibility_shift skipped: ", e$message)
        NULL
      }
    )
  } else if (isTRUE(strict)) {
    stop("compatibility_shift requires plot data. ",
         "Rerun run_simulation(..., include_plot_data = TRUE).",
         call. = FALSE)
  } else {
    message("Optional showcase plot 'compatibility_shift' requires ",
            "plot data. Rerun run_simulation(..., include_plot_data = TRUE).")
  }

  Filter(Negate(is.null), plots)
}


.showcase_ghost <- function(result, ..., strict = FALSE) {
  plots <- list()

  plots$ghost_contrast <- tryCatch(
    plot_ghost_contrast(result, ...),
    error = function(e) {
      message("ghost_contrast skipped: ", e$message)
      NULL
    }
  )

  cluster_result <- .build_cluster_result_from_simulation_showcase(result)
  if (!is.null(cluster_result)) {
    plots$cluster_embedding <- tryCatch(
      plot_cluster_embedding(cluster_result, ...),
      error = function(e) {
        message("cluster_embedding skipped: ", e$message)
        NULL
      }
    )
  }

  pd <- result$artifacts$plot_data
  if (!is.null(pd$dyad_heatmap) && is.data.frame(pd$dyad_heatmap)) {
    plots$clustered_dyad_heatmap <- tryCatch(
      plot_clustered_dyad_heatmap(result, ...),
      error = function(e) {
        message("clustered_dyad_heatmap skipped: ", e$message)
        NULL
      }
    )
  } else if (isTRUE(strict)) {
    stop("clustered_dyad_heatmap requires plot data. ",
         "Rerun run_simulation(..., include_plot_data = TRUE).",
         call. = FALSE)
  } else {
    message("Optional showcase plot 'clustered_dyad_heatmap' requires ",
            "plot data. Rerun run_simulation(..., include_plot_data = TRUE).")
  }

  Filter(Negate(is.null), plots)
}


.build_cluster_result_from_simulation_showcase <- function(result) {
  embedding <- result$artifacts$embedding_2d
  assignments <- result$artifacts$cluster_assignments
  ghost_clusters <- result$results$ghost_clusters

  if (is.null(embedding) || !is.data.frame(embedding) ||
      nrow(embedding) == 0) {
    return(NULL)
  }

  required_embedding_cols <- c("model_id", "x", "y")
  if (!all(required_embedding_cols %in% names(embedding))) {
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
    ghost_clusters = ghost_clusters,
    prior_model_id = result$artifacts$prior_model_id %||% NULL
  )
}

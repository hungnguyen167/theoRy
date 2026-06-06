#' Detect ghost clusters via compatibility profile clustering
#'
#' Discovers internally consistent but "invisible" theoretical traditions by
#' clustering compatibility profiles and contrasting them against a user prior
#' model. Uses UMAP for dimensionality reduction and DBSCAN for clustering.
#'
#' @param dyads A dyad matrix data frame returned by
#'   \code{\link{build_dyad_matrix}}. The object must include its
#'   \code{theory_context} attribute, which is added automatically by
#'   \code{build_dyad_matrix()}.
#' @param prior_model Optional model ID (e.g. \code{"M0001"}) for ghost
#'   detection contrast analysis. When \code{NULL} (default), only clustering
#'   is performed without ghost identification.
#' @param eps DBSCAN eps parameter controlling neighborhood radius. Must be
#'   positive. Defaults to 0.5.
#' @param min_samples DBSCAN min_samples parameter. Must be at least 2.
#'   Defaults to 5.
#' @param umap_components Number of dimensions for UMAP reduction (2 or 3).
#'   Defaults to 2.
#' @param umap_n_neighbors UMAP n_neighbors parameter. Defaults to 15.
#' @param umap_min_dist UMAP min_dist parameter. Must be in [0, 1].
#'   Defaults to 0.1.
#' @param umap_metric Distance metric for UMAP. Defaults to "euclidean".
#' @param random_state Random seed for reproducibility. Defaults to 42.
#'   Use \code{NULL} for non-deterministic results.
#' @param internal_threshold Minimum internal_compatibility for a cluster to be
#'   considered internally consistent. Must be in [0, 1]. Defaults to 0.6.
#' @param prior_threshold Minimum prior_compatibility for a cluster to be
#'   considered mainstream (aligned with prior). Must be in [0, 1].
#'   Defaults to 0.4.
#' @param score_field Dyad column to use as the compatibility score. Defaults
#'   to \code{"similarity_rate"}. Use causal fields such as
#'   \code{"mas_compatible"} or \code{"full_compatible"} for alternative
#'   simulation scenarios.
#' @param url Base URL of the theoRy Python backend API. Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return A list with components:
#'   \item{cluster_assignments}{Data frame with columns \code{model_id} and
#'     \code{cluster_id}. Noise models have \code{NA} for \code{cluster_id}.}
#'   \item{cluster_summaries}{Data frame with columns \code{cluster_id},
#'     \code{model_count}, and \code{internal_compatibility}.}
#'   \item{ghost_clusters}{Data frame with columns \code{cluster_id},
#'     \code{model_count}, \code{internal_compatibility},
#'     \code{prior_compatibility}, \code{prior_distance}, \code{label}, and
#'     \code{representative_models} (list-column). Empty when no prior is
#'     supplied or no ghosts are detected.}
#'   \item{embedding_2d}{Data frame with columns \code{model_id}, \code{x},
#'     and \code{y} for visualization.}
#'
#' @details
#' Ghost cluster detection works by:
#' \enumerate{
#'   \item Building compatibility profile vectors from dyad records
#'   \item Reducing dimensionality with UMAP
#'   \item Clustering with DBSCAN
#'   \item Contrasting each cluster against the user's prior model
#' }
#'
#' Clusters are labeled as:
#' \itemize{
#'   \item \code{"ghost"}: High internal compatibility but low prior
#'     compatibility (invisible from the prior's perspective)
#'   \item \code{"mainstream"}: High prior compatibility (aligned with prior)
#'   \item \code{"fragmented"}: Low internal compatibility
#' }
#'
#' When ghost clusters are detected, a summary message is printed showing
#' the number of ghost clusters, total models, and the top ghost cluster
#' with representative models.
#'
#' DBSCAN's \\code{eps} is a sensitivity parameter rather than a fixed
#' substantive threshold, especially after UMAP because UMAP coordinates have
#' arbitrary scale. For small synthetic simulations, use
#' \\code{min_samples = 2} and consider a sweep over plausible \\code{eps}
#' values. A robust ghost-cluster result should appear across a stable plateau
#' of \\code{eps} values, not only at one narrow setting. A practical default
#' reporting rule is to choose the smallest \\code{eps} in the plateau that
#' gives the expected number of clusters with no noise points and the same
#' ghost-cluster count.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' reg <- build_component_registry(c("X", "Y", "Z"), timing = c(1, 2, 3))
#' states <- expand_model_states(reg, mode = "exhaustive",
#'   edge_statuses = c("causal", "unknown"))
#' dyads <- build_dyad_matrix(reg, states, mode = "basic")
#'
#' # Clustering with ghost detection
#' result <- detect_ghost_clusters(dyads, prior_model = "M0001")
#' result$ghost_clusters
#'
#' # Clustering without prior
#' result_no_prior <- detect_ghost_clusters(dyads)
#' result_no_prior$cluster_summaries
#'
#' # Sensitivity check for DBSCAN eps
#' eps_grid <- seq(0.1, 2.0, by = 0.1)
#' eps_check <- lapply(eps_grid, function(e) {
#'   res <- suppressMessages(detect_ghost_clusters(
#'     dyads,
#'     prior_model = "M0001",
#'     eps = e,
#'     min_samples = 2,
#'     umap_n_neighbors = 2
#'   ))
#'   data.frame(
#'     eps = e,
#'     clusters = length(unique(na.omit(res$cluster_assignments$cluster_id))),
#'     noise = sum(is.na(res$cluster_assignments$cluster_id)),
#'     ghosts = nrow(res$ghost_clusters)
#'   )
#' })
#' eps_check <- do.call(rbind, eps_check)
#' valid_eps <- subset(eps_check, clusters == 2 & noise == 0 & ghosts == 1)
#' chosen_eps <- min(valid_eps$eps)
#'
#' stop_theory_engine()
#' }
#'
#' @export
detect_ghost_clusters <- function(dyads,
                                   prior_model = NULL,
                                   eps = 0.5,
                                   min_samples = 5L,
                                   umap_components = 2L,
                                   umap_n_neighbors = 15L,
                                   umap_min_dist = 0.1,
                                   umap_metric = "euclidean",
                                    random_state = 42L,
                                    internal_threshold = 0.6,
                                    prior_threshold = 0.4,
                                    score_field = "similarity_rate",
                                    url = getOption("theoRy.engine_url",
                                                     "http://localhost:8000")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x

  if (missing(dyads) || !is.data.frame(dyads)) {
    stop("dyads must be a data frame returned by build_dyad_matrix().",
         call. = FALSE)
  }
  context <- attr(dyads, "theory_context")
  if (is.null(context) || is.null(context$registry_data) ||
      is.null(context$state_data)) {
    stop("dyads is missing theory_context. Recreate it with build_dyad_matrix().",
         call. = FALSE)
  }

  if (!is.numeric(eps) || eps <= 0) {
    stop("eps must be positive.", call. = FALSE)
  }
  if (!is.numeric(min_samples) || min_samples < 2) {
    stop("min_samples must be at least 2.", call. = FALSE)
  }
  if (!umap_components %in% c(2L, 3L)) {
    stop("umap_components must be 2 or 3.", call. = FALSE)
  }
  if (!is.numeric(umap_min_dist) || umap_min_dist < 0 || umap_min_dist > 1) {
    stop("umap_min_dist must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(internal_threshold) || internal_threshold < 0 ||
      internal_threshold > 1) {
    stop("internal_threshold must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(prior_threshold) || prior_threshold < 0 ||
      prior_threshold > 1) {
    stop("prior_threshold must be between 0 and 1.", call. = FALSE)
  }
  if (!is.character(score_field) || length(score_field) != 1L ||
      is.na(score_field) || identical(score_field, "")) {
    stop("score_field must be a non-empty string.", call. = FALSE)
  }

  dyad_records <- .delta_u_dyads_to_records(dyads)
  if (score_field %in% names(dyads)) {
    for (i in seq_along(dyad_records)) {
      value <- dyads[[score_field]][i]
      if (!is.na(value)) {
        dyad_records[[i]][[score_field]] <- value
      }
    }
  }

  payload <- list(
    registry_data = context$registry_data,
    state_data = context$state_data,
    model_ids = context$model_ids,
    dyads = dyad_records,
    eps = eps,
    min_samples = as.integer(min_samples),
    umap_components = as.integer(umap_components),
    umap_n_neighbors = as.integer(umap_n_neighbors),
    umap_min_dist = umap_min_dist,
    umap_metric = umap_metric,
    random_state = random_state,
    internal_threshold = internal_threshold,
    prior_threshold = prior_threshold,
    score_field = score_field
  )

  if (!is.null(prior_model)) {
    payload$prior_model_id <- prior_model
  }

  req <- httr2::request(url) |>
    httr2::req_url_path("api/v1/clusters") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(payload) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Python backend not reachable at ", url,
           ". Start the server with start_theory_engine().",
           call. = FALSE)
    }
  )

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid backend response: expected JSON response body.",
           call. = FALSE)
    }
  )

  if (!is.list(body) || is.null(body$status)) {
    stop("Invalid backend response: missing status field.", call. = FALSE)
  }

  if (identical(body$status, "error")) {
    code <- if (is.null(body$code)) "UNKNOWN" else body$code
    message <- if (is.null(body$message)) "Unknown backend error" else body$message

    if (identical(code, "NO_DYADS")) {
      stop("No dyad records available. Run build_dyad_matrix() first.",
           call. = FALSE)
    }
    if (identical(code, "MODEL_NOT_FOUND")) {
      stop("Prior model ", prior_model, " not found in the multiverse.",
           call. = FALSE)
    }
    stop("Backend error [", code, "]: ", message, call. = FALSE)
  }

  if (!identical(body$status, "success") || is.null(body$data)) {
    stop("Invalid backend response: missing data.", call. = FALSE)
  }

  data <- body$data

  result <- list(
    cluster_assignments = .parse_cluster_assignments(data$cluster_assignments),
    cluster_summaries = .parse_cluster_summaries(data$cluster_summaries),
    ghost_clusters = .parse_ghost_clusters(data$ghost_clusters),
    embedding_2d = .parse_embedding_2d(data$embedding_2d)
  )

  if (!is.null(prior_model) && nrow(result$ghost_clusters) > 0) {
    n_ghost <- nrow(result$ghost_clusters)
    m_models <- sum(result$ghost_clusters$model_count)
    message("Found ", n_ghost, " ghost cluster(s) with ", m_models, " total models")
    top <- result$ghost_clusters[1, ]
    message("Top ghost cluster: ", top$cluster_id,
            " (", top$model_count, " models, ",
            "internal compatibility = ", top$internal_compatibility, ")")
    if (length(top$representative_models[[1]]) > 0) {
      message("Representative models: ",
              paste(top$representative_models[[1]], collapse = ", "))
    }
  }

  result
}


.parse_cluster_assignments <- function(assignments) {
  if (is.null(assignments) || length(assignments) == 0) {
    return(data.frame(
      model_id = character(0),
      cluster_id = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    model_id = vapply(assignments, function(a) a$model_id, character(1),
                      USE.NAMES = FALSE),
    cluster_id = vapply(assignments, function(a) {
      if (is.null(a$cluster_id)) NA_character_ else a$cluster_id
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}


.parse_cluster_summaries <- function(summaries) {
  if (is.null(summaries) || length(summaries) == 0) {
    return(data.frame(
      cluster_id = character(0),
      model_count = integer(0),
      internal_compatibility = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    cluster_id = vapply(summaries, function(s) s$cluster_id, character(1),
                        USE.NAMES = FALSE),
    model_count = vapply(summaries, function(s) as.integer(s$model_count),
                         integer(1), USE.NAMES = FALSE),
    internal_compatibility = vapply(summaries, function(s) {
      s$internal_compatibility %||% NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}


.parse_ghost_clusters <- function(ghosts) {
  if (is.null(ghosts) || length(ghosts) == 0) {
    return(data.frame(
      cluster_id = character(0),
      model_count = integer(0),
      internal_compatibility = numeric(0),
      prior_compatibility = numeric(0),
      prior_distance = numeric(0),
      label = character(0),
      representative_models = I(list()),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    cluster_id = vapply(ghosts, function(g) g$cluster_id, character(1),
                        USE.NAMES = FALSE),
    model_count = vapply(ghosts, function(g) as.integer(g$model_count),
                         integer(1), USE.NAMES = FALSE),
    internal_compatibility = vapply(ghosts, function(g) {
      g$internal_compatibility %||% NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    prior_compatibility = vapply(ghosts, function(g) {
      g$prior_compatibility %||% NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    prior_distance = vapply(ghosts, function(g) {
      g$prior_distance %||% NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    label = vapply(ghosts, function(g) g$label %||% NA_character_,
                   character(1), USE.NAMES = FALSE),
    representative_models = I(lapply(ghosts, function(g) {
      if (is.null(g$representative_models)) {
        character(0)
      } else {
        unlist(g$representative_models)
      }
    })),
    stringsAsFactors = FALSE
  )
}


.parse_embedding_2d <- function(embedding) {
  if (is.null(embedding) || length(embedding$model_ids) == 0) {
    return(data.frame(
      model_id = character(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    model_id = unlist(embedding$model_ids),
    x = unlist(embedding$x),
    y = unlist(embedding$y),
    stringsAsFactors = FALSE
  )
}

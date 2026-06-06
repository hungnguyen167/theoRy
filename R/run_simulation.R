`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run a proof-of-concept simulation scenario
#'
#' Generates a synthetic multiverse and runs one of three built-in scenarios:
#'
#' \enumerate{
#'   \item \strong{Illusion of Precision} — demonstrates how surface consensus
#'     can mask deep structural incompatibility, and how Delta-U identifies
#'     the hidden lynchpin components.
#'   \item \strong{Lynchpin of Certainty} — demonstrates a phase transition:
#'     resolving a single component dramatically increases global compatibility.
#'   \item \strong{Ghost Discovery} — demonstrates how clustering + contrast
#'     analysis reveals an internally consistent but prior-incompatible
#'     sub-community ("ghost cluster").
#' }
#'
#' @param scenario Scenario name: \code{"illusion_of_precision"},
#'   \code{"lynchpin_of_certainty"}, or \code{"ghost_discovery"}.
#'   Partial matching is supported.
#' @param n_models Number of synthetic models to generate. Must be at least 10.
#' @param n_components Number of registry components (nodes + edges). Must be
#'   at least 5.
#' @param random_state Random seed for reproducibility. \code{NULL} produces
#'   non-deterministic results.  Defaults to \code{42L}.
#' @param url Base URL of the theoRy Python backend API.  Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{scenario}}{Scenario name.}
#'   \item{\code{results}}{Scenario-specific metrics (list).}
#'   \item{\code{artifacts}}{Registry data, state data, model IDs, summary
#'     stats.}
#'   \item{\code{summary}}{Character vector of human-readable key findings.}
#' }
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' illusion <- run_simulation("illusion_of_precision", n_models = 100)
#' illusion$results$surface_consensus
#' illusion$results$structural_compatibility
#'
#' lynchpin <- run_simulation("lynchpin_of_certainty", n_models = 200)
#' lynchpin$results$phase_transition_score
#'
#' ghost <- run_simulation("ghost_discovery", n_models = 150)
#' ghost$results$ghost_cluster_found
#'
#' stop_theory_engine()
#' }
#'
#' @export
run_simulation <- function(scenario = c("illusion_of_precision",
                                         "lynchpin_of_certainty",
                                         "ghost_discovery"),
                            n_models = 100L,
                            n_components = 50L,
                            random_state = 42L,
                            url = getOption("theoRy.engine_url",
                                             "http://localhost:8000")) {
  # ── local validation ──────────────────────────────────────────────────────────
  scenario <- match.arg(scenario)

  if (!is.numeric(n_models) || n_models < 10) {
    stop("n_models must be at least 10.", call. = FALSE)
  }
  if (!is.numeric(n_components) || n_components < 5) {
    stop("n_components must be at least 5.", call. = FALSE)
  }
  if (!is.null(random_state) && (!is.numeric(random_state) || random_state < 0)) {
    stop("random_state must be a non-negative integer or NULL.", call. = FALSE)
  }

  # ── request construction ──────────────────────────────────────────────────────
  payload <- list(
    scenario = scenario,
    n_models = as.integer(n_models),
    n_components = as.integer(n_components),
    random_state = random_state
  )

  # ── HTTP call ─────────────────────────────────────────────────────────────────
  req <- httr2::request(url) |>
    httr2::req_url_path("api/v1/simulate") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(payload) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Python backend not reachable at ", url,
           ". Start the server with start_theory_engine().", call. = FALSE)
    }
  )

  # ── response parsing ──────────────────────────────────────────────────────────
  status_code <- httr2::resp_status(resp)
  body <- httr2::resp_body_json(resp)

  if (status_code >= 400 || identical(body$status, "error")) {
    code <- body$code %||% "UNKNOWN"
    msg <- body$message %||% "Unknown error"
    if (identical(code, "INVALID_SCENARIO")) {
      stop("Invalid scenario. Must be one of: illusion_of_precision, ",
           "lynchpin_of_certainty, ghost_discovery.", call. = FALSE)
    }
    stop("Backend error [", code, "]: ", msg, call. = FALSE)
  }

  data <- body$data

  # ── build result structure ────────────────────────────────────────────────────
  result <- list(
    scenario = data$scenario,
    results = NULL,
    artifacts = NULL,
    summary = character(0)
  )

  if (identical(data$scenario, "illusion_of_precision")) {
    result$results <- .parse_illusion_results(data$results)
    result$summary <- c(
      sprintf("Surface consensus: %.2f", data$results$surface_consensus),
      sprintf("Structural compatibility: %.2f", data$results$structural_compatibility),
      sprintf("Consensus gap: %.2f \u2014 the illusion of precision is exposed",
              data$results$consensus_gap),
      if (length(data$results$lynchpin_components) > 0) {
        top <- data$results$lynchpin_components[[1]]
        sprintf("Top lynchpin: %s (delta_u = %.3f)",
                top$component_id, top$delta_u)
      }
    )
  } else if (identical(data$scenario, "lynchpin_of_certainty")) {
    result$results <- .parse_lynchpin_results(data$results)
    result$summary <- c(
      sprintf("Baseline compatibility: %.2f", data$results$baseline_compatibility),
      sprintf("Post-resolution compatibility: %.2f", data$results$post_resolution_compatibility),
      sprintf("Phase transition score: %.2f", data$results$phase_transition_score),
      sprintf("Lynchpin component: %s (rank %d)",
              data$results$lynchpin_component_id,
              data$results$lynchpin_rank)
    )
  } else if (identical(data$scenario, "ghost_discovery")) {
    result$results <- .parse_ghost_results(data$results)
    result$summary <- c(
      sprintf("Ghost cluster found: %s", data$results$ghost_cluster_found),
      sprintf("Clusters detected: %d", data$results$clusters_detected),
      sprintf("Noise models: %d", data$results$noise_count),
      if (data$results$ghost_cluster_found && length(data$results$ghost_clusters) > 0) {
        g <- data$results$ghost_clusters[[1]]
        sprintf("Top ghost cluster: %s (%d models, internal = %.2f, prior = %.2f)",
                g$cluster_id, g$model_count,
                g$internal_compatibility, g$prior_compatibility)
      }
    )
  }

  result$artifacts <- list(
    registry_data = .parse_registry_artifact(data$artifacts$registry_data),
    state_data = .parse_state_artifact(data$artifacts$state_data),
    model_ids = unlist(data$artifacts$model_ids),
    summary_stats = data$artifacts$summary_stats
  )

  # ── console output ────────────────────────────────────────────────────────────
  message("Simulation complete: ", data$scenario)
  for (line in result$summary) {
    message("  ", line)
  }

  result
}


# ── parsing helpers ────────────────────────────────────────────────────────────


.parse_illusion_results <- function(r) {
  lynchpins <- if (length(r$lynchpin_components) > 0) {
    data.frame(
      rank = vapply(r$lynchpin_components, function(x) as.integer(x$rank), integer(1)),
      component_id = vapply(r$lynchpin_components, function(x) x$component_id, character(1)),
      type = vapply(r$lynchpin_components, function(x) x$type %||% NA_character_, character(1)),
      source = vapply(r$lynchpin_components, function(x) x$source %||% NA_character_, character(1)),
      target = vapply(r$lynchpin_components, function(x) x$target %||% NA_character_, character(1)),
      delta_u = vapply(r$lynchpin_components, function(x) x$delta_u, numeric(1)),
      best_resolution = vapply(r$lynchpin_components, function(x) x$best_resolution, character(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      rank = integer(0), component_id = character(0),
      type = character(0), source = character(0), target = character(0),
      delta_u = numeric(0), best_resolution = character(0),
      stringsAsFactors = FALSE
    )
  }
  list(
    surface_consensus = r$surface_consensus,
    structural_compatibility = r$structural_compatibility,
    consensus_gap = r$consensus_gap,
    lynchpin_components = lynchpins,
    n_lynchpins = r$n_lynchpins,
    lynchpin_identified = r$lynchpin_identified
  )
}


.parse_lynchpin_results <- function(r) {
  list(
    baseline_compatibility = r$baseline_compatibility,
    post_resolution_compatibility = r$post_resolution_compatibility,
    phase_transition_score = r$phase_transition_score,
    lynchpin_component_id = r$lynchpin_component_id,
    lynchpin_rank = as.integer(r$lynchpin_rank),
    compatibility_timeline = if (length(r$compatibility_timeline) > 0) {
      data.frame(
        step = vapply(r$compatibility_timeline, function(x) x$step, character(1)),
        compatibility = vapply(r$compatibility_timeline, function(x) x$compatibility, numeric(1)),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(step = character(0), compatibility = numeric(0), stringsAsFactors = FALSE)
    }
  )
}


.parse_ghost_results <- function(r) {
  ghost_df <- if (length(r$ghost_clusters) > 0) {
    data.frame(
      cluster_id = vapply(r$ghost_clusters, function(x) x$cluster_id, character(1)),
      model_count = vapply(r$ghost_clusters, function(x) as.integer(x$model_count), integer(1)),
      internal_compatibility = vapply(r$ghost_clusters, function(x) x$internal_compatibility, numeric(1)),
      prior_compatibility = vapply(r$ghost_clusters, function(x) x$prior_compatibility, numeric(1)),
      prior_distance = vapply(r$ghost_clusters, function(x) x$prior_distance, numeric(1)),
      label = vapply(r$ghost_clusters, function(x) x$label, character(1)),
      representative_models = I(lapply(r$ghost_clusters, function(x) {
        if (is.null(x$representative_models)) character(0) else unlist(x$representative_models)
      })),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      cluster_id = character(0), model_count = integer(0),
      internal_compatibility = numeric(0), prior_compatibility = numeric(0),
      prior_distance = numeric(0), label = character(0),
      representative_models = I(list()),
      stringsAsFactors = FALSE
    )
  }

  list(
    ghost_cluster_found = r$ghost_cluster_found,
    clusters_detected = as.integer(r$clusters_detected),
    ghost_clusters = ghost_df,
    mainstream_cluster = r$mainstream_cluster,
    noise_count = as.integer(r$noise_count),
    total_ghost_models = as.integer(r$total_ghost_models %||% 0L)
  )
}


.parse_registry_artifact <- function(records) {
  if (length(records) == 0) {
    return(data.frame(
      comp_id = character(0), type = character(0),
      source = character(0), target = character(0),
      direction = character(0), description = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    comp_id = vapply(records, function(x) x$comp_id, character(1)),
    type = vapply(records, function(x) x$type, character(1)),
    source = vapply(records, function(x) x$source, character(1)),
    target = vapply(records, function(x) x$target %||% NA_character_, character(1)),
    direction = vapply(records, function(x) x$direction %||% NA_character_, character(1)),
    description = vapply(records, function(x) x$description, character(1)),
    stringsAsFactors = FALSE
  )
}


.parse_state_artifact <- function(records) {
  if (length(records) == 0) {
    return(data.frame(
      model_id = character(0), comp_id = character(0),
      status = character(0), timing = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    model_id = vapply(records, function(x) x$model_id, character(1)),
    comp_id = vapply(records, function(x) x$comp_id, character(1)),
    status = vapply(records, function(x) x$status, character(1)),
    timing = vapply(records, function(x) as.integer(x$timing), integer(1)),
    stringsAsFactors = FALSE
  )
}

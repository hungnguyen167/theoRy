`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run a proof-of-concept simulation scenario
#'
#' Generates a synthetic multiverse and runs one of three built-in scenarios,
#' or optionally runs a seeded simulation on a user-provided multiverse from
#' \code{\link{build_component_registry}} and \code{\link{expand_model_states}}.
#'
#' \enumerate{
#'   \item \strong{Illusion of Precision} — demonstrates how high structural
#'     similarity can mask low causal compatibility across resolved and partial
#'     models.
#'   \item \strong{Lynchpin of Certainty} — demonstrates a phase transition:
#'     resolving a single component dramatically increases global compatibility.
#'   \item \strong{Ghost Discovery} — demonstrates how clustering + contrast
#'     analysis reveals an internally consistent but prior-incompatible
#'     sub-community ("ghost cluster").
#' }
#'
#' For finer control over each scenario's parameters, use the scenario-specific
#' wrappers \code{\link{run_simulation_illusion}},
#' \code{\link{run_simulation_lynchpin}}, or
#' \code{\link{run_simulation_ghost}}.
#'
#' @param scenario Scenario name: \code{"illusion_of_precision"},
#'   \code{"lynchpin_of_certainty"}, or \code{"ghost_discovery"}.
#'   Partial matching is supported.
#' @param n_models Number of synthetic models to generate. Must be at least 10.
#'   Ignored in seeded mode and by the fixed 192-model generated Illusion design.
#' @param n_components Number of registry components (nodes + edges). Must be
#'   at least 5. Ignored in seeded mode and by the fixed generated Illusion
#'   registries.
#' @param include_bidirectional Logical. Must be \code{FALSE}; simulations
#'   currently support directed components only. Bidirected components remain
#'   supported by general, non-simulation APIs.
#' @param registry Optional component registry from
#'   \code{\link{build_component_registry}}. Required when \code{states} is
#'   supplied. When present, simulation uses the supplied multiverse instead
#'   of generating synthetic components.
#' @param states Optional model-state records from
#'   \code{\link{expand_model_states}}. Required when \code{registry} is
#'   supplied. The simulation computes dyads from these states internally.
#' @param sample_n Optional positive integer. In seeded mode, number of
#'   distinct model IDs to sample without replacement from \code{states}.
#'   If \code{NULL}, all supplied models are used.
#' @param random_state Random seed for reproducibility. \code{NULL} produces
#'   non-deterministic results.  Defaults to \code{42L}.
#' @param mode Computation mode: \code{"concrete"} (Python backend) or
#'   \code{"symbolic"} (BDD-based). Seeded mode only supports
#'   \code{"concrete"}.
#' @param compatibility_metric One metric that drives the simulation:
#'   \code{"similarity_rate"}, \code{"mas_compatible"}, or
#'   \code{"identified_compatible"}. The concrete Illusion of Precision
#'   scenario requires \code{"mas_compatible"} or
#'   \code{"identified_compatible"} and defaults to the former. Other
#'   concrete scenarios default to \code{"similarity_rate"}.
#' @param crux_mode Crux semantics used by the lynchpin/crux scenarios:
#'   \code{"marginal"} (default) ranks uncertain components by evaluating both
#'   causal and non-causal resolutions; \code{"global"} resolves every
#'   applicable unknown edge instance to a single status. Illusion and Ghost
#'   scenarios only accept the default \code{"marginal"} value.
#' @param global_status Required status (\code{"causal"} or
#'   \code{"non-causal"}) for \code{crux_mode = "global"}. Must be
#'   \code{NULL} in marginal mode.
#' @param exposure Optional exposure node. Required for causal metrics except
#'   in a generated Illusion of Precision simulation, where the backend infers
#'   \code{"X1"}.
#' @param outcome Optional outcome node. Required for causal metrics except in
#'   a generated Illusion of Precision simulation, where the backend infers
#'   \code{"Y"}.
#' @param include_plot_data Logical. When \code{TRUE}, request bounded
#'   diagnostic artifacts used by showcase simulation plots. Only supported
#'   for concrete simulations.
#' @param plot_sample_n Positive integer or \code{NULL}. Maximum number of
#'   models included in model/matrix-style plot diagnostics.
#' @param pair_sample_n Positive integer or \code{NULL}. Maximum number of
#'   dyad rows included in pairwise plot diagnostics.
#' @param ... Additional scenario-specific parameters passed to the backend.
#'   See \code{\link{run_simulation_illusion}},
#'   \code{\link{run_simulation_lynchpin}}, and
#'   \code{\link{run_simulation_ghost}} for available options.
#' @param url Base URL of the theoRy Python backend API.  Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{scenario}}{Scenario name.}
#'   \item{\code{results}}{Scenario-specific metrics (list). Concrete
#'     Illusion results contain \code{mean_similarity_rate},
#'     \code{compatibility_metric}, \code{compatibility_rate},
#'     \code{precision_illusion_gap}, \code{resolved_model_count},
#'     \code{partial_model_count}, \code{design}, and \code{diagnostics}.}
#'   \item{\code{artifacts}}{Registry data, state data, model IDs, summary
#'     stats.}
#'   \item{\code{summary}}{Character vector of human-readable key findings.}
#' }
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' illusion <- run_simulation("illusion_of_precision")
#' illusion$results$mean_similarity_rate
#' illusion$results$compatibility_rate
#' illusion$results$precision_illusion_gap
#'
#' lynchpin <- run_simulation("lynchpin_of_certainty", n_models = 200)
#' lynchpin$results$phase_transition_score
#'
#' ghost <- run_simulation("ghost_discovery", n_models = 150)
#' ghost$results$ghost_cluster_found
#'
#' # Seeded simulation with a pre-built multiverse
#' registry <- build_component_registry(nodes, edges)
#' states <- expand_model_states(registry, mode = "sampled",
#'                                n_models = 1000, seed = 42)
#' sim <- run_simulation("ghost_discovery", registry = registry,
#'                        states = states, sample_n = 200)
#'
#' stop_theory_engine()
#' }
#'
#' @export
run_simulation <- function(scenario = c("illusion_of_precision",
                                         "lynchpin_of_certainty",
                                         "crux_of_certainty",
                                         "ghost_discovery"),
                            n_models = 100L,
                            n_components = 50L,
                            include_bidirectional = FALSE,
                            registry = NULL,
                            states = NULL,
                            sample_n = NULL,
                            random_state = 42L,
                            mode = c("concrete", "symbolic"),
                            compatibility_metric = NULL,
                            crux_mode = c("marginal", "global"),
                            global_status = NULL,
                            exposure = NULL,
                            outcome = NULL,
                            include_plot_data = FALSE,
                            plot_sample_n = 200L,
                            pair_sample_n = 5000L,
                            ...,
                            url = getOption("theoRy.engine_url",
                                             "http://localhost:8000")) {
  scenario <- match.arg(scenario)
  mode <- match.arg(mode)
  crux_mode <- match.arg(crux_mode)
  .validate_simulation_direction(include_bidirectional)

  .validate_plot_data_args(include_plot_data, plot_sample_n, pair_sample_n)

  if (identical(mode, "symbolic")) {
    if (!identical(crux_mode, "marginal") || !is.null(global_status)) {
      stop("Symbolic simulations do not support global crux arguments.",
           call. = FALSE)
    }
    compatibility_metric <- compatibility_metric %||% "similarity_rate"
    compatibility_metric <- match.arg(
      compatibility_metric,
      c("similarity_rate", "mas_compatible", "identified_compatible")
    )
    if (!identical(compatibility_metric, "similarity_rate")) {
      stop("Symbolic simulations currently support similarity_rate only.",
           call. = FALSE)
    }
    if (isTRUE(include_plot_data)) {
      stop("Simulation plot data is only supported for concrete simulations.",
           call. = FALSE)
    }
    if (!is.null(registry) || !is.null(states)) {
      stop("Seeded state simulation is only supported in concrete mode.",
           call. = FALSE)
    }
    if (!is.null(sample_n)) {
      stop("sample_n is only used when registry and states are supplied.",
           call. = FALSE)
    }
    return(.run_symbolic_simulation(
      scenario = scenario,
      seed = random_state,
      url = url
    ))
  }

  compatibility_metric <- .resolve_simulation_metric(
    scenario, compatibility_metric
  )

  .run_simulation_internal(
    scenario = scenario,
    n_models = n_models,
    n_components = n_components,
    include_bidirectional = include_bidirectional,
    registry = registry,
    states = states,
    sample_n = sample_n,
    random_state = random_state,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode,
    global_status = global_status,
    exposure = exposure,
    outcome = outcome,
    include_plot_data = include_plot_data,
    plot_sample_n = plot_sample_n,
    pair_sample_n = pair_sample_n,
    url = url,
    ...
  )
}


.run_symbolic_simulation <- function(scenario, seed = 42L, url) {
  payload <- list(
    scenario = scenario,
    mode = "sampled",
    n_samples = 500L,
    seed = seed
  )

  req <- httr2::request(url) |>
    httr2::req_url_path("api/v1/symbolic/simulate") |>
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

  status_code <- httr2::resp_status(resp)
  body <- httr2::resp_body_json(resp)

  if (status_code >= 400 || identical(body$status, "error")) {
    code <- body$code %||% "UNKNOWN"
    msg <- body$message %||% "Unknown error"
    stop("Backend error [", code, "]: ", msg, call. = FALSE)
  }

  data <- body$data

  result <- list(
    scenario = data$scenario,
    mode = data$mode,
    exact = data$exact,
    universe_summary = data$universe_summary,
    classes = data$classes,
    results = data$classes,
    metrics = data$metrics,
    artifacts = data$artifacts,
    warnings = data$warnings,
    summary = sprintf("Symbolic simulation complete: %s (%s, %d classes)",
                        data$scenario, data$mode,
                        length(data$classes))
  )
  class(result) <- c("theory_symbolic_simulation", "list")
  result
}


# ── internal dispatcher ────────────────────────────────────────────────────────

.run_simulation_internal <- function(scenario, n_models, n_components,
                                       include_bidirectional = FALSE,
                                       registry = NULL, states = NULL,
                                       sample_n = NULL,
                                       include_plot_data = FALSE,
                                       plot_sample_n = 200L,
                                       pair_sample_n = 5000L,
                                       random_state,
                                       compatibility_metric = NULL,
                                       crux_mode = c("marginal", "global"),
                                       global_status = NULL,
                                       exposure = NULL,
                                       outcome = NULL,
                                       url, ...) {
  .validate_simulation_direction(include_bidirectional)
  .validate_plot_data_args(include_plot_data, plot_sample_n, pair_sample_n)
  is_seeded <- !is.null(registry) || !is.null(states)
  compatibility_metric <- .resolve_simulation_metric(
    scenario, compatibility_metric
  )
  crux_mode <- match.arg(crux_mode)
  is_crux_scenario <- scenario %in% c(
    "lynchpin_of_certainty", "crux_of_certainty"
  )
  if (is_crux_scenario) {
    if (!is.null(global_status)) {
      global_status <- match.arg(global_status, c("causal", "non-causal"))
    }
    if (identical(crux_mode, "global") && is.null(global_status)) {
      stop("global_status ('causal' or 'non-causal') is required when ",
           "crux_mode = 'global'.", call. = FALSE)
    }
    if (identical(crux_mode, "marginal") && !is.null(global_status)) {
      stop("global_status is only valid with crux_mode = 'global'.",
           call. = FALSE)
    }
  } else if (!identical(crux_mode, "marginal") || !is.null(global_status)) {
    stop("crux_mode and global_status only apply to lynchpin/crux scenarios.",
         call. = FALSE)
  }
  .validate_simulation_query(
    scenario, compatibility_metric, exposure, outcome, is_seeded
  )

  if (is_seeded) {
    if (is.null(registry) || is.null(states)) {
      stop(
        if (is.null(registry)) "states requires registry for seeded simulation."
        else "registry requires states for seeded simulation.",
        call. = FALSE
      )
    }
    if (!is.null(sample_n) && (!is.numeric(sample_n) || length(sample_n) != 1L ||
        is.na(sample_n) || sample_n < 2 || sample_n != as.integer(sample_n))) {
      stop("sample_n must be an integer of at least 2 or NULL.", call. = FALSE)
    }
  } else {
    if (!is.null(sample_n)) {
      stop("sample_n is only used when registry and states are supplied.",
           call. = FALSE)
    }
    if (!is.numeric(n_models) || n_models < 10) {
      stop("n_models must be at least 10.", call. = FALSE)
    }
    if (!is.numeric(n_components) || n_components < 5) {
      stop("n_components must be at least 5.", call. = FALSE)
    }
  }

  if (!is.null(random_state) && (!is.numeric(random_state) || random_state < 0)) {
    stop("random_state must be a non-negative integer or NULL.", call. = FALSE)
  }

  if (is_seeded) {
    .send_seeded_simulation(scenario, registry, states, sample_n,
                             include_plot_data, plot_sample_n,
                             pair_sample_n, random_state,
                             compatibility_metric, crux_mode, global_status,
                             exposure, outcome, url, ...)
  } else {
    .send_synthetic_simulation(scenario, n_models, n_components,
                                include_bidirectional,
                                include_plot_data, plot_sample_n,
                                pair_sample_n, random_state,
                                compatibility_metric, crux_mode, global_status,
                                exposure, outcome, url, ...)
  }
}


.validate_simulation_direction <- function(include_bidirectional) {
  if (!is.logical(include_bidirectional) || length(include_bidirectional) != 1L ||
      is.na(include_bidirectional)) {
    stop("include_bidirectional must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(include_bidirectional)) {
    stop(
      "Simulations support directed components only; ",
      "include_bidirectional must be FALSE.",
      call. = FALSE
    )
  }
}


.resolve_simulation_metric <- function(scenario, compatibility_metric) {
  if (is.null(compatibility_metric)) {
    compatibility_metric <- if (identical(scenario, "illusion_of_precision")) {
      "mas_compatible"
    } else {
      "similarity_rate"
    }
  }
  compatibility_metric <- match.arg(
    compatibility_metric,
    c("similarity_rate", "mas_compatible", "identified_compatible")
  )
  if (identical(scenario, "illusion_of_precision") &&
      identical(compatibility_metric, "similarity_rate")) {
    stop(
      "illusion_of_precision requires compatibility_metric ",
      "'mas_compatible' or 'identified_compatible'.",
      call. = FALSE
    )
  }
  compatibility_metric
}


.simulation_metric_label <- function(compatibility_metric) {
  labels <- c(
    similarity_rate = "Similarity Rate",
    mas_compatible = "MAS Compatibility",
    identified_compatible = "Identified Compatibility"
  )
  label <- unname(labels[compatibility_metric])
  if (length(label) == 0L || is.na(label)) {
    return(gsub("_", " ", compatibility_metric, fixed = TRUE))
  }
  label
}


.validate_simulation_query <- function(scenario, compatibility_metric,
                                       exposure, outcome, is_seeded) {
  if (xor(is.null(exposure), is.null(outcome))) {
    stop("Both or neither of exposure and outcome must be provided.", call. = FALSE)
  }
  generated_illusion <- identical(scenario, "illusion_of_precision") &&
    !is_seeded
  if (!identical(compatibility_metric, "similarity_rate") &&
      (is.null(exposure) || is.null(outcome)) && !generated_illusion) {
    stop("Causal compatibility metrics require exposure and outcome.", call. = FALSE)
  }
}


.df_to_records <- function(df) {
  if (!is.data.frame(df)) stop("Expected a data frame", call. = FALSE)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, function(col) {
    if (is.factor(col)) as.character(col) else col
  })
  lapply(seq_len(nrow(df)), function(i) {
    record <- as.list(df[i, , drop = FALSE])
    missing <- vapply(record, .record_value_is_missing, logical(1))
    record[!missing]
  })
}


.record_value_is_missing <- function(x) {
  length(x) == 0L || (length(x) == 1L && (is.na(x) || is.nan(x)))
}


.validate_seeded_simulation_inputs <- function(registry, states, sample_n) {
  if (!is.data.frame(registry)) {
    stop("registry must be a data frame for seeded simulation.", call. = FALSE)
  }
  if (!is.data.frame(states)) {
    stop("states must be a data frame for seeded simulation.", call. = FALSE)
  }

  required_registry <- c("comp_id", "type", "source", "target", "direction", "description")
  missing_registry <- setdiff(required_registry, names(registry))
  if (length(missing_registry) > 0) {
    stop("registry is missing required column(s): ",
         paste(missing_registry, collapse = ", "), call. = FALSE)
  }

  required_states <- c("model_id", "comp_id", "status")
  missing_states <- setdiff(required_states, names(states))
  if (length(missing_states) > 0) {
    stop("states is missing required column(s): ",
         paste(missing_states, collapse = ", "), call. = FALSE)
  }

  if (!is.null(sample_n)) {
    if (!is.numeric(sample_n) || length(sample_n) != 1L || is.na(sample_n) ||
        sample_n < 1 || sample_n != as.integer(sample_n)) {
      stop("sample_n must be a positive integer or NULL.", call. = FALSE)
    }
  }
}


.validate_plot_data_args <- function(include_plot_data, plot_sample_n,
                                     pair_sample_n) {
  if (!is.logical(include_plot_data) || length(include_plot_data) != 1L ||
      is.na(include_plot_data)) {
    stop("include_plot_data must be TRUE or FALSE.", call. = FALSE)
  }
  if (!.is_positive_integer_or_null(plot_sample_n)) {
    stop("plot_sample_n must be a positive integer or NULL.", call. = FALSE)
  }
  if (!.is_positive_integer_or_null(pair_sample_n)) {
    stop("pair_sample_n must be a positive integer or NULL.", call. = FALSE)
  }
}


.is_positive_integer_or_null <- function(x) {
  is.null(x) || (is.numeric(x) && length(x) == 1L && !is.na(x) &&
    x >= 1 && x == as.integer(x))
}


.send_synthetic_simulation <- function(scenario, n_models, n_components,
                                         include_bidirectional = FALSE,
                                         include_plot_data, plot_sample_n,
                                         pair_sample_n, random_state,
                                         compatibility_metric,
                                         crux_mode, global_status, exposure,
                                         outcome,
                                         url, ...) {
  payload <- list(
    scenario = scenario,
    n_models = as.integer(n_models),
    n_components = as.integer(n_components),
    random_state = random_state,
    include_plot_data = isTRUE(include_plot_data),
    include_bidirectional = isTRUE(include_bidirectional),
    compatibility_metric = compatibility_metric
  )
  if (identical(scenario, "lynchpin_of_certainty") ||
      identical(scenario, "crux_of_certainty")) {
    payload$crux_mode <- crux_mode
    if (!is.null(global_status)) {
      payload$global_status <- global_status
    }
  }
  if (!is.null(exposure)) {
    payload$exposure <- exposure
    payload$outcome <- outcome
  }
  if (isTRUE(include_plot_data)) {
    if (!is.null(plot_sample_n)) payload$plot_sample_n <- as.integer(plot_sample_n)
    if (!is.null(pair_sample_n)) payload$pair_sample_n <- as.integer(pair_sample_n)
  }
  extra <- list(...)
  for (nm in names(extra)) {
    if (!is.null(extra[[nm]])) payload[[nm]] <- extra[[nm]]
  }
  .send_simulation_request(url, payload)
}


.send_seeded_simulation <- function(scenario, registry, states, sample_n,
                                       include_plot_data, plot_sample_n,
                                       pair_sample_n, random_state,
                                       compatibility_metric,
                                       crux_mode, global_status, exposure,
                                       outcome,
                                       url, ...) {
  .validate_seeded_simulation_inputs(registry, states, sample_n)

  registry_records <- .df_to_records(registry)
  state_records <- .df_to_records(states)

  payload <- list(
    scenario = scenario,
    registry_data = registry_records,
    state_data = state_records,
    random_state = random_state,
    include_plot_data = isTRUE(include_plot_data),
    compatibility_metric = compatibility_metric
  )
  if (identical(scenario, "lynchpin_of_certainty") ||
      identical(scenario, "crux_of_certainty")) {
    payload$crux_mode <- crux_mode
    if (!is.null(global_status)) {
      payload$global_status <- global_status
    }
  }
  if (!is.null(exposure)) {
    payload$exposure <- exposure
    payload$outcome <- outcome
  }
  if (!is.null(sample_n)) {
    payload$sample_n <- as.integer(sample_n)
  }
  if (isTRUE(include_plot_data)) {
    if (!is.null(plot_sample_n)) payload$plot_sample_n <- as.integer(plot_sample_n)
    if (!is.null(pair_sample_n)) payload$pair_sample_n <- as.integer(pair_sample_n)
  }

  extra <- list(...)
  for (nm in names(extra)) {
    if (!is.null(extra[[nm]])) payload[[nm]] <- extra[[nm]]
  }

  .send_simulation_request(url, payload)
}


.send_simulation_request <- function(url, payload) {
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

  status_code <- httr2::resp_status(resp)
  body <- httr2::resp_body_json(resp)

  if (status_code >= 400 || identical(body$status, "error")) {
    code <- body$code %||% "UNKNOWN"
    msg <- body$message %||% "Unknown error"
    stop("Backend error [", code, "]: ", msg, call. = FALSE)
  }

  data <- body$data

  result <- list(
    scenario = data$scenario,
    compatibility_metric = data$results$compatibility_metric,
    results = NULL,
    artifacts = NULL,
    summary = character(0)
  )

  if (identical(data$scenario, "illusion_of_precision")) {
    result$results <- .parse_illusion_results(data$results)
    result$summary <- c(
      sprintf("Mean structural similarity: %.2f",
              result$results$mean_similarity_rate),
      sprintf("%s rate: %.2f",
              .simulation_metric_label(result$results$compatibility_metric),
              result$results$compatibility_rate),
      sprintf("Precision illusion gap: %.2f",
              result$results$precision_illusion_gap),
      sprintf("Models: %d resolved, %d partial",
              result$results$resolved_model_count,
              result$results$partial_model_count),
      sprintf("Design: %s", result$results$design)
    )
  } else if (identical(data$scenario, "lynchpin_of_certainty") ||
             identical(data$scenario, "crux_of_certainty")) {
    result$results <- .parse_lynchpin_results(data$results)
    result$summary <- c(
      sprintf("Baseline compatibility: %.2f", data$results$baseline_compatibility),
      sprintf("Post-resolution compatibility: %.2f", data$results$post_resolution_compatibility),
      sprintf("Phase transition score: %.2f", data$results$phase_transition_score)
    )
    if (!is.null(data$results$lynchpin_component_id)) {
      result$summary <- c(
        result$summary,
        sprintf("Lynchpin component: %s (rank %d)",
                data$results$lynchpin_component_id,
                data$results$lynchpin_rank)
      )
    } else if (identical(data$results$crux_mode, "global")) {
      result$summary <- c(
        result$summary,
        sprintf("Global crux status: %s", data$results$target_status)
      )
    }
  } else if (identical(data$scenario, "ghost_discovery")) {
    result$results <- .parse_ghost_results(data$results)
    result$summary <- c(
      sprintf("Ghost cluster found: %s", data$results$ghost_cluster_found),
      sprintf("Clusters detected: %d", data$results$clusters_detected),
      sprintf("Noise models: %d", data$results$noise_count),
      if (isTRUE(data$results$ghost_cluster_found) && length(data$results$ghost_clusters) > 0) {
        g <- data$results$ghost_clusters[[1]]
        sprintf("Top ghost cluster: %s (%d models, internal = %.2f, prior = %.2f)",
                g$cluster_id, g$model_count,
                g$internal_compatibility, g$prior_compatibility)
      }
    )
  }

  core_artifacts <- list(
    registry_data = .parse_registry_artifact(data$artifacts$registry_data),
    state_data = .parse_state_artifact(data$artifacts$state_data),
    model_ids = unlist(data$artifacts$model_ids),
    summary_stats = data$artifacts$summary_stats
  )

  extra_artifacts <- .parse_extra_artifacts(data$artifacts)

  result$artifacts <- c(core_artifacts, extra_artifacts)

  message("Simulation complete: ", data$scenario)
  for (line in result$summary) {
    message("  ", line)
  }

  result
}


# ── Scenario A: Illusion of Precision ──────────────────────────────────────────

#' Run the "Illusion of Precision" simulation
#'
#' Generates a directed synthetic multiverse where high mean structural
#' similarity masks a lower rate of causal compatibility. The generated design
#' includes resolved and partial models and uses exposure \code{"X1"} and
#' outcome \code{"Y"}, inferred by the backend when omitted.
#'
#' @inheritParams run_simulation
#' @param n_models Ignored for generated Illusion runs, whose exhaustive design
#'   always contains 128 resolved models and 64 partial theories.
#' @param n_components Ignored for generated Illusion runs, whose MAS and
#'   identification registries are fixed by their seed theories.
#' @param random_state Random seed. \code{NULL} for non-deterministic.
#' @param enforce_thresholds When \code{NULL} (default), synthetic simulations
#'   enforce acceptance thresholds and seeded simulations do not. Set
#'   \code{TRUE} or \code{FALSE} to override this behavior.
#' @param include_plot_data Logical. When \code{TRUE}, request bounded
#'   diagnostic artifacts for showcase simulation plots.
#' @param plot_sample_n Positive integer or \code{NULL}. Maximum number of
#'   models included in model/matrix-style plot diagnostics.
#' @param pair_sample_n Positive integer or \code{NULL}. Maximum number of
#'   dyad rows included in pairwise plot diagnostics.
#' @param url Backend URL.
#'
#' @return A list with \code{scenario}, \code{results}, \code{artifacts},
#'   and \code{summary}. \code{results} contains
#'   \code{mean_similarity_rate}, \code{compatibility_metric},
#'   \code{compatibility_rate}, \code{precision_illusion_gap},
#'   \code{resolved_model_count}, \code{partial_model_count}, \code{design},
#'   and \code{diagnostics}.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' r <- run_simulation_illusion(compatibility_metric = "mas_compatible")
#' r$results$precision_illusion_gap
#' }
#'
#' @export
run_simulation_illusion <- function(n_models = 100L,
                                     n_components = 50L,
                                     include_bidirectional = FALSE,
                                     registry = NULL,
                                     states = NULL,
                                     sample_n = NULL,
                                     random_state = 42L,
                                      compatibility_metric = c(
                                        "mas_compatible",
                                        "identified_compatible"
                                       ),
                                      crux_mode = c("marginal", "global"),
                                      global_status = NULL,
                                      exposure = NULL,
                                      outcome = NULL,
                                      enforce_thresholds = NULL,
                                     include_plot_data = FALSE,
                                     plot_sample_n = 200L,
                                     pair_sample_n = 5000L,
                                     url = getOption("theoRy.engine_url",
                                                      "http://localhost:8000")) {
  .validate_simulation_direction(include_bidirectional)
  compatibility_metric <- match.arg(compatibility_metric)
  crux_mode <- match.arg(crux_mode)
  .run_simulation_internal(
    scenario = "illusion_of_precision",
    n_models = n_models,
    n_components = n_components,
    include_bidirectional = include_bidirectional,
    registry = registry,
    states = states,
    sample_n = sample_n,
    random_state = random_state,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode,
    global_status = global_status,
    exposure = exposure,
    outcome = outcome,
    include_plot_data = include_plot_data,
    plot_sample_n = plot_sample_n,
    pair_sample_n = pair_sample_n,
    enforce_thresholds = enforce_thresholds,
    url = url
  )
}


# ── Scenario B: Lynchpin of Certainty ──────────────────────────────────────────

#' Run the "Lynchpin of Certainty" simulation
#'
#' Generates a fragmented multiverse with multiple incompatible zones and
#' seeds one component whose resolution triggers a phase transition in
#' global compatibility.
#'
#' @inheritParams run_simulation
#' @param n_models Number of synthetic models to generate (>= 10).
#' @param n_components Number of registry components (>= 5).
#' @param random_state Random seed.
#' @param n_zones Number of incompatible zones in the multiverse.  When
#'   \code{NULL} (default), uses \code{min(4, max(3, n_edges // 5))}.
#' @param noise_fraction Fraction of models that are random noise.
#'   Default 0.10.
#' @param enforce_thresholds When \code{NULL} (default), synthetic simulations
#'   enforce acceptance thresholds and seeded simulations do not. Set
#'   \code{TRUE} or \code{FALSE} to override this behavior.
#' @param include_plot_data Logical. When \code{TRUE}, request bounded
#'   diagnostic artifacts for showcase simulation plots.
#' @param plot_sample_n Positive integer or \code{NULL}. Maximum number of
#'   models included in model/matrix-style plot diagnostics.
#' @param pair_sample_n Positive integer or \code{NULL}. Maximum number of
#'   dyad rows included in pairwise plot diagnostics.
#' @param url Backend URL.
#'
#' @return A list with \code{scenario}, \code{results}, \code{artifacts},
#'   \code{summary}.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' r <- run_simulation_lynchpin(n_models = 200, n_zones = 3)
#' r$results$phase_transition_score
#' }
#'
#' @export
run_simulation_lynchpin <- function(n_models = 200L,
                                     n_components = 80L,
                                     include_bidirectional = FALSE,
                                     registry = NULL,
                                     states = NULL,
                                     sample_n = NULL,
                                     random_state = 42L,
                                     compatibility_metric = c(
                                       "similarity_rate", "mas_compatible",
                                       "identified_compatible"
                                     ),
                                     crux_mode = c("marginal", "global"),
                                     global_status = NULL,
                                     exposure = NULL,
                                     outcome = NULL,
                                     n_zones = NULL,
                                     noise_fraction = 0.10,
                                     enforce_thresholds = NULL,
                                     include_plot_data = FALSE,
                                     plot_sample_n = 200L,
                                     pair_sample_n = 5000L,
                                     url = getOption("theoRy.engine_url",
                                                      "http://localhost:8000")) {
  .validate_simulation_direction(include_bidirectional)
  compatibility_metric <- match.arg(compatibility_metric)
  crux_mode <- match.arg(crux_mode)
  .run_simulation_internal(
    scenario = "lynchpin_of_certainty",
    n_models = n_models,
    n_components = n_components,
    include_bidirectional = include_bidirectional,
    registry = registry,
    states = states,
    sample_n = sample_n,
    random_state = random_state,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode,
    global_status = global_status,
    exposure = exposure,
    outcome = outcome,
    include_plot_data = include_plot_data,
    plot_sample_n = plot_sample_n,
    pair_sample_n = pair_sample_n,
    n_zones = n_zones,
    noise_fraction = noise_fraction,
    enforce_thresholds = enforce_thresholds,
    url = url
  )
}


#' Run the "Crux of Certainty" simulation
#'
#' Convenience wrapper for `run_simulation(scenario = "crux_of_certainty", ...)`.
#' This is the user-facing name for what was previously known as the
#' Lynchpin of Certainty scenario.
#'
#' @inheritParams run_simulation_lynchpin
#' @return A list with fields `scenario`, `n_models`, `n_components`,
#'   `results`, and `summary`.  The `results` element inherits from
#'   `run_simulation_lynchpin`.
#' @export
run_simulation_crux <- function(n_models = 200L,
                                 n_components = 80L,
                                 include_bidirectional = FALSE,
                                 registry = NULL,
                                 states = NULL,
                                 sample_n = NULL,
                                 random_state = 42L,
                                 compatibility_metric = c(
                                   "similarity_rate", "mas_compatible",
                                   "identified_compatible"
                                 ),
                                 crux_mode = c("marginal", "global"),
                                 global_status = NULL,
                                 exposure = NULL,
                                 outcome = NULL,
                                 n_zones = NULL,
                                 noise_fraction = 0.10,
                                 enforce_thresholds = NULL,
                                 include_plot_data = FALSE,
                                 plot_sample_n = 200L,
                                 pair_sample_n = 5000L,
                                 url = getOption("theoRy.engine_url",
                                                  "http://localhost:8000")) {
  .validate_simulation_direction(include_bidirectional)
  compatibility_metric <- match.arg(compatibility_metric)
  crux_mode <- match.arg(crux_mode)
  .run_simulation_internal(
    scenario = "crux_of_certainty",
    n_models = n_models,
    n_components = n_components,
    include_bidirectional = include_bidirectional,
    registry = registry,
    states = states,
    sample_n = sample_n,
    random_state = random_state,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode,
    global_status = global_status,
    exposure = exposure,
    outcome = outcome,
    include_plot_data = include_plot_data,
    plot_sample_n = plot_sample_n,
    pair_sample_n = pair_sample_n,
    n_zones = n_zones,
    noise_fraction = noise_fraction,
    enforce_thresholds = enforce_thresholds,
    url = url
  )
}


# ── Scenario C: Ghost Discovery ────────────────────────────────────────────────

#' Run the "Ghost Discovery" simulation
#'
#' Generates a multiverse with a mainstream cluster, a ghost cluster
#' (internally consistent but prior-invisible), and noise.  Demonstrates
#' that the clustering pipeline discovers theoretical traditions a biased
#' researcher would miss.
#'
#' @inheritParams run_simulation
#' @param n_models Number of synthetic models to generate (>= 10).
#' @param n_components Number of registry components (>= 5).
#' @param random_state Random seed.
#' @param mainstream_fraction Fraction of models in the mainstream cluster.
#'   Default 0.70.
#' @param ghost_fraction Fraction of models in the ghost cluster.
#'   Default 0.20.
#' @param eps DBSCAN eps parameter for clustering.  Default 0.5.
#' @param min_samples DBSCAN min_samples.  When \code{NULL} (default), uses
#'   \code{max(2, n_models // 20)}.
#' @param internal_threshold Minimum internal compatibility for a cluster to
#'   be considered internally consistent.  Default 0.6.
#' @param prior_threshold Minimum prior compatibility for "mainstream" label.
#'   Default 0.4.
#' @param divergent_fraction Fraction of edges where mainstream and ghost
#'   disagree.  When \code{NULL} (default), uses \code{n_edges // 4}.
#' @param enforce_thresholds When \code{NULL} (default), synthetic simulations
#'   enforce acceptance thresholds and seeded simulations do not. Set
#'   \code{TRUE} or \code{FALSE} to override this behavior.
#' @param include_plot_data Logical. When \code{TRUE}, request bounded
#'   diagnostic artifacts for showcase simulation plots.
#' @param plot_sample_n Positive integer or \code{NULL}. Maximum number of
#'   models included in model/matrix-style plot diagnostics.
#' @param pair_sample_n Positive integer or \code{NULL}. Maximum number of
#'   dyad rows included in pairwise plot diagnostics.
#' @param url Backend URL.
#'
#' @return A list with \code{scenario}, \code{results}, \code{artifacts},
#'   \code{summary}.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' r <- run_simulation_ghost(n_models = 150, ghost_fraction = 0.25)
#' r$results$ghost_cluster_found
#' }
#'
#' @export
run_simulation_ghost <- function(n_models = 150L,
                                  n_components = 60L,
                                  include_bidirectional = FALSE,
                                  registry = NULL,
                                  states = NULL,
                                  sample_n = NULL,
                                  random_state = 42L,
                                  compatibility_metric = c(
                                    "similarity_rate", "mas_compatible",
                                    "identified_compatible"
                                  ),
                                  crux_mode = c("marginal", "global"),
                                  global_status = NULL,
                                  exposure = NULL,
                                  outcome = NULL,
                                  mainstream_fraction = 0.70,
                                  ghost_fraction = 0.20,
                                  eps = 0.5,
                                  min_samples = NULL,
                                  internal_threshold = 0.6,
                                  prior_threshold = 0.4,
                                  divergent_fraction = NULL,
                                  enforce_thresholds = NULL,
                                  include_plot_data = FALSE,
                                  plot_sample_n = 200L,
                                  pair_sample_n = 5000L,
                                  url = getOption("theoRy.engine_url",
                                                   "http://localhost:8000")) {
  .validate_simulation_direction(include_bidirectional)
  compatibility_metric <- match.arg(compatibility_metric)
  crux_mode <- match.arg(crux_mode)
  .run_simulation_internal(
    scenario = "ghost_discovery",
    n_models = n_models,
    n_components = n_components,
    include_bidirectional = include_bidirectional,
    registry = registry,
    states = states,
    sample_n = sample_n,
    random_state = random_state,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode,
    global_status = global_status,
    exposure = exposure,
    outcome = outcome,
    include_plot_data = include_plot_data,
    plot_sample_n = plot_sample_n,
    pair_sample_n = pair_sample_n,
    mainstream_fraction = mainstream_fraction,
    ghost_fraction = ghost_fraction,
    eps = eps,
    min_samples = min_samples,
    internal_threshold = internal_threshold,
    prior_threshold = prior_threshold,
    divergent_fraction = divergent_fraction,
    enforce_thresholds = enforce_thresholds,
    url = url
  )
}


# ── parsing helpers ────────────────────────────────────────────────────────────


.parse_illusion_results <- function(r) {
  diagnostics <- list(
    n_dyads = as.integer(r$n_dyads),
    n_comparable_dyads = as.integer(r$n_comparable_dyads),
    n_unavailable_dyads = as.integer(r$n_unavailable_dyads),
    exposure = r$exposure,
    outcome = r$outcome,
    analysis_model_count = as.integer(r$analysis_model_count),
    completion_support_model_count = as.integer(
      r$completion_support_model_count
    )
  )
  list(
    mean_similarity_rate = r$mean_similarity_rate,
    compatibility_metric = r$compatibility_metric,
    compatibility_rate = r$compatibility_rate,
    precision_illusion_gap = r$precision_illusion_gap,
    resolved_model_count = as.integer(r$resolved_model_count),
    partial_model_count = as.integer(r$partial_model_count),
    design = r$design,
    diagnostics = diagnostics
  )
}


.parse_lynchpin_results <- function(r) {
  parsed <- list(
    compatibility_metric = r$compatibility_metric,
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

  optional_integer <- intersect(c(
    "models_retained", "dyads_retained", "models_changed"
  ), names(r))
  for (field in optional_integer) {
    parsed[[field]] <- as.integer(r[[field]] %||% NA_integer_)
  }
  optional_numeric <- intersect(c("mapping_coverage"), names(r))
  for (field in optional_numeric) {
    parsed[[field]] <- as.numeric(r[[field]] %||% NA_real_)
  }
  if ("crux_mode" %in% names(r)) {
    parsed$crux_mode <- as.character(
      r$crux_mode %||% NA_character_
    )
  }
  if ("target_status" %in% names(r)) {
    parsed$target_status <- as.character(
      r$target_status %||% NA_character_
    )
  }
  if ("crux_component_id" %in% names(r)) {
    parsed$crux_component_id <- as.character(
      r$crux_component_id %||% NA_character_
    )
  }

  extra_names <- setdiff(names(r), names(parsed))
  for (field in extra_names) {
    parsed[[field]] <- r[[field]]
  }
  parsed
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
    compatibility_metric = r$compatibility_metric,
    ghost_cluster_found = r$ghost_cluster_found,
    clusters_detected = as.integer(r$clusters_detected),
    ghost_clusters = ghost_df,
    mainstream_cluster = r$mainstream_cluster,
    noise_count = as.integer(r$noise_count),
    total_ghost_models = as.integer(r$total_ghost_models %||% 0L),
    metric_unique_values = unlist(r$metric_unique_values %||% numeric(0)),
    all_pairs_compatible = isTRUE(r$all_pairs_compatible),
    all_pairs_incompatible = isTRUE(r$all_pairs_incompatible),
    profile_variance = r$profile_variance %||% NA_real_,
    degenerate_metric = isTRUE(r$degenerate_metric)
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
  record_names <- unique(unlist(lapply(records, names)))
  known_cols <- c("comp_id", "type", "source", "target", "direction", "description")
  optional_cols <- intersect(c("fixed_status", "observed"), record_names)
  .parse_records_artifact(records, c(known_cols, optional_cols))
}


.parse_state_artifact <- function(records) {
  if (length(records) == 0) {
    return(data.frame(
      model_id = character(0), comp_id = character(0),
      status = character(0), timing = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    model_id = vapply(records, function(x) x$model_id, character(1)),
    comp_id = vapply(records, function(x) x$comp_id, character(1)),
    status = vapply(records, function(x) x$status, character(1)),
    timing = vapply(records, function(x) {
      if (is.null(x$timing)) NA_integer_ else as.integer(x$timing)
    }, integer(1)),
    stringsAsFactors = FALSE
  )
  if (any(vapply(records, function(x) "seeded" %in% names(x), logical(1)))) {
    out$seeded <- vapply(records, function(x) {
      if (is.null(x$seeded)) NA else isTRUE(x$seeded)
    }, logical(1))
  }
  out
}


.parse_extra_artifacts <- function(artifacts) {
  known_core <- c("registry_data", "state_data", "model_ids", "summary_stats")
  extra_names <- setdiff(names(artifacts), known_core)
  if (length(extra_names) == 0) return(list())

  extra <- list()
  for (nm in extra_names) {
    val <- artifacts[[nm]]
    if (is.null(val)) next
    extra[[nm]] <- .parse_extra_artifact_field(val, nm)
  }
  extra
}


.parse_extra_artifact_field <- function(val, name) {
  if (identical(name, "rankings")) {
    return(.parse_delta_u_rankings(val))
  }
  if (identical(name, "embedding_2d")) {
    return(.parse_embedding_artifact(val))
  }
  if (identical(name, "cluster_assignments")) {
    out <- .parse_records_artifact(val, c("model_id", "cluster_id"))
    if (is.data.frame(out)) {
      out$model_id <- as.character(out$model_id)
      out$cluster_id <- as.character(out$cluster_id)
    }
    return(out)
  }
  if (identical(name, "cluster_summaries")) {
    return(.parse_records_artifact(val, NULL))
  }
  if (identical(name, "contrast_analysis")) {
    return(.parse_records_artifact(val, NULL))
  }
  if (identical(name, "prior_model_id")) {
    if (is.list(val)) return(unlist(val))
    return(as.character(val))
  }
  if (identical(name, "plot_data")) {
    return(.parse_plot_data_artifact(val))
  }
  val
}


.parse_embedding_artifact <- function(val) {
  if (is.null(val)) return(NULL)
  if (is.data.frame(val)) return(val)
  model_ids <- unlist(val$model_ids %||% val$model_id %||% NULL)
  xs <- unlist(val$x %||% NULL)
  ys <- unlist(val$y %||% NULL)
  if (is.null(model_ids) || is.null(xs) || is.null(ys)) return(val)
  if (length(model_ids) != length(xs) || length(xs) != length(ys)) return(val)
  data.frame(
    model_id = as.character(model_ids),
    x = as.numeric(xs),
    y = as.numeric(ys),
    stringsAsFactors = FALSE
  )
}


.parse_records_artifact <- function(records, known_cols) {
  if (is.null(records)) return(NULL)
  if (is.data.frame(records)) return(records)
  if (!is.list(records)) return(records)
  if (length(records) == 0) {
    if (is.null(known_cols)) return(records)
    cols <- stats::setNames(rep(list(character(0)), length(known_cols)), known_cols)
    return(as.data.frame(cols, stringsAsFactors = FALSE))
  }

  if (!is.list(records[[1]])) return(records)

  if (is.null(known_cols)) {
    colnames <- unique(unlist(lapply(records, names)))
    if (length(colnames) == 0) return(records)
    known_cols <- colnames
  }

  cols <- lapply(known_cols, function(cn) .record_column(records, cn))
  names(cols) <- known_cols
  df <- as.data.frame(cols, stringsAsFactors = FALSE)

  extra_cols <- setdiff(unique(unlist(lapply(records, names))), known_cols)
  if (length(extra_cols) > 0) {
    for (ec in extra_cols) {
      vec <- lapply(records, function(x) x[[ec]] %||% NA)
      df[[ec]] <- I(vec)
    }
  }
  df
}


.record_column <- function(records, col_name) {
  vals <- lapply(records, function(x) {
    if (col_name %in% names(x) && !is.null(x[[col_name]])) x[[col_name]] else NA
  })

  is_scalar <- vapply(vals, function(x) {
    !is.list(x) && length(x) <= 1L
  }, logical(1), USE.NAMES = FALSE)

  if (!all(is_scalar)) {
    return(I(vals))
  }

  vals <- lapply(vals, function(x) if (length(x) == 0L) NA else x)

  if (any(vapply(vals, is.character, logical(1), USE.NAMES = FALSE))) {
    return(vapply(vals, function(x) {
      if (length(x) == 0L || is.na(x)) NA_character_ else as.character(x)
    }, character(1), USE.NAMES = FALSE))
  }

  if (any(vapply(vals, is.numeric, logical(1), USE.NAMES = FALSE))) {
    return(vapply(vals, function(x) {
      if (length(x) == 0L || is.na(x)) NA_real_ else as.numeric(x)
    }, numeric(1), USE.NAMES = FALSE))
  }

  if (any(vapply(vals, is.logical, logical(1), USE.NAMES = FALSE))) {
    return(vapply(vals, function(x) {
      if (length(x) == 0L || is.na(x)) NA else as.logical(x)
    }, logical(1), USE.NAMES = FALSE))
  }

  vapply(vals, function(x) {
    if (length(x) == 0L || is.na(x)) NA_character_ else as.character(x)
  }, character(1), USE.NAMES = FALSE)
}


.parse_plot_data_artifact <- function(val) {
  if (is.null(val)) return(NULL)
  if (!is.list(val)) return(val)
  out <- list()
  for (nm in names(val)) {
    field <- val[[nm]]
    if (identical(nm, "metadata")) {
      out[[nm]] <- field
    } else if (is.list(field) && length(field) > 0 && is.list(field[[1]])) {
      out[[nm]] <- .parse_records_artifact(field, NULL)
    } else if (is.data.frame(field)) {
      out[[nm]] <- field
    } else {
      out[[nm]] <- field
    }
  }
  out
}

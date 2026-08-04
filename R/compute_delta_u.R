`%||%` <- function(x, y) if (is.null(x)) y else x


#' Compute uncertainty reduction (Delta-U) for component resolution
#'
#' Identifies which theoretical claims, if resolved, would maximize global
#' compatibility across the multiverse. Pass the dyad matrix returned by
#' \code{\link{build_dyad_matrix}} so Delta-U uses the intended registry,
#' states, and dyad records explicitly.
#'
#' @param dyads A dyad matrix data frame returned by
#'   \code{\link{build_dyad_matrix}}. The object must include its
#'   \code{theory_context} attribute, which is added automatically by
#'   \code{build_dyad_matrix()}.
#' @param component_id Optional component ID (e.g. \code{"C0001"}) for
#'   single-component analysis.  When \code{NULL} (default) a ranking of
#'   all uncertain components is returned.
#' @param top_k Maximum number of components to return in ranking mode.
#'   Defaults to 10.  Ignored when \code{component_id} is supplied.
#' @param mode Computation mode: \code{"exhaustive"} (default) evaluates
#'   all uncertain components; \code{"two-stage"} uses a heatmap filter
#'   to reduce the number of full evaluations.
#' @param heatmap_threshold Minimum Delta-U score for a component to be
#'   included in Stage 2 of two-stage mode.  Must be in \eqn{[0, 1]}.
#'   Defaults to \code{0.1} when \code{mode = "two-stage"} and omitted.
#' @param synergistic_set_size When supplied, also evaluate the combined
#'   impact of resolving \emph{sets} of components together.  Must be at
#'   least 2.
#' @param synergistic_search Search strategy for synergistic sets:
#'   \code{"greedy"} (default) or \code{"beam"}.
#' @param synergistic_beam_width Beam width for beam search.  Defaults to 5.
#' @param compatibility_metric Compatibility metric used to score each dyad:
#'   \code{"similarity_rate"} (default), \code{"mas_compatible"}, or
#'   \code{"identified_compatible"}. Exactly one metric is used. The two
#'   causal metrics require \code{exposure} and \code{outcome} in the dyads'
#'   \code{theory_context} attribute.
#' @param crux_mode Crux semantics: \code{"marginal"} (default) ranks each
#'   uncertain component by evaluating both causal and non-causal resolutions;
#'   \code{"global"} resolves every applicable unknown edge instance to a
#'   single user-selected status and compares the remapped multiverse against
#'   the unchanged baseline.
#' @param global_status Required status for \code{crux_mode = "global"}:
#'   \code{"causal"} or \code{"non-causal"}. Must be \code{NULL} in marginal
#'   mode.
#' @param device Compute device: \code{"auto"} (default), \code{"cpu"}, or
#'   \code{"cuda"}.  \code{"cuda"} requires a CUDA-capable GPU with PyTorch.
#' @param use_tensor_engine Whether to use the tensorized structural engine
#'   when \code{compatibility_metric = "similarity_rate"} (default
#'   \code{TRUE}). Improves performance on larger state spaces.
#' @param url Base URL of the theoRy Python backend API.  Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return By default, a data frame with columns:
#'   \item{rank}{Rank (1 = highest impact)}
#'   \item{component_id}{Component identifier}
#'   \item{type}{Component type (\code{"node"} or \code{"edge"})}
#'   \item{source}{Node source name}
#'   \item{target}{Node target name (\code{NA} for nodes)}
#'   \item{delta_u}{Delta-U score (mean compatibility improvement)}
#'   \item{best_resolution}{Recommended resolution (\code{"causal"},
#'     \code{"non-causal"}, or \code{"none"})}
#'   \item{dyads_improved}{Number of dyads that improved}
#'   \item{dyads_worsened}{Number of dyads that worsened}
#'   When supplied by the backend, the data frame also includes
#'   \code{baseline_compatibility}, \code{post_compatibility_causal},
#'   \code{post_compatibility_non_causal}, \code{models_changed_causal},
#'   \code{models_changed_non_causal}, \code{mapping_coverage_causal},
#'   \code{mapping_coverage_non_causal}, and \code{crux_mode}.
#'
#'   When \code{component_id} is supplied, a one-row data frame with the
#'   same columns plus \code{delta_u_causal} and \code{delta_u_non_causal}.
#'
#'   When \code{synergistic_set_size} is supplied, a list with components
#'   \code{rankings} (data frame) and \code{synergistic_sets} (data frame
#'   with columns \code{components}, \code{delta_u_combined},
#'   \code{delta_u_individual_sum}, \code{synergy_score}, \code{label}).
#'   The returned data frame or list has a \code{compatibility_metric}
#'   attribute recording the selected metric.
#'
#'   With \code{crux_mode = "global"}, a one-row data frame with
#'   \code{crux_mode}, \code{target_status}, \code{feasible},
#'   \code{baseline_compatibility}, \code{post_compatibility},
#'   \code{compatibility_change}, \code{delta_u}, \code{model_count},
#'   \code{dyad_count}, \code{models_changed}, \code{unknown_instances_forced},
#'   \code{dyads_improved}, \code{dyads_worsened}, and
#'   \code{mapping_coverage}.
#'
#' @details
#' Delta-U simulates what would happen if each uncertain (unknown)
#' \strong{applicable edge} were resolved.  For each edge and each possible
#' resolution direction (causal / non-causal), it recomputes dyadic
#' compatibility scores and measures the average change in global
#' compatibility.  The component with the largest positive delta is
#' the \strong{Lynchpin} that maximally reduces theoretical uncertainty.
#'
#' Under sparse node semantics, only edge components that are \code{"unknown"}
#' in at least one model where the edge is applicable are considered as
#' candidates.  Absent nodes and inapplicable edges are excluded from
#' default Delta-U candidate selection.
#'
#' Both crux modes are \emph{model-remapping} analyses: hypothetical
#' resolutions never mutate model claims and never recompute causal profiles.
#' Each affected model is mapped to the existing multiverse model whose
#' semantic state (node presence, applicable edge statuses, timing,
#' constraints) is identical except for the resolved edge(s), and the mapped
#' models' existing dyad records are reused.  The model and dyad counts are
#' preserved.  This requires a \emph{resolution-closed} multiverse; when an
#' exact match is missing, marginal ranking and strict resolutions fail with
#' a completion-coverage error instead of synthesizing new models.
#'
#' Edge components respect temporal integrity and joint acyclicity: a
#' resolution that would violate timing or create a cycle is reported as
#' infeasible rather than silently skipped.
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
#' rankings <- compute_delta_u(dyads, top_k = 5)
#' head(rankings)
#'
#' single <- compute_delta_u(dyads, component_id = rankings$component_id[1])
#'
#' syn <- compute_delta_u(dyads, top_k = 10, synergistic_set_size = 2)
#' syn$synergistic_sets
#'
#' causal_dyads <- build_dyad_matrix(
#'   reg, states, mode = "full", exposure = "X", outcome = "Z"
#' )
#' causal_rankings <- compute_delta_u(
#'   causal_dyads, top_k = 10,
#'   compatibility_metric = "identified_compatible"
#' )
#' }
#'
#' @export
compute_delta_u <- function(dyads,
                            component_id = NULL,
                            top_k = 10L,
                            mode = c("exhaustive", "two-stage"),
                            heatmap_threshold = NULL,
                            synergistic_set_size = NULL,
                            synergistic_search = c("greedy", "beam"),
                            synergistic_beam_width = NULL,
                            compatibility_metric = c("similarity_rate",
                                                     "mas_compatible",
                                                     "identified_compatible"),
                            crux_mode = c("marginal", "global"),
                            global_status = NULL,
                            device = c("auto", "cpu", "cuda"),
                            use_tensor_engine = TRUE,
                            url = getOption("theoRy.engine_url",
                                             "http://localhost:8000")) {
  mode <- match.arg(mode)
  compatibility_metric <- match.arg(compatibility_metric)
  crux_mode <- match.arg(crux_mode)
  device <- match.arg(device)

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

  if (!is.numeric(top_k) || length(top_k) != 1L || is.na(top_k) ||
      !is.finite(top_k) ||
      top_k < 1 || top_k != as.integer(top_k)) {
    stop("top_k must be positive integer-valued.", call. = FALSE)
  }
  if (identical(mode, "two-stage")) {
    if (is.null(heatmap_threshold)) {
      heatmap_threshold <- 0.1
    }
    if (!is.numeric(heatmap_threshold) ||
        heatmap_threshold < 0 || heatmap_threshold > 1) {
      stop("heatmap_threshold must be between 0 and 1.", call. = FALSE)
    }
  }
  if (!is.null(synergistic_set_size)) {
    if (!is.numeric(synergistic_set_size) || synergistic_set_size < 2) {
      stop("synergistic_set_size must be at least 2.", call. = FALSE)
    }
    synergistic_search <- match.arg(synergistic_search)
  }
  if (!is.null(synergistic_beam_width) &&
      (!is.numeric(synergistic_beam_width) || synergistic_beam_width <= 0)) {
    stop("synergistic_beam_width must be positive.", call. = FALSE)
  }

  if (!is.null(global_status)) {
    global_status <- match.arg(global_status, c("causal", "non-causal"))
  }
  if (identical(crux_mode, "global")) {
    if (is.null(global_status)) {
      stop("global_status ('causal' or 'non-causal') is required when ",
           "crux_mode = 'global'.", call. = FALSE)
    }
    if (!is.null(component_id)) {
      stop("component_id is not used with crux_mode = 'global'.", call. = FALSE)
    }
    if (identical(mode, "two-stage")) {
      stop("crux_mode = 'global' does not support two-stage mode.",
           call. = FALSE)
    }
    if (!is.null(synergistic_set_size)) {
      stop("crux_mode = 'global' does not support synergistic sets.",
           call. = FALSE)
    }
  } else if (!is.null(global_status)) {
    stop("global_status is only valid with crux_mode = 'global'.",
         call. = FALSE)
  }

  causal_metric <- compatibility_metric %in%
    c("mas_compatible", "identified_compatible")
  if (causal_metric &&
      (is.null(context$exposure) || is.null(context$outcome))) {
    stop(
      "compatibility_metric = '", compatibility_metric,
      "' requires exposure and outcome in the dyads' theory_context. ",
      "Recreate dyads with build_dyad_matrix(..., exposure = ..., outcome = ...).",
      call. = FALSE
    )
  }
  if (causal_metric && !compatibility_metric %in% names(dyads)) {
    stop(
      "compatibility_metric = '", compatibility_metric,
      "' requires full dyads (build_dyad_matrix(mode = 'full')). ",
      "The column '", compatibility_metric, "' is missing from dyads.",
      call. = FALSE
    )
  }

  payload <- list(
    registry_data = context$registry_data,
    state_data = context$state_data,
    model_ids = I(context$model_ids),
    dyads = .delta_u_dyads_to_records(dyads),
    top_k = as.integer(top_k),
    mode = mode,
    compatibility_metric = compatibility_metric,
    crux_mode = crux_mode
  )

  if (!is.null(global_status)) {
    payload$global_status <- global_status
  }
  if (!is.null(component_id)) {
    payload$component_id <- component_id
  }
  if (!is.null(heatmap_threshold)) {
    payload$heatmap_threshold <- heatmap_threshold
  }
  if (!is.null(synergistic_set_size)) {
    payload$synergistic_set_size <- as.integer(synergistic_set_size)
    payload$synergistic_search <- synergistic_search %||% "greedy"
    payload$synergistic_beam_width <- synergistic_beam_width %||% 5L
  }
  if (!is.null(context$exposure) && !is.null(context$outcome)) {
    payload$exposure <- context$exposure
    payload$outcome <- context$outcome
  }

  payload$device <- device
  payload$use_tensor_engine <- use_tensor_engine

  req <- httr2::request(url) |>
    httr2::req_url_path("api/v1/delta-u") |>
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
    stop("Backend error [", code, "]: ", message, call. = FALSE)
  }

  if (!identical(body$status, "success") || is.null(body$data)) {
    stop("Invalid backend response: missing data.", call. = FALSE)
  }

  data <- body$data
  rankings <- NULL
  synergistic <- NULL
  global_crux <- NULL

  if (!is.null(data$global_result)) {
    global_crux <- .parse_global_crux(data$global_result)
  } else if (!is.null(data$result)) {
    rankings <- .parse_single_result(data$result, context$registry_data)
  } else if (!is.null(data$rankings)) {
    rankings <- .parse_delta_u_rankings(data$rankings)
  }

  if (!is.null(data$synergistic_sets)) {
    synergistic <- .parse_synergistic_sets(data$synergistic_sets)
  }

  if (!is.null(rankings)) {
    attr(rankings, "compatibility_metric") <-
      data$compatibility_metric %||% compatibility_metric
    if (!is.null(data$device)) {
      attr(rankings, "device") <- data$device
    }
  }

  if (!is.null(global_crux)) {
    attr(global_crux, "compatibility_metric") <-
      data$compatibility_metric %||% compatibility_metric
    if (!is.null(data$device)) {
      attr(global_crux, "device") <- data$device
    }
    return(global_crux)
  }

  if (!is.null(synergistic)) {
    result <- list(rankings = rankings, synergistic_sets = synergistic)
    attr(result, "compatibility_metric") <-
      data$compatibility_metric %||% compatibility_metric
    if (!is.null(data$device)) {
      attr(result, "device") <- data$device
    }
    return(result)
  }

  rankings
}


.delta_u_dyads_to_records <- function(dyads) {
  valid_metrics <- c(
    "similarity_rate", "mas_compatible", "identified_compatible"
  )

  required <- c("dyad_id", "ego_id", "alter_id", "similarity_rate")
  missing_cols <- setdiff(required, names(dyads))
  if (length(missing_cols) > 0) {
    stop("dyads is missing required column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  lapply(seq_len(nrow(dyads)), function(i) {
    row <- dyads[i, , drop = FALSE]
    entry <- list(
      dyad_id = as.character(row$dyad_id),
      ego_id = as.character(row$ego_id),
      alter_id = as.character(row$alter_id),
      similarity_rate = as.numeric(row$similarity_rate)
    )
    # Selected compatibility metric(s) present in the dyads (similarity_rate
    # is already set above and must not be coerced to logical)
    metric_fields <- setdiff(intersect(valid_metrics, names(dyads)), "similarity_rate")
    for (field in metric_fields) {
      value <- dyads[[field]][i]
      entry[[field]] <- if (is.null(value) || is.na(value)) NULL else as.logical(value)
    }
    # Per-model profile fields needed for copied/self-source dyads
    logical_profiles <- c("identified_ego", "identified_alter")
    for (field in intersect(logical_profiles, names(dyads))) {
      value <- dyads[[field]][i]
      entry[field] <- list(
        if (is.null(value) || is.na(value)) NULL else as.logical(value)
      )
    }
    list_profiles <- c(
      "mas_ego", "mas_alter",
      "identification_nodes_ego", "identification_nodes_alter"
    )
    for (field in intersect(list_profiles, names(dyads))) {
      value <- dyads[[field]][[i]]
      if (!is.null(value) && startsWith(field, "identification_nodes_")) {
        value <- unname(as.list(as.character(value)))
      }
      entry[field] <- list(value)
    }
    entry
  })
}


.parse_single_result <- function(result, registry_data = NULL) {
  meta <- .component_metadata(result$component_id, registry_data)
  row <- list(
    rank = 1L,
    component_id = result$component_id %||% NA_character_,
    type = result$type %||% meta$type %||% NA_character_,
    source = result$source %||% meta$source %||% NA_character_,
    target = if (!is.null(result$target)) {
      result$target
    } else {
      meta$target %||% NA_character_
    },
    delta_u = result$delta_u %||% NA_real_,
    best_resolution = result$best_resolution %||% NA_character_,
    dyads_improved = as.integer(result$dyads_improved %||% NA_integer_),
    dyads_worsened = as.integer(result$dyads_worsened %||% NA_integer_),
    delta_u_causal = result$delta_u_causal %||% NA_real_,
    delta_u_non_causal = result$delta_u_non_causal %||% NA_real_
  )

  optional_numeric <- c(
    "baseline_compatibility", "post_compatibility_causal",
    "post_compatibility_non_causal", "mapping_coverage_causal",
    "mapping_coverage_non_causal"
  )
  for (field in intersect(optional_numeric, names(result))) {
    row[[field]] <- as.numeric(result[[field]] %||% NA_real_)
  }
  optional_integer <- c(
    "models_changed_causal", "models_changed_non_causal",
    "instances_forced_causal", "instances_forced_non_causal"
  )
  for (field in intersect(optional_integer, names(result))) {
    row[[field]] <- as.integer(result[[field]] %||% NA_integer_)
  }
  if ("crux_mode" %in% names(result)) {
    row$crux_mode <- as.character(
      result$crux_mode %||% NA_character_
    )
  }
  data.frame(row, stringsAsFactors = FALSE)
}


.component_metadata <- function(component_id, registry_data = NULL) {
  empty <- list(type = NULL, source = NULL, target = NULL)
  if (is.null(component_id) || is.null(registry_data)) {
    return(empty)
  }

  if (is.data.frame(registry_data)) {
    rows <- registry_data[registry_data$comp_id == component_id, , drop = FALSE]
    if (nrow(rows) < 1) {
      return(empty)
    }
    target <- rows$target[[1]]
    if (is.null(target) || is.na(target)) {
      target <- NULL
    }
    return(list(
      type = rows$type[[1]],
      source = rows$source[[1]],
      target = target
    ))
  }

  matches <- Filter(function(x) identical(x$comp_id, component_id), registry_data)
  if (length(matches) < 1) {
    return(empty)
  }
  row <- matches[[1]]
  list(
    type = row$type %||% NULL,
    source = row$source %||% NULL,
    target = row$target %||% NULL
  )
}


.parse_delta_u_rankings <- function(rankings) {
  if (length(rankings) == 0) {
    return(data.frame(
      rank = integer(0), component_id = character(0),
      type = character(0), source = character(0),
      target = character(0), delta_u = numeric(0),
      best_resolution = character(0),
      dyads_improved = integer(0), dyads_worsened = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  df <- data.frame(
    rank = vapply(rankings, function(r) as.integer(r$rank %||% NA_integer_),
                  integer(1), USE.NAMES = FALSE),
    component_id = vapply(rankings, function(r) r$component_id %||% NA_character_,
                          character(1), USE.NAMES = FALSE),
    type = vapply(rankings, function(r) r$type %||% NA_character_,
                  character(1), USE.NAMES = FALSE),
    source = vapply(rankings, function(r) r$source %||% NA_character_,
                    character(1), USE.NAMES = FALSE),
    target = vapply(rankings, function(r) {
      if (is.null(r$target)) NA_character_ else r$target
    }, character(1), USE.NAMES = FALSE),
    delta_u = vapply(rankings, function(r) r$delta_u %||% NA_real_,
                     numeric(1), USE.NAMES = FALSE),
    best_resolution = vapply(rankings, function(r) r$best_resolution %||%
                               NA_character_,
                             character(1), USE.NAMES = FALSE),
    dyads_improved = vapply(rankings, function(r) {
      as.integer(r$dyads_improved %||% NA_integer_)
    }, integer(1), USE.NAMES = FALSE),
    dyads_worsened = vapply(rankings, function(r) {
      as.integer(r$dyads_worsened %||% NA_integer_)
    }, integer(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )

  record_names <- unique(unlist(lapply(rankings, names)))
  optional_numeric <- intersect(c(
    "baseline_compatibility", "post_compatibility_causal",
    "post_compatibility_non_causal", "mapping_coverage_causal",
    "mapping_coverage_non_causal"
  ), record_names)
  for (field in optional_numeric) {
    df[[field]] <- vapply(rankings, function(r) {
      as.numeric(r[[field]] %||% NA_real_)
    }, numeric(1), USE.NAMES = FALSE)
  }
  optional_integer <- intersect(c(
    "models_changed_causal", "models_changed_non_causal",
    "instances_forced_causal", "instances_forced_non_causal"
  ), record_names)
  for (field in optional_integer) {
    df[[field]] <- vapply(rankings, function(r) {
      as.integer(r[[field]] %||% NA_integer_)
    }, integer(1), USE.NAMES = FALSE)
  }
  if ("crux_mode" %in% record_names) {
    df$crux_mode <- vapply(rankings, function(r) {
      as.character(r$crux_mode %||% NA_character_)
    }, character(1), USE.NAMES = FALSE)
  }

  df
}


.parse_global_crux <- function(result) {
  if (is.null(result)) {
    return(NULL)
  }
  row <- list(
    crux_mode = result$crux_mode %||% NA_character_,
    target_status = result$target_status %||% NA_character_,
    feasible = result$feasible %||% NA,
    baseline_compatibility = result$baseline_compatibility %||% NA_real_,
    post_compatibility = result$post_compatibility %||% NA_real_,
    compatibility_change = result$compatibility_change %||% NA_real_,
    delta_u = result$delta_u %||% NA_real_,
    model_count = as.integer(result$model_count %||% NA_integer_),
    dyad_count = as.integer(result$dyad_count %||% NA_integer_),
    models_changed = as.integer(result$models_changed %||% NA_integer_),
    unknown_instances_forced = as.integer(
      result$unknown_instances_forced %||% NA_integer_
    ),
    dyads_improved = as.integer(result$dyads_improved %||% NA_integer_),
    dyads_worsened = as.integer(result$dyads_worsened %||% NA_integer_),
    mapping_coverage = result$mapping_coverage %||% NA_real_
  )
  data.frame(row, stringsAsFactors = FALSE)
}


.parse_synergistic_sets <- function(sets) {
  if (length(sets) == 0) {
    return(data.frame(
      rank = integer(0),
      components = I(list()),
      delta_u_combined = numeric(0),
      delta_u_individual_sum = numeric(0),
      synergy_score = numeric(0),
      label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  comps <- lapply(sets, function(s) {
    if (is.list(s$components)) unlist(s$components) else s$components
  })

  data.frame(
    rank = vapply(sets, function(s) as.integer(s$rank %||% NA_integer_),
                  integer(1), USE.NAMES = FALSE),
    components = I(comps),
    delta_u_combined = vapply(sets, function(s) s$delta_u_combined %||% NA_real_,
                              numeric(1), USE.NAMES = FALSE),
    delta_u_individual_sum = vapply(sets, function(s) {
      s$delta_u_individual_sum %||% NA_real_
    }, numeric(1), USE.NAMES = FALSE),
    synergy_score = vapply(sets, function(s) s$synergy_score %||% NA_real_,
                           numeric(1), USE.NAMES = FALSE),
    label = vapply(sets, function(s) s$label %||% NA_character_,
                   character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

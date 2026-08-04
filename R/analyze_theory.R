`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run the full theoRy analysis pipeline in one call
#'
#' Chains the five core theoRy pipeline steps - component registry,
#' model-state expansion, dyad matrix, Delta-U crux ranking, and
#' (optionally) ghost cluster detection - with sensible defaults and
#' progress messages. Optionally generates standard visualizations.
#'
#' @param nodes Character vector of variable names in \code{"programmatic"}
#'   input mode. Omit it in \code{"interactive"} mode.
#' @param timing Integer vector of chronological positions, parallel to
#'   \code{nodes}. When \code{NULL} and both \code{exposure} and
#'   \code{outcome} are supplied, programmatic mode requires
#'   \code{time_points} for unspecified non-focal nodes.
#' @param exposure Name of the exposure variable. Required in programmatic
#'   mode and collected in interactive mode.
#' @param outcome Name of the outcome variable. Required in programmatic mode
#'   and collected in interactive mode.
#' @param prior_model Optional model ID (e.g. \code{"M0001"}) for ghost
#'   cluster detection contrast analysis. When \code{NULL} (default),
#'   ghost detection is skipped.
#' @param mode Model expansion mode: \code{"exhaustive"} (default),
#'   \code{"sampled"}, or \code{"symbolic"}. Concrete marginal/global crux
#'   analysis requires a resolution-closed multiverse. Sampled expansion is
#'   therefore suitable only when the resulting sample is resolution-closed;
#'   otherwise Delta-U reports a completion-coverage error.
#' @param n_models Number of models to sample in \code{"sampled"} mode.
#'   Defaults to 200.
#' @param seed Random seed for reproducible sampling. Defaults to 42.
#' @param node_policy Controls node-subset generation: \code{"all-present"}
#'   (default) includes every node in every model; \code{"vary"} generates
#'   models with variable node scope. With \code{"vary"}, both
#'   \code{exposure} and \code{outcome} must be present. Ignored in
#'   \code{"symbolic"} mode.
#' @param top_k Maximum number of components to return in the Delta-U
#'   ranking. Defaults to 10.
#' @param crux_mode Crux semantics forwarded to
#'   \code{\link{compute_delta_u}}: \code{"marginal"} (default) ranks
#'   uncertain components; \code{"global"} resolves every applicable unknown
#'   edge instance to one status. Not available in symbolic mode.
#' @param global_status Required status (\code{"causal"} or
#'   \code{"non-causal"}) for \code{crux_mode = "global"}. Must be
#'   \code{NULL} in marginal mode.
#' @param plot Logical. When \code{TRUE}, generate standard plots
#'   (dyad heatmap, crux ranking, cluster embedding when available).
#'   Defaults to \code{FALSE}.
#' @param eps DBSCAN eps parameter for ghost detection. Defaults to 0.5.
#' @param min_samples DBSCAN min_samples parameter. Defaults to 5.
#' @param url Base URL of the theoRy Python backend. Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#' @param input_mode Either \code{"programmatic"} (default) or
#'   \code{"interactive"}. Interactive mode runs the guided registry
#'   questionnaire before the analysis and prints a programmatic call that
#'   recreates the selected multiverse.
#' @param constraints,include_bidirectional,time_points,timing_options Registry
#'   options forwarded to
#'   \code{build_component_registry()} in programmatic mode.
#' @param optional_nodes Character vector of nodes allowed to be absent,
#'   forwarded to \code{build_component_registry()} in programmatic mode.
#' @param max_models,allow_large Expansion safety controls forwarded to
#'   \code{expand_model_states()}.
#' @param causal_backend Causal backend passed to \code{build_dyad_matrix()}.
#'   Defaults to \code{"r"} for general identification through
#'   Dagitty/CausalEffect.
#'
#' @return A list with components:
#'   \item{registry}{Data frame from \code{\link{build_component_registry}}.}
#'   \item{states}{Data frame from \code{\link{expand_model_states}}.}
#'   \item{dyads}{Data frame from \code{\link{build_dyad_matrix}}.}
#'   \item{delta_u_rankings}{Data frame from \code{\link{compute_delta_u}}.}
#'   \item{ghost_clusters}{List from \code{\link{detect_ghost_clusters}} or
#'     \code{NULL} when no prior model is supplied.}
#'   \item{summary}{Character vector of human-readable key findings, including
#'     model and non-outcome-node counts, available-dyad compatibility
#'     percentages, the most common MAS, the MAS uniquely enabling the most
#'     compatible model dyads, the top Delta-U crux, and available ghost-cluster
#'     counts. With \code{node_policy = "vary"}, the summary also identifies
#'     the relevant node most often differing between identified-incompatible
#'     pairs where both effects and both relevant-node sets are available.}
#'   \item{plots}{Named list of \code{ggplot} objects or \code{NULL} when
#'     \code{plot = FALSE}.}
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' result <- analyze_theory(
#'   nodes = c("X1", "X2", "Y"),
#'   timing = c(1, 2, 3),
#'   exposure = "X1",
#'   outcome = "Y",
#'   prior_model = "M0001",
#'   plot = TRUE
#' )
#' cat(result$summary, sep = "\n")
#' for (p in result$plots) print(p)
#'
#' stop_theory_engine()
#' }
#'
#' @export
analyze_theory <- function(nodes = NULL,
                             timing = NULL,
                             exposure = NULL,
                             outcome = NULL,
                             prior_model = NULL,
                             mode = c("exhaustive", "sampled", "symbolic"),
                             n_models = 200L,
                             seed = 42L,
                             node_policy = c("all-present", "vary"),
                             top_k = 10L,
                             crux_mode = c("marginal", "global"),
                             global_status = NULL,
                             plot = FALSE,
                             eps = 0.5,
                             min_samples = 5L,
                             url = getOption("theoRy.engine_url",
                                              "http://localhost:8000"),
                             input_mode = c("programmatic", "interactive"),
                             constraints = NULL,
                             include_bidirectional = FALSE,
                             time_points = NULL,
                             timing_options = NULL,
                             optional_nodes = character(),
                             max_models = 10000L,
                             allow_large = FALSE,
                             causal_backend = c("r", "auto", "native")) {
  mode <- match.arg(mode)
  node_policy <- match.arg(node_policy)
  input_mode <- match.arg(input_mode)
  causal_backend <- match.arg(causal_backend)
  crux_mode <- match.arg(crux_mode)
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

  # ── 0. Health check ──────────────────────────────────────────────────────────
  alive <- tryCatch(
    httr2::request(paste0(url, "/api/v1/health")) |>
      httr2::req_timeout(3) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )
  if (is.null(alive) || !identical(alive$status, "success")) {
    stop("Theory engine not running at ", url,
         ". Call start_theory_engine() first.", call. = FALSE)
  }

  if (identical(input_mode, "interactive") && identical(mode, "symbolic")) {
    stop("input_mode = 'interactive' is not available in symbolic mode.",
         call. = FALSE)
  }
  if (identical(mode, "symbolic")) {
    if (is.null(exposure) || is.null(outcome)) {
      stop("exposure and outcome are required.", call. = FALSE)
    }
    if (identical(crux_mode, "global")) {
      stop("crux_mode = 'global' is not available in symbolic mode.",
           call. = FALSE)
    }
    message("Step 1/5: Building symbolic multiverse...")
    symbolic <- build_symbolic_multiverse(
      nodes = data.frame(name = nodes, timing = timing, stringsAsFactors = FALSE),
      exposure = exposure,
      outcome = outcome,
      mode = "full",
      url = url
    )

    message("Step 2/5: Computing symbolic query classes...")
    sym_req <- list(
      nodes = lapply(seq_along(nodes), function(i) list(name = nodes[i], timing = timing[i])),
      exposure = exposure,
      outcome = outcome,
      mode = "sampled",
      n_samples = 500L,
      signature_policy = "paper_v1"
    )
    resp <- httr2::request(paste0(url, "/api/v1/symbolic/query-classes")) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(sym_req) |>
      httr2::req_perform()
    query_classes <- httr2::resp_body_json(resp)$data
    class(query_classes) <- c("theory_symbolic_classes", "list")

    message("Step 3/5: Computing symbolic Delta-U...")
    delta_u <- compute_symbolic_delta_u(symbolic, top_k = as.integer(top_k),
                                         mode = "sampled", n_samples = 500L, url = url)

    ghost_result <- NULL
    if (!is.null(prior_model)) {
      message("Step 4/5: Detecting symbolic ghost clusters...")
    } else {
      message("Step 4/5: Skipping ghost detection (no prior model supplied).")
    }

    summary <- character(0)
    summary <- c(summary, sprintf("Symbolic mode: %d edge variables", symbolic$edge_count))

    if (is.list(delta_u$results) && length(delta_u$results) > 0) {
      top <- delta_u$results[[1]]
      summary <- c(summary, sprintf("Top crux: %s (%s \u2192 %s, delta_u = %.4f)",
                                     top$component_id, top$source, top$target, top$delta_u))
    }

    plots <- NULL

    return(list(
      registry = NULL,
      states = NULL,
      dyads = query_classes,
      delta_u_rankings = delta_u,
      ghost_clusters = ghost_result,
      summary = summary,
      plots = plots
    ))
  }

  # ── 1. Build registry ────────────────────────────────────────────────────────
  message("Step 1/5: Building component registry...")
  if (identical(input_mode, "interactive")) {
    registry <- build_component_registry_interactive(url = url)
    exposure <- attr(registry, "exposure")
    outcome <- attr(registry, "outcome")
    timing_options <- attr(registry, "timing_options")
    optional_nodes <- attr(registry, "optional_nodes")
  } else {
    registry <- build_component_registry(
      nodes = nodes,
      timing = timing,
      exposure = exposure,
      outcome = outcome,
      constraints = constraints,
      include_bidirectional = include_bidirectional,
      time_points = time_points,
      timing_options = timing_options,
      optional_nodes = optional_nodes,
      url = url
    )
  }

  # ── 2. Expand model states ───────────────────────────────────────────────────
  message("Step 2/5: Expanding model states...")
  if (identical(mode, "exhaustive")) {
    states <- expand_model_states(
      registry,
      mode = "exhaustive",
      node_policy = node_policy,
      timing_options = timing_options,
      optional_nodes = optional_nodes,
      max_models = max_models,
      allow_large = allow_large,
      url = url
    )
    model_count <- length(unique(states$model_id))
    if (model_count > 10000) {
      warning("Exhaustive expansion produced ", model_count,
              " models; dyad and crux computation may be expensive.",
              call. = FALSE)
    }
  } else {
    states <- expand_model_states(
      registry, mode = "sampled",
      n_models = as.integer(n_models),
      seed = as.integer(seed),
      node_policy = node_policy,
      timing_options = timing_options,
      optional_nodes = optional_nodes,
      max_models = max_models,
      allow_large = allow_large,
      url = url
    )
  }

  # ── 3. Build dyad matrix ─────────────────────────────────────────────────────
  message("Step 3/5: Computing dyad matrix...")
  dyads <- build_dyad_matrix(
    registry, states,
    mode = "full",
    exposure = exposure,
    outcome = outcome,
    causal_backend = causal_backend,
    url = url
  )

  # ── 4. Compute Delta-U ───────────────────────────────────────────────────────
  message("Step 4/5: Computing Delta-U crux rankings...")
  delta_u <- compute_delta_u(
    dyads,
    top_k = as.integer(top_k),
    crux_mode = crux_mode,
    global_status = global_status,
    url = url
  )

  # ── 5. Ghost detection ───────────────────────────────────────────────────────
  ghost_result <- NULL
  if (!is.null(prior_model)) {
    message("Step 5/5: Detecting ghost clusters...")
    ghost_result <- detect_ghost_clusters(
      dyads,
      prior_model = prior_model,
      eps = eps,
      min_samples = as.integer(min_samples),
      url = url
    )
  } else {
    message("Step 5/5: Skipping ghost detection (no prior model supplied).")
  }

  # ── Build summary ────────────────────────────────────────────────────────────
  summary <- character(0)
  summary <- c(summary, sprintf("Models: %d",
                                 length(unique(states$model_id))))
  x_variables <- unique(registry$source[
    registry$type == "node" & registry$source != outcome
  ])
  summary <- c(summary, sprintf("X variables (non-outcome nodes): %d",
                                 length(x_variables)))
  summary <- c(summary, sprintf("Components: %d", nrow(registry)))
  summary <- c(
    summary,
    .analyze_theory_compatibility_summary(
      dyads, "mas_compatible", "MAS"
    ),
    .analyze_theory_identified_models_summary(
      dyads, unique(states$model_id)
    ),
    .analyze_theory_compatibility_summary(
      dyads, "identified_compatible", "Identified"
    )
  )
  summary <- c(summary, .analyze_theory_mas_summary(dyads))
  summary <- c(
    summary,
    .analyze_theory_missing_component_summary(
      dyads, registry, node_policy
    )
  )

  if (is.data.frame(delta_u) && nrow(delta_u) > 0) {
    if (identical(delta_u$crux_mode[1], "global")) {
      summary <- c(
        summary,
        sprintf("Global crux (%s): compatibility %.4f -> %.4f (change %+.4f)",
                delta_u$target_status[1],
                delta_u$baseline_compatibility[1],
                delta_u$post_compatibility[1],
                delta_u$compatibility_change[1])
      )
    } else {
      top <- delta_u[1, ]
      src <- top$source %||% ""
      tgt <- top$target %||% ""
      resolution <- top$best_resolution %||% "none"
      summary <- c(summary, sprintf(
        "Top crux: %s (%s \u2192 %s, resolution = %s, delta_u = %.4f)",
        top$component_id, src, tgt, resolution, top$delta_u
      ))
    }
  }

  if (!is.null(ghost_result) &&
      is.data.frame(ghost_result$ghost_clusters) &&
      nrow(ghost_result$ghost_clusters) > 0) {
    summary <- c(summary, sprintf("Ghost clusters found: %d",
                                   nrow(ghost_result$ghost_clusters)))
  }

  # ── Build plots ──────────────────────────────────────────────────────────────
  plots <- NULL
  if (isTRUE(plot)) {
    plots <- list()
    plots$dyad_heatmap <- plot_dyad_heatmap(dyads)

    if (is.data.frame(delta_u) && nrow(delta_u) > 0 &&
        !identical(delta_u$crux_mode[1], "global")) {
      plots$crux_ranking <- plot_lynchpin_ranking(delta_u)
    }

    if (!is.null(ghost_result)) {
      plots$cluster_embedding <- plot_cluster_embedding(ghost_result)
    }
  }

  if (identical(input_mode, "interactive")) {
    cat("\n--- Recreate this multiverse programmatically ---\n")
    cat(
      .analyze_theory_programmatic_call(
        registry = registry,
        prior_model = prior_model,
        mode = mode,
        n_models = n_models,
        seed = seed,
        node_policy = node_policy,
        top_k = top_k,
        crux_mode = crux_mode,
        global_status = global_status,
        plot = plot,
        eps = eps,
        min_samples = min_samples,
        url = url,
        max_models = max_models,
        allow_large = allow_large,
        causal_backend = causal_backend
      ),
      sep = "\n"
    )
    cat("\n")
  }

  # ── Return ───────────────────────────────────────────────────────────────────
  list(
    registry = registry,
    states = states,
    dyads = dyads,
    delta_u_rankings = delta_u,
    ghost_clusters = ghost_result,
    summary = summary,
    plots = plots
  )
}


.analyze_theory_programmatic_call <- function(registry,
                                               prior_model,
                                               mode,
                                               n_models,
                                               seed,
                                               node_policy,
                                               top_k,
                                               crux_mode,
                                               global_status,
                                               plot,
                                               eps,
                                               min_samples,
                                               url,
                                               max_models,
                                               allow_large,
                                               causal_backend) {
  nodes <- as.character(registry$source[registry$type == "node"])
  timing_options <- attr(registry, "timing_options")
  timing <- unname(vapply(nodes, function(node) {
    options <- timing_options[[node]]
    if (length(options) == 1L) as.integer(options[[1]]) else NA_integer_
  }, integer(1)))

  arguments <- list(
    list("nodes", nodes),
    list("timing", timing),
    list("exposure", attr(registry, "exposure")),
    list("outcome", attr(registry, "outcome")),
    list("constraints", attr(registry, "constraints")),
    list("include_bidirectional", FALSE),
    list("timing_options", timing_options),
    list("optional_nodes", attr(registry, "optional_nodes")),
    list("mode", mode),
    list("node_policy", node_policy),
    list("top_k", as.integer(top_k)),
    list("crux_mode", crux_mode),
    list("global_status", global_status),
    list("plot", isTRUE(plot)),
    list("eps", eps),
    list("min_samples", as.integer(min_samples)),
    list("url", url),
    list("max_models", as.integer(max_models)),
    list("allow_large", isTRUE(allow_large)),
    list("causal_backend", causal_backend),
    list("input_mode", "programmatic")
  )
  if (identical(mode, "sampled")) {
    arguments <- append(
      arguments,
      list(list("n_models", as.integer(n_models)), list("seed", as.integer(seed))),
      after = 9L
    )
  }
  if (!is.null(prior_model)) {
    arguments <- append(arguments, list(list("prior_model", prior_model)), after = 4L)
  }

  lines <- lapply(arguments, function(argument) {
    value <- deparse(argument[[2]], width.cutoff = 80L)
    value[[1]] <- paste0("  ", argument[[1]], " = ", value[[1]])
    value
  })
  for (i in seq_len(length(lines) - 1L)) {
    lines[[i]][[length(lines[[i]])]] <- paste0(lines[[i]][[length(lines[[i]])]], ",")
  }

  c("result <- analyze_theory(", unlist(lines, use.names = FALSE), ")")
}


.analyze_theory_compatibility_summary <- function(dyads, column, label) {
  if (!is.data.frame(dyads) || !column %in% names(dyads)) {
    return(sprintf("%s compatibility: unavailable", label))
  }

  values <- dyads[[column]]
  available <- !is.na(values)
  available_count <- sum(available)
  if (available_count == 0L) {
    return(sprintf("%s compatibility: unavailable (0/%d dyads)",
                   label, length(values)))
  }

  compatible_rate <- mean(values[available])
  sprintf("%s compatibility: %.1f%% (%d/%d available dyads)",
          label, 100 * compatible_rate, sum(values[available]), available_count)
}


.analyze_theory_identified_models_summary <- function(dyads, model_ids) {
  model_ids <- unique(as.character(model_ids))
  required <- c(
    "ego_id", "alter_id", "identified_ego", "identified_alter"
  )
  if (!is.data.frame(dyads) || !all(required %in% names(dyads)) ||
      length(model_ids) == 0L) {
    return("Identified models: unavailable")
  }

  ids <- c(as.character(dyads$ego_id), as.character(dyads$alter_id))
  values <- c(dyads$identified_ego, dyads$identified_alter)
  statuses <- vapply(model_ids, function(model_id) {
    observed <- unique(values[ids == model_id & !is.na(values)])
    if (length(observed) == 1L) observed[[1]] else NA
  }, logical(1))

  available_count <- sum(!is.na(statuses))
  if (available_count == 0L) {
    return(sprintf(
      "Identified models: unavailable (identification available for 0/%d models)",
      length(model_ids)
    ))
  }

  availability <- ""
  if (available_count < length(model_ids)) {
    availability <- sprintf(
      " (identification available for %d/%d models)",
      available_count, length(model_ids)
    )
  }
  sprintf(
    "Identified models: %d/%d%s",
    sum(statuses %in% TRUE), length(model_ids), availability
  )
}


.analyze_theory_value_key <- function(values) {
  values <- as.character(values)
  if (length(values) == 0L) {
    return("0:")
  }
  paste0(nchar(values), ":", values, collapse = "|")
}


.analyze_theory_format_mas <- function(mas) {
  if (length(mas) == 0L) {
    return("{}")
  }
  paste0("{", paste(mas, collapse = ", "), "}")
}


.analyze_theory_model_mas <- function(dyads) {
  if (!is.data.frame(dyads) ||
      !all(c("ego_id", "mas_ego") %in% names(dyads))) {
    return(NULL)
  }

  model_ids <- unique(as.character(dyads$ego_id))
  profiles <- lapply(model_ids, function(model_id) {
    row <- which(as.character(dyads$ego_id) == model_id)[[1]]
    mas <- dyads$mas_ego[[row]]
    if (is.null(mas)) {
      return(NULL)
    }

    normalized <- lapply(mas, function(set) {
      sort(unique(as.character(set)))
    })
    keys <- vapply(normalized, .analyze_theory_value_key, character(1))
    normalized <- normalized[!duplicated(keys)]
    names(normalized) <- keys[!duplicated(keys)]
    normalized
  })
  names(profiles) <- model_ids
  profiles
}


.analyze_theory_mas_summary <- function(dyads) {
  profiles <- .analyze_theory_model_mas(dyads)
  if (is.null(profiles) || length(profiles) == 0L) {
    return(c(
      "Most common MAS set: unavailable",
      "MAS set uniquely enabling most compatibility: unavailable"
    ))
  }

  available <- !vapply(profiles, is.null, logical(1))
  available_profiles <- profiles[available]
  labels <- character(0)
  all_keys <- character(0)
  for (profile in available_profiles) {
    keys <- names(profile)
    all_keys <- c(all_keys, keys)
    for (key in keys) {
      if (!key %in% names(labels)) {
        labels[[key]] <- .analyze_theory_format_mas(profile[[key]])
      }
    }
  }

  if (length(all_keys) == 0L) {
    return(c(
      "Most common MAS set: none",
      "MAS set uniquely enabling most compatibility: none (0 model dyads)"
    ))
  }

  frequencies <- table(all_keys)
  most_common_keys <- names(frequencies)[frequencies == max(frequencies)]
  most_common_keys <- most_common_keys[order(labels[most_common_keys])]
  most_common <- paste(unname(labels[most_common_keys]), collapse = "; ")
  most_common_count <- as.integer(max(frequencies))
  availability <- ""
  if (sum(available) < length(profiles)) {
    availability <- sprintf(
      "; MAS available for %d/%d models", sum(available), length(profiles)
    )
  }
  prevalence_line <- sprintf(
    "Most common MAS set: %s (%d/%d models%s)",
    most_common, most_common_count, length(profiles), availability
  )

  unique_pair_counts <- setNames(integer(length(labels)), names(labels))
  available_ids <- names(available_profiles)
  if (length(available_ids) >= 2L) {
    for (left in seq_len(length(available_ids) - 1L)) {
      for (right in seq.int(left + 1L, length(available_ids))) {
        shared <- intersect(
          names(available_profiles[[available_ids[[left]]]]),
          names(available_profiles[[available_ids[[right]]]])
        )
        if (length(shared) == 1L) {
          unique_pair_counts[[shared]] <- unique_pair_counts[[shared]] + 1L
        }
      }
    }
  }

  if (length(unique_pair_counts) == 0L || max(unique_pair_counts) == 0L) {
    contribution_line <-
      "MAS set uniquely enabling most compatibility: none (0 model dyads)"
  } else {
    top_keys <- names(unique_pair_counts)[
      unique_pair_counts == max(unique_pair_counts)
    ]
    top_keys <- top_keys[order(labels[top_keys])]
    contribution_line <- sprintf(
      "MAS set uniquely enabling most compatibility: %s (%d model dyads)",
      paste(unname(labels[top_keys]), collapse = "; "),
      max(unique_pair_counts)
    )
  }

  c(prevalence_line, contribution_line)
}


.analyze_theory_missing_component_summary <- function(dyads,
                                                       registry,
                                                       node_policy) {
  if (!identical(node_policy, "vary")) {
    return(character(0))
  }

  dyad_fields <- c(
    "ego_id", "alter_id", "identified_ego", "identified_alter",
    "identified_compatible", "identification_nodes_ego",
    "identification_nodes_alter"
  )
  if (!is.data.frame(dyads) || !all(dyad_fields %in% names(dyads)) ||
      !is.data.frame(registry) ||
      !all(c("comp_id", "type", "source") %in% names(registry))) {
    return("Missing component contribution to identified incompatibility: unavailable")
  }

  eligible <- !is.na(dyads$identified_compatible) &
    !dyads$identified_compatible &
    (dyads$identified_ego %in% TRUE) &
    (dyads$identified_alter %in% TRUE) &
    !vapply(dyads$identification_nodes_ego, is.null, logical(1)) &
    !vapply(dyads$identification_nodes_alter, is.null, logical(1))
  pairs <- dyads[eligible, , drop = FALSE]
  if (nrow(pairs) == 0L) {
    return(paste0(
      "Missing component contribution to identified incompatibility: ",
      "unavailable (no eligible model pairs)"
    ))
  }

  pair_keys <- mapply(function(ego, alter) {
    .analyze_theory_value_key(sort(c(ego, alter)))
  }, as.character(pairs$ego_id), as.character(pairs$alter_id),
  USE.NAMES = FALSE)
  pairs <- pairs[!duplicated(pair_keys), , drop = FALSE]

  nodes <- registry[registry$type == "node", , drop = FALSE]
  node_names <- as.character(nodes$source)
  missing_counts <- setNames(integer(length(node_names)), node_names)
  for (row in seq_len(nrow(pairs))) {
    ego_nodes <- as.character(pairs$identification_nodes_ego[[row]])
    alter_nodes <- as.character(pairs$identification_nodes_alter[[row]])
    missing <- union(
      setdiff(ego_nodes, alter_nodes),
      setdiff(alter_nodes, ego_nodes)
    )
    missing_counts[missing] <- missing_counts[missing] + 1L
  }

  if (length(missing_counts) == 0L || max(missing_counts) == 0L) {
    return(sprintf(
      paste0(
        "Missing component contribution to identified incompatibility: ",
        "none (no node-presence differences in %d eligible model pairs)"
      ),
      nrow(pairs)
    ))
  }

  top_names <- names(missing_counts)[missing_counts == max(missing_counts)]
  top_nodes <- nodes[match(top_names, as.character(nodes$source)), , drop = FALSE]
  top_labels <- paste0(top_nodes$source, " (", top_nodes$comp_id, ")")
  top_labels <- sort(top_labels)
  sprintf(
    paste0(
      "Missing component contributing most to identified incompatibility: ",
      "%s (missing in %d/%d eligible model pairs)"
    ),
    paste(top_labels, collapse = "; "),
    max(missing_counts),
    nrow(pairs)
  )
}

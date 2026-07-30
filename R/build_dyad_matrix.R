#' Build a dyadic compatibility matrix
#'
#' Sends a component registry and model states to the theoRy Python backend
#' to compute dyadic comparisons for every directed non-self model pair.
#'
#' @param registry A data frame (from \code{\link{build_component_registry}})
#'   or a path to a Parquet file containing the component registry.
#' @param states A list of state records or a data frame (from
#'   \code{\link{expand_model_states}}) with columns \code{model_id},
#'   \code{comp_id}, \code{status}, and optionally \code{timing}.
#'   Under sparse semantics, node components use \code{"present"} status
#'   and edge components use \code{"causal"}, \code{"unknown"}, or
#'   \code{"non-causal"}.  Missing node records mean absent.
#' @param mode Output mode: \code{"basic"} (structural only, default),
#'   \code{"full"} (structural + causal metrics),
#'   \code{"single-ref"} (dyads with the reference model as ego only),
#'   \code{"two-stage"} (top-K detailed comparisons with heatmap summary),
#'   \code{"symbolic"} (symbolic query class comparison).
#' @param reference_id Required when \code{mode = "single-ref"}. The model ID
#'   used as the ego model for pairwise comparisons.
#' @param top_k Required when \code{mode = "two-stage"}. Number of top
#'   similarity pairs to compute full causal metrics for. Defaults to 100.
#' @param exposure Optional name of the exposure (cause) variable for causal
#'   metrics. Must be a node in the registry. When omitted, defaults to the
#'   first node in the registry.
#' @param outcome Optional name of the outcome variable for causal metrics.
#'   Must be a node in the registry. When omitted, defaults to the last node
#'   in the registry. Both or neither of \code{exposure} and \code{outcome}
#'   must be provided.
#' @param causal_backend Causal-identification backend: \code{"auto"}
#'   (default) uses the native NetworkX implementation when it supports the
#'   model and falls back to the R stack when available. If a query requires
#'   the R stack but it is unavailable, \code{"auto"} returns unavailable
#'   causal fields rather than failing the full analysis. \code{"native"}
#'   never loads R; \code{"r"} requires the Dagitty/CausalEffect stack.
#' @param url Base URL of the theoRy Python backend API. Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return For \code{mode = "basic"}, \code{"full"}, or \code{"single-ref"}:
#'   a data frame with one row per directed non-self model pair.
#'   The returned object includes a \code{theory_context} attribute used by
#'   \code{\link{compute_delta_u}} to make follow-on Delta-U calls explicit.
#'   Columns for \code{"basic"}:
#'   \item{dyad_id}{Deterministic pair ID (\code{"M0001__M0002"})}
#'   \item{ego_id}{ID of the ego model}
#'   \item{alter_id}{ID of the alter model}
#'   \item{similarity_rate}{Similarity based on shared resolved claims over
#'     union of resolved claims. Node presence differences count once;
#'     inapplicable edges are ignored.}
#'   \item{timing_compatible}{Whether timing is compatible}
#'   \item{existence_conflict}{Whether there are existence conflicts}
#'   \item{repair_cost}{Number of structural differences}
#'
#'   Additional columns for \code{"full"}:
#'   \item{mas_ego}{Minimal adjustment sets for the ego model (list-column)}
#'   \item{mas_alter}{Minimal adjustment sets for the alter model (list-column)}
#'   \item{mas_compatible}{Whether adjustment sets are compatible}
#'   \item{identified_ego}{Whether the ego model's exposure-outcome effect is
#'     identified}
#'   \item{identified_alter}{Whether the alter model's exposure-outcome effect
#'     is identified}
#'   \item{identified_compatible}{Whether the exposure-outcome effect is
#'     identified in both models and the two models' relevant declared node
#'     sets are exactly equal after removing robust directed-path
#'     intermediates. The relevant set uses all declared present nodes
#'     (observed and latent); a node is a directed-path intermediate only
#'     when it lies on at least one directed exposure-outcome path in the
#'     declared directed graph (bidirected edges never qualify). For partial
#'     models, a node is removed only when it is an intermediate in every
#'     valid represented completion; incomplete completion coverage returns
#'     unavailable. Two non-identified models are not compatible.}
#'
#'   For \code{"two-stage"}: a list with components
#'   \code{heatmap_summary} and \code{detailed_comparisons} (data frame).
#'
#'   The returned object includes a \code{theory_context} attribute
#'   preserving registry, state, model IDs, and exposure/outcome metadata.
#'   \code{\link{compute_delta_u}} can recompute causal dyads from this context
#'   when \code{compatibility_metric} is \code{"mas_compatible"} or
#'   \code{"identified_compatible"}. \code{mode = "full"} dyads preserve
#'   both causal compatibility columns for use as Delta-U baselines.
#'
#' @details Self-dyads are not computed.  Both \code{ego -> alter} and
#'   \code{alter -> ego} are included as distinct rows.
#'   For \code{M} models, the result has \code{M * (M - 1)} rows.
#'   \code{mas_ego} and \code{mas_alter} are represented as list-columns
#'   using \code{I(list(...))}. An empty outer list means no adjustment sets
#'   are available, while a list containing \code{character(0)} represents a
#'   valid empty adjustment set.
#'
#'   Under sparse node semantics, a node-presence difference between two
#'   models counts as one structural difference.  Edges incident to absent
#'   nodes are inapplicable and are not compared.  Only edges applicable in
#'   both models contribute to edge-level similarity.  Shared \code{"non-causal"}
#'   resolutions reward agreement.
#'
#'   When \code{exposure} and \code{outcome} are omitted, the function
#'   attempts to resolve them from the \code{states} data frame attributes
#'   (\code{attr(states, "exposure")}, \code{attr(states, "outcome")}) and
#'   then from the \code{registry} attributes. Explicit arguments always
#'   take precedence. A warning is issued if the resolved exposure/outcome
#'   differs from the metadata found on the inputs.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' reg <- build_component_registry(c("X", "Y", "Z"), c(1, 2, 3))
#' states <- expand_model_states(reg, mode = "exhaustive",
#'   node_timing = c(X = 1, Y = 2, Z = 3))
#' dyads <- build_dyad_matrix(reg, states)
#' head(dyads)
#'
#' # Full mode with causal metrics
#' full <- build_dyad_matrix(reg, states, mode = "full")
#'
#' # Full mode with explicit exposure and outcome
#' full <- build_dyad_matrix(reg, states, mode = "full",
#'   exposure = "X", outcome = "Z")
#'
#' # Single-reference mode
#' single <- build_dyad_matrix(reg, states, mode = "single-ref",
#'   reference_id = "M0001")
#'
#' # Two-stage top-K detailed comparisons
#' two_stage <- build_dyad_matrix(reg, states, mode = "two-stage",
#'   top_k = 5)
#' }
#'
#' @export
build_dyad_matrix <- function(registry,
                               states,
                               mode = c("basic", "full", "single-ref", "two-stage", "symbolic"),
                               reference_id = NULL,
                                top_k = NULL,
                                exposure = NULL,
                                outcome = NULL,
                                causal_backend = c("auto", "native", "r"),
                                url = getOption("theoRy.engine_url",
                                                "http://localhost:8000")) {

  mode <- match.arg(mode)
  causal_backend <- match.arg(causal_backend)
  `%||%` <- function(x, y) if (is.null(x)) y else x

  if (xor(is.null(exposure), is.null(outcome))) {
    stop("Both or neither of exposure and outcome must be provided.",
         call. = FALSE)
  }

  if (identical(mode, "single-ref") && is.null(reference_id)) {
    stop("reference_id is required when mode = 'single-ref'", call. = FALSE)
  }

  if (identical(mode, "two-stage")) {
    if (is.null(top_k)) {
      top_k <- 100L
    }
    if (!is.numeric(top_k) || top_k <= 0) {
      stop("top_k must be positive when mode = 'two-stage'", call. = FALSE)
    }
  }

  if (is.data.frame(registry)) {
    registry_df <- registry
  } else if (is.character(registry) && length(registry) == 1 && file.exists(registry)) {
    registry_df <- arrow::read_parquet(registry)
  } else {
    stop("registry must be a data frame or a path to a Parquet file.")
  }

  registry_data <- lapply(seq_len(nrow(registry_df)), function(i) {
    row <- registry_df[i, ]
    entry <- list(
      comp_id = row$comp_id,
      type = row$type,
      source = row$source,
      description = row$description
    )
    if (is.na(row$target)) entry$target <- NULL else entry$target <- row$target
    if (is.na(row$direction)) entry$direction <- NULL else entry$direction <- row$direction
    if ("fixed_status" %in% names(row) && !is.null(row$fixed_status) && !is.na(row$fixed_status)) {
      entry$fixed_status <- row$fixed_status
    }
    entry
  })

  if (is.data.frame(states)) {
    state_list <- lapply(seq_len(nrow(states)), function(i) {
      row <- states[i, ]
      entry <- list(
        model_id = row$model_id,
        comp_id = row$comp_id,
        status = row$status
      )
      if (!is.null(row$timing) && !is.na(row$timing)) {
        entry$timing <- as.integer(row$timing)
      }
      entry
    })
  } else {
    state_list <- states
  }

  # Resolve exposure/outcome: explicit > state attrs > registry attrs
  if (is.null(exposure) && is.null(outcome)) {
    if (is.data.frame(states)) {
      st_exposure <- attr(states, "exposure") %||% NULL
      st_outcome <- attr(states, "outcome") %||% NULL
      if (!is.null(st_exposure) && !is.null(st_outcome)) {
        exposure <- st_exposure
        outcome <- st_outcome
      } else if (!is.null(st_exposure) || !is.null(st_outcome)) {
        warning("State exposure/outcome metadata is incomplete; ignoring it.",
                call. = FALSE)
      }
    }
    if (is.null(exposure)) {
      rg_exposure <- attr(registry_df, "exposure") %||% NULL
      rg_outcome <- attr(registry_df, "outcome") %||% NULL
      if (!is.null(rg_exposure) && !is.null(rg_outcome)) {
        exposure <- rg_exposure
        outcome <- rg_outcome
      } else if (!is.null(rg_exposure) || !is.null(rg_outcome)) {
        warning("Registry exposure/outcome metadata is incomplete; ignoring it.",
                call. = FALSE)
      }
    }
  } else {
    # Explicit args were provided; warn if metadata differs
    st_exposure <- if (is.data.frame(states)) attr(states, "exposure") else NULL
    st_outcome  <- if (is.data.frame(states)) attr(states, "outcome") else NULL
    rg_exposure <- attr(registry_df, "exposure")
    rg_outcome  <- attr(registry_df, "outcome")
    meta_exposure <- st_exposure %||% rg_exposure %||% NULL
    meta_outcome  <- st_outcome %||% rg_outcome %||% NULL
    if (!is.null(meta_exposure) && !is.null(meta_outcome) &&
        (!identical(meta_exposure, exposure) || !identical(meta_outcome, outcome))) {
      warning(
        "exposure/outcome (", exposure, "/", outcome,
        ") differ from metadata attributes (", meta_exposure, "/", meta_outcome,
        "). Using explicit values.", call. = FALSE
      )
    } else if (xor(is.null(meta_exposure), is.null(meta_outcome))) {
      warning("Input exposure/outcome metadata is incomplete. Using explicit values.",
              call. = FALSE)
    }
  }

  if (!is.null(exposure) && !is.null(outcome)) {
    node_names <- unique(registry_df$source[registry_df$type == "node"])
    if (!exposure %in% node_names) {
      stop("Exposure '", exposure, "' is not in the registry node list.",
           call. = FALSE)
    }
    if (!outcome %in% node_names) {
      stop("Outcome '", outcome, "' is not in the registry node list.",
           call. = FALSE)
    }
    if (identical(exposure, outcome)) {
      stop("Exposure and outcome must be distinct nodes.", call. = FALSE)
    }
  }

  model_ids <- unique(vapply(state_list, function(x) x$model_id, character(1), USE.NAMES = FALSE))
  model_ids <- sort(model_ids)

  if (identical(mode, "symbolic")) {
    sym_payload <- list(
      registry_data = registry_data,
      exposure = exposure,
      outcome = outcome,
      mode = "sampled",
      n_samples = 500L,
      signature_policy = "paper_v1"
    )
    req <- httr2::request(url) |>
      httr2::req_url_path("api/v1/symbolic/query-classes") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(sym_payload) |>
      httr2::req_error(is_error = function(resp) FALSE)
  } else {
    payload <- list(
      registry_data = registry_data,
      state_data = state_list,
      model_ids = I(model_ids),
      mode = mode,
      causal_backend = causal_backend
    )

    if (!is.null(reference_id)) {
      payload$reference_id <- reference_id
    }
    if (!is.null(top_k)) {
      payload$top_k <- as.integer(top_k)
    }
    if (!is.null(exposure) && !is.null(outcome)) {
      payload$exposure <- exposure
      payload$outcome <- outcome
    }

    req <- httr2::request(url) |>
      httr2::req_url_path("api/v1/dyad-matrix") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(payload) |>
      httr2::req_error(is_error = function(resp) FALSE)
  }

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Python backend not reachable at ", url, ". Start the server with start_theory_engine().", call. = FALSE)
    }
  )

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid backend response: expected JSON response body.", call. = FALSE)
    }
  )

  if (!is.list(body) || is.null(body$status)) {
    stop("Invalid backend response: missing status field.", call. = FALSE)
  }

  if (identical(body$status, "error")) {
    code <- if (is.null(body$code)) "UNKNOWN" else body$code
    message <- if (is.null(body$message)) "Unknown backend error" else body$message
    stop("Backend error [", code, "]: ", message, call. = FALSE)
  }

  if (!identical(body$status, "success") || is.null(body$data)) {
    stop("Invalid backend response: missing data.", call. = FALSE)
  }

  if (identical(mode, "symbolic")) {
    result <- body$data
    class(result) <- c("theory_symbolic_classes", "list")
    attr(result, "theory_context") <- list(
      registry_data = registry_data,
      state_data = state_list,
      model_ids = model_ids,
      mode = mode,
      exposure = exposure,
      outcome = outcome
    )
    return(result)
  }

  if (identical(mode, "two-stage")) {
    dc <- body$data$detailed_comparisons
    hs <- body$data$heatmap_summary

    if (!is.list(dc) || !is.list(hs)) {
      stop("Invalid backend response for two-stage: missing heatmap_summary or detailed_comparisons.", call. = FALSE)
    }

    dc_df <- .parse_dyads_to_df(dc, full = TRUE)

    result <- list(
      heatmap_summary = hs,
      detailed_comparisons = dc_df
    )
    attr(result, "theory_context") <- list(
      registry_data = registry_data,
      state_data = state_list,
      model_ids = model_ids,
      mode = mode,
      exposure = exposure,
      outcome = outcome
    )
    return(result)
  }

  dyads <- body$data$dyads

  if (is.null(dyads)) {
    stop("Invalid backend response: missing data.dyads.", call. = FALSE)
  }

  is_full <- identical(mode, "full")

  required_fields <- c("dyad_id", "ego_id", "alter_id", "similarity_rate",
                       "timing_compatible", "existence_conflict", "repair_cost")

  malformed <- vapply(dyads, function(d) {
    !is.list(d) || any(!required_fields %in% names(d))
  }, logical(1), USE.NAMES = FALSE)
  if (any(malformed)) {
    stop("Invalid backend response: malformed dyad records.", call. = FALSE)
  }

  result <- .parse_dyads_to_df(dyads, full = is_full)
  attr(result, "theory_context") <- list(
    registry_data = registry_data,
    state_data = state_list,
    model_ids = model_ids,
    mode = mode,
    exposure = exposure,
    outcome = outcome
  )
  result
}


.parse_dyads_to_df <- function(dyads, full = FALSE) {
  base_fields <- list(
    dyad_id = vapply(dyads, function(d) d$dyad_id, character(1), USE.NAMES = FALSE),
    ego_id = vapply(dyads, function(d) d$ego_id, character(1), USE.NAMES = FALSE),
    alter_id = vapply(dyads, function(d) d$alter_id, character(1), USE.NAMES = FALSE),
    similarity_rate = vapply(dyads, function(d) d$similarity_rate, numeric(1), USE.NAMES = FALSE),
    timing_compatible = vapply(dyads, function(d) d$timing_compatible, logical(1), USE.NAMES = FALSE),
    existence_conflict = vapply(dyads, function(d) d$existence_conflict, logical(1), USE.NAMES = FALSE),
    repair_cost = vapply(dyads, function(d) d$repair_cost, integer(1), USE.NAMES = FALSE)
  )

  if (!full) {
    return(data.frame(base_fields, stringsAsFactors = FALSE))
  }

  normalize_mas <- function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    lapply(x, function(set) {
      if (is.null(set) || length(set) == 0) {
        character(0)
      } else {
        as.character(unlist(set, use.names = FALSE))
      }
    })
  }

  mas_ego_col <- lapply(dyads, function(d) {
    normalize_mas(d$mas_ego)
  })
  mas_alter_col <- lapply(dyads, function(d) {
    normalize_mas(d$mas_alter)
  })

  data.frame(
    base_fields,
    mas_ego = I(mas_ego_col),
    mas_alter = I(mas_alter_col),
    mas_compatible = vapply(dyads, function(d) {
      if (is.null(d$mas_compatible)) NA else d$mas_compatible
    }, logical(1), USE.NAMES = FALSE),
    identified_ego = vapply(dyads, function(d) {
      if (is.null(d$identified_ego)) NA else d$identified_ego
    }, logical(1), USE.NAMES = FALSE),
    identified_alter = vapply(dyads, function(d) {
      if (is.null(d$identified_alter)) NA else d$identified_alter
    }, logical(1), USE.NAMES = FALSE),
    identified_compatible = vapply(dyads, function(d) {
      if (is.null(d$identified_compatible)) NA else d$identified_compatible
    }, logical(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

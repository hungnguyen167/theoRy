#' Expand a component registry into model-state records
#'
#' Takes a component registry (from \code{\link{build_component_registry}}
#' or a Parquet file) and generates model-state records using exhaustive
#' or sampled expansion.  Optional \code{seed_claims} inject user-specified
#' theories into the multiverse.
#'
#' @param registry A registry data frame (as returned by
#'   \code{build_component_registry()}), or a path to a Parquet file.
#' @param mode Expansion mode: \code{"sampled"} (default, random sample)
#'   or \code{"exhaustive"} (all valid edge-status combinations).
#' @param seed_claims Optional data frame or list of state claims with
#'   \code{model_id}, \code{comp_id}, \code{status}, and optionally
#'   \code{timing}.  When provided, the engine searches for each seeded
#'   model in the generated multiverse.  Found models are promoted to the
#'   top and flagged with \code{seeded = TRUE}.  Models not found in the
#'   multiverse are appended at the top with \code{seeded = TRUE}.
#'   Non-seeded models are renumbered \code{M0001}, \code{M0002}, \ldots
#'   after the seeded block.
#'
#'   Seed claims use sparse semantics: node statuses may be \code{"present"}
#'   or \code{"absent"}; edge statuses may be \code{"causal"}, \code{"unknown"},
#'   or \code{"non-causal"}; bidirected-edge claims use \code{"present"} or
#'   \code{"absent"}. Edge claims infer endpoint node presence.
#'   Omitted nodes default to absent; omitted edges among present nodes
#'   default to unknown.
#' @param node_timing Optional named integer vector mapping node names to
#'   chronological positions, e.g. \code{c(SolarRad = 1, Temp = 2)}.
#'   Used for temporal validation in exhaustive / sampled modes.
#' @param timing_options Optional named list of allowed integer positions per
#'   node. When supplied, each generated model selects one allowed position
#'   for every present node. This overrides \code{node_timing} for named nodes.
#' @param optional_nodes Optional names of nodes that may be absent. When
#'   supplied, this takes precedence over \code{node_policy}; all other nodes
#'   remain present.
#' @param max_models Safety cap for exhaustive mode (default 10,000).
#'   Expansion fails if the projected model count exceeds this.
#' @param n_models Number of models to sample in \code{"sampled"} mode.
#' @param seed Random seed for reproducible sampling.
#' @param edge_statuses Character vector of edge statuses to enumerate
#'   or sample over in exhaustive and sampled modes.  Defaults to
#'   \code{c("causal", "unknown", "non-causal")}.  Pass
#'   \code{c("causal", "unknown")} for binary (old) behavior.
#' @param bidirected_statuses Character vector of possible states for
#'   \code{<->} components. Defaults to \code{c("present", "absent")}.
#' @param node_policy Controls node-subset generation:
#'   \code{"all-present"} (default, backward-compatible) includes all
#'   registry nodes in every model; \code{"vary"} enumerates over
#'   non-empty node subsets, producing models with variable node scope.
#' @param exposure Optional name of the exposure variable. If omitted,
#'   registry exposure metadata is inherited when available.
#' @param outcome Optional name of the outcome variable. If omitted,
#'   registry outcome metadata is inherited when available. Both or neither
#'   of \code{exposure} and \code{outcome} must be given. When supplied with
#'   \code{node_policy = "vary"}, generated models must include both nodes.
#' @param url Base URL of the theoRy Python backend.
#'   Defaults to \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#' @param allow_large Whether to proceed with an exhaustive expansion larger
#'   than the backend warning threshold but still within \code{max_models}.
#'
#' @return A data frame with columns: \code{model_id}, \code{comp_id},
#'   \code{status}, \code{timing}, \code{seeded} (logical).  Under sparse
#'   semantics, node components use \code{"present"} status, directed edges
#'   use \code{"causal"}, \code{"unknown"}, or \code{"non-causal"}, and
#'   bidirected edges use \code{"present"} or \code{"absent"}.
#'   Only present nodes and applicable edges are emitted.  The returned
#'   data frame also carries a \code{seeded_model_ids} attribute (character
#'   vector of model IDs flagged as seeded, empty when no seed claims are
#'   provided) and optional \code{exposure}/\code{outcome} attributes
#'   forwarded to downstream functions like \code{\link{build_dyad_matrix}}.
#'   A \code{pruning_report} attribute records timing assignments excluded by
#'   required paths or temporal constraints.
#'
#' @details
#' \describe{
#'   \item{sampled}{Generate a random sample of valid model states from
#'     the edge-status space.  \code{n_models} controls the sample size.}
#'   \item{exhaustive}{Enumerate all valid combinations of edge statuses
#'     (by default \code{"causal"}, \code{"unknown"}, \code{"non-causal"})
#'     under temporal and DAG constraints.
#'     Only \code{"causal"} edges are subject to temporal/DAG validation.}
#' }
#'
#' Node components use presence semantics (\code{"present"} / absent) rather
#' than causal-status semantics.  An edge is applicable only when both
#' endpoint nodes are present in the model.  Inapplicable edges are not
#' emitted as state records.
#'
#' Registry components with \code{fixed_status = "causal"} are emitted as
#' causal in every generated model and in every normalized seeded model.
#' Fixed edges are not enumerated as mutable candidates; their status is
#' immutable.  A seed that explicitly sets a fixed edge to \code{unknown}
#' or \code{non-causal} is rejected.  Omitted fixed edges in a seed are
#' normalized to \code{causal}.
#'
#' When \code{seed_claims} is provided alongside either mode, the engine
#' first generates the multiverse, then searches for each seed model by
#' comparing sparse semantic vectors (present node set + applicable edge
#' statuses).  Matched models are renamed to the seed's \code{model_id}
#' and flagged \code{seeded = TRUE}.  Unmatched seed models are appended
#' at the top.  All non-seeded models are renumbered sequentially after
#' the seeded block.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' reg <- build_component_registry(c("X", "Y", "Z"), c(1, 2, 3),
#'   exposure = "X", outcome = "Z")
#'
#' # Exhaustive tri-state (default): causal, unknown, non-causal
#' states <- expand_model_states(reg, mode = "exhaustive",
#'   node_timing = c(X = 1, Y = 2, Z = 3))
#' head(states)
#'
#' # Binary (old behavior): causal, unknown only
#' states2 <- expand_model_states(reg, mode = "exhaustive",
#'   node_timing = c(X = 1, Y = 2, Z = 3),
#'   edge_statuses = c("causal", "unknown"))
#' head(states2)
#'
#' # Sampled with a user theory injected as M0001
#' # Edge claims infer that X, Y are present
#' my_claims <- data.frame(
#'   model_id = "M0001",
#'   comp_id  = reg$comp_id[reg$type == "edge"],
#'   status   = "causal"
#' )
#' states3 <- expand_model_states(reg, mode = "sampled",
#'   n_models = 200, seed_claims = my_claims)
#' attr(states3, "seeded_model_ids")  # "M0001"
#' head(subset(states3, seeded))
#' }
#'
#' @export
expand_model_states <- function(registry,
                                   mode = c("sampled", "exhaustive"),
                                   seed_claims = NULL,
                                   node_timing = NULL,
                                   timing_options = NULL,
                                   optional_nodes = NULL,
                                   max_models = 10000L,
                                   n_models = NULL,
                                   seed = NULL,
                                   edge_statuses = c("causal", "unknown", "non-causal"),
                                   bidirected_statuses = c("present", "absent"),
                                   node_policy = c("all-present", "vary"),
                                   exposure = NULL,
                                   outcome = NULL,
                                   allow_large = FALSE,
                                   url = getOption("theoRy.engine_url",
                                                   "http://localhost:8000")) {
  mode <- match.arg(mode)
  node_policy <- match.arg(node_policy)
  `%||%` <- function(x, y) if (is.null(x)) y else x

  if (xor(is.null(exposure), is.null(outcome))) {
    stop("Both or neither of exposure and outcome must be provided.",
         call. = FALSE)
  }

  if (is.character(registry) && length(registry) == 1 && file.exists(registry)) {
    registry_df <- arrow::read_parquet(registry)
  } else if (is.data.frame(registry)) {
    registry_df <- registry
  } else {
    stop("registry must be a data frame or a path to a Parquet file.")
  }

  if (is.null(node_timing) && !is.null(attr(registry_df, "node_timing"))) {
    node_timing <- attr(registry_df, "node_timing")
  }
  if (is.null(timing_options) && !is.null(attr(registry_df, "timing_options"))) {
    timing_options <- attr(registry_df, "timing_options")
  }
  if (is.null(optional_nodes) && !is.null(attr(registry_df, "optional_nodes"))) {
    optional_nodes <- attr(registry_df, "optional_nodes")
  }

  rg_exposure <- attr(registry_df, "exposure") %||% NULL
  rg_outcome <- attr(registry_df, "outcome") %||% NULL
  if (is.null(exposure) && is.null(outcome)) {
    if (!is.null(rg_exposure) && !is.null(rg_outcome)) {
      exposure <- rg_exposure
      outcome <- rg_outcome
    } else if (!is.null(rg_exposure) || !is.null(rg_outcome)) {
      warning("Registry exposure/outcome metadata is incomplete; ignoring it.",
              call. = FALSE)
    }
  } else if (!is.null(rg_exposure) && !is.null(rg_outcome) &&
             (!identical(rg_exposure, exposure) || !identical(rg_outcome, outcome))) {
    warning(
      "exposure/outcome (", exposure, "/", outcome,
      ") differ from registry metadata (", rg_exposure, "/", rg_outcome,
      "). Using explicit values.", call. = FALSE
    )
  }

  if (is.null(exposure) || is.null(outcome)) {
    stop("exposure and outcome are required. Supply them or use a registry ",
         "created by build_component_registry().", call. = FALSE)
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

  payload <- list(
    registry_data = registry_data,
    mode = mode,
    max_models = as.integer(max_models)
  )

  if (!is.null(seed_claims)) {
    payload$seed_claims <- seed_claims_to_records(seed_claims)
  }

  if (!is.null(node_timing)) {
    known_timing <- node_timing[!is.na(node_timing)]
    if (length(known_timing)) {
      payload$node_timing <- as.list(as.integer(known_timing))
      names(payload$node_timing) <- names(known_timing)
    }
  }

  if (!is.null(timing_options)) {
    if (!is.list(timing_options) || is.null(names(timing_options))) {
      stop("timing_options must be a named list.", call. = FALSE)
    }
    payload$timing_options <- lapply(timing_options, function(values) {
      unname(as.list(as.integer(values)))
    })
  }
  if (!is.null(optional_nodes)) {
    payload$optional_nodes <- as.list(as.character(optional_nodes))
  }

  if (!is.null(n_models)) payload$n_models <- as.integer(n_models)
  if (!is.null(seed)) payload$seed <- as.integer(seed)
  payload$edge_statuses <- as.list(edge_statuses)
  payload$bidirected_statuses <- as.list(bidirected_statuses)
  payload$node_policy <- node_policy
  payload$allow_large <- isTRUE(allow_large)
  if (!is.null(exposure) && !is.null(outcome)) {
    payload$exposure <- exposure
    payload$outcome <- outcome
  }

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_path("api/v1/model-states") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(payload) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      stop("Python backend not reachable at ", url,
           ". Start the server with start_theory_engine().", call. = FALSE)
    }
  )

  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid backend response: expected JSON.", call. = FALSE)
    }
  )

  if (identical(body$status, "error")) {
    code <- if (is.null(body$code)) "UNKNOWN" else body$code
    msg  <- if (is.null(body$message)) "Unknown backend error" else body$message
    stop("Backend error [", code, "]: ", msg, call. = FALSE)
  }

  if (!identical(body$status, "success") ||
      is.null(body$data) ||
      is.null(body$data$state_data)) {
    stop("Invalid backend response: missing state_data.", call. = FALSE)
  }

  df <- records_to_df(body$data$state_data,
                       col_types = c(timing = "integer",
                                     seeded = "logical"))

  seeded_ids <- body$data$seeded_model_ids
  if (!is.null(seeded_ids)) {
    attr(df, "seeded_model_ids") <- unlist(seeded_ids)
  } else {
    attr(df, "seeded_model_ids") <- character(0)
  }

  if (!is.null(exposure) && !is.null(outcome)) {
    attr(df, "exposure") <- exposure
    attr(df, "outcome") <- outcome
  }
  attr(df, "pruning_report") <- body$data$pruning_report %||% list()

  df
}


seed_claims_to_records <- function(seed_claims) {
  required <- c("model_id", "comp_id", "status")

  if (is.data.frame(seed_claims)) {
    missing_cols <- setdiff(required, names(seed_claims))
    if (length(missing_cols) > 0) {
      stop("seed_claims is missing required column(s): ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    return(lapply(seq_len(nrow(seed_claims)), function(i) {
      row <- seed_claims[i, , drop = FALSE]
      entry <- list(
        model_id = as.character(row$model_id),
        comp_id = as.character(row$comp_id),
        status = as.character(row$status)
      )
      if ("timing" %in% names(row) && !is.na(row$timing)) {
        entry$timing <- as.integer(row$timing)
      }
      entry
    }))
  }

  if (!is.list(seed_claims)) {
    stop("seed_claims must be a data frame or list of claim records.",
         call. = FALSE)
  }

  lapply(seed_claims, function(cl) {
    if (!is.list(cl)) {
      stop("Each seed_claims entry must be a list-like claim record.",
           call. = FALSE)
    }
    missing_fields <- setdiff(required, names(cl))
    if (length(missing_fields) > 0) {
      stop("seed_claims entry is missing required field(s): ",
           paste(missing_fields, collapse = ", "), call. = FALSE)
    }
    entry <- list(
      model_id = cl$model_id,
      comp_id = cl$comp_id,
      status = cl$status
    )
    if (!is.null(cl$timing) && !is.na(cl$timing)) {
      entry$timing <- as.integer(cl$timing)
    }
    entry
  })
}

#' Expand a component registry into model-state records
#'
#' Takes a component registry (from \code{\link{build_component_registry}}
#' or a Parquet file) and generates model-state records using seeded,
#' exhaustive, or sampled expansion.
#'
#' @param registry A registry data frame (as returned by
#'   \code{build_component_registry()}), or a path to a Parquet file.
#' @param mode Expansion mode: \code{"seeded"} (default, requires
#'   \code{seed_claims}), \code{"exhaustive"} (all valid edge-status
#'   combinations), or \code{"sampled"} (random sample, requires
#'   \code{n_models}).
#' @param seed_claims A data frame or list of state claims with
#'   \code{model_id}, \code{comp_id}, \code{status}, and optionally
#'   \code{timing}.  Required for \code{mode = "seeded"}.
#' @param node_timing Optional named integer vector mapping node names to
#'   chronological positions, e.g. \code{c(SolarRad = 1, Temp = 2)}.
#'   Used for temporal validation in exhaustive / sampled modes.
#' @param max_models Safety cap for exhaustive mode (default 10,000).
#'   Expansion fails if the projected model count exceeds this.
#' @param n_models Number of models to sample in \code{"sampled"} mode.
#' @param seed Random seed for reproducible sampling.
#' @param edge_statuses Character vector of edge statuses to enumerate
#'   or sample over in exhaustive and sampled modes.  Defaults to
#'   \code{c("causal", "unknown", "non-causal")}.  Pass
#'   \code{c("causal", "unknown")} for binary (old) behavior.
#' @param exposure Optional name of the exposure variable. If omitted,
#'   registry exposure metadata is inherited when available.
#' @param outcome Optional name of the outcome variable. If omitted,
#'   registry outcome metadata is inherited when available. Both or neither
#'   of \code{exposure} and \code{outcome} must be given.
#' @param url Base URL of the theoRy Python backend.
#'   Defaults to \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return A data frame with columns: \code{model_id}, \code{comp_id},
#'   \code{status}, \code{timing}. The returned data frame may also carry
#'   \code{exposure} and \code{outcome} attributes that are forwarded to
#'   downstream functions like \code{\link{build_dyad_matrix}}.
#'
#' @details
#' \describe{
#'   \item{seeded}{Fill user-provided model claims; default unspecified
#'     components to \code{"unknown"}.}
#'   \item{exhaustive}{Enumerate all valid combinations of edge statuses
#'     (by default \code{"causal"}, \code{"unknown"}, \code{"non-causal"})
#'     under temporal and DAG constraints.
#'     Only \code{"causal"} edges are subject to temporal/DAG validation.}
#'   \item{sampled}{Generate a random sample of valid model states from
#'     the same edge-status space.}
#' }
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' reg <- build_component_registry(c("X", "Y", "Z"), c(1, 2, 3))
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
#' }
#'
#' @export
expand_model_states <- function(registry,
                                 mode = c("seeded", "exhaustive", "sampled"),
                                 seed_claims = NULL,
                                 node_timing = NULL,
                                 max_models = 10000L,
                                 n_models = NULL,
                                 seed = NULL,
                                 edge_statuses = c("causal", "unknown", "non-causal"),
                                 exposure = NULL,
                                 outcome = NULL,
                                 url = getOption("theoRy.engine_url",
                                                 "http://localhost:8000")) {
  mode <- match.arg(mode)
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
    payload$node_timing <- as.list(as.integer(node_timing))
    names(payload$node_timing) <- names(node_timing)
  }

  if (!is.null(n_models)) payload$n_models <- as.integer(n_models)
  if (!is.null(seed)) payload$seed <- as.integer(seed)
  payload$edge_statuses <- as.list(edge_statuses)

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
                       col_types = c(timing = "integer"))

  if (!is.null(exposure) && !is.null(outcome)) {
    attr(df, "exposure") <- exposure
    attr(df, "outcome") <- outcome
  }

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

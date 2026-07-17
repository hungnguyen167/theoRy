#' Build a component registry from nodes and timing
#'
#' Provides a compact list of variable names, optional timing values, and
#' optional descriptions.  The Python backend generates the full component
#' registry containing node components and all admissible directed (and
#' optionally bidirectional) edge components under temporal constraints.
#'
#' @param nodes A character vector of variable names, or a data frame with
#'   columns \code{name}, \code{timing} (integer), and optionally
#'   \code{description}.
#' @param timing Integer vector of chronological positions, parallel to
#'   \code{nodes}.  Ignored when \code{nodes} is a data frame.
#' @param descriptions Optional character vector of human-readable
#'   descriptions, parallel to \code{nodes}.  Ignored when \code{nodes} is
#'   a data frame.
#' @param respect_timing When \code{TRUE} (default), only generate directed
#'   edges where \code{timing(source) < timing(target)}.
#' @param include_bidirectional When \code{TRUE}, also generate \verb{<->}
#'   bidirectional edge components for unordered node pairs.
#' @param constraints Optional data frame or list of edge constraints with
#'   columns \code{source}, \code{target}, \code{direction}, and
#'   \code{rule} (\code{"allow"}, \code{"forbid"}, \code{"require"}).
#' @param exposure Optional name of the exposure (cause) variable. Must be
#'   a node name in \code{nodes} if provided.
#' @param outcome Optional name of the outcome variable. Must be a node name
#'   in \code{nodes} if provided. Both or neither of \code{exposure} and
#'   \code{outcome} must be given.
#' @param url Base URL of the theoRy Python backend.  Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#' @param output_path If supplied, write the generated registry to this
#'   Parquet file path in addition to returning it as a data frame.
#'
#' @return A data frame with columns: \code{comp_id}, \code{type},
#'   \code{source}, \code{target}, \code{direction}, \code{description}.
#'   Additionally, \code{fixed_status} column (\code{"causal"} or \code{NA})
#'   marks components whose status is immutable across all models; currently
#'   only set when \code{exposure} and \code{outcome} are provided with no
#'   timing, in which case \code{exposure -> outcome} is fixed as causal.
#'   The returned data frame also has optional \code{exposure} and
#'   \code{outcome} attributes that are forwarded to downstream functions.
#'
#' @details
#' The builder assigns deterministic \code{C{NNNN}} component IDs.
#' Nodes are assigned first (sorted by name), followed by directed edges
#' (sorted by source, target), then bidirectional edges if requested.
#'
#' When \code{exposure} and \code{outcome} are provided, they are attached
#' as attributes on the returned data frame. These are used by
#' \code{\link{build_dyad_matrix}} when its own \code{exposure}/\code{outcome}
#' arguments are omitted.
#'
#' When all node timings are missing and both \code{exposure} and
#' \code{outcome} are supplied, the registry enforces an implicit
#' exposure-before-outcome ordering: the \code{exposure -> outcome} directed
#' edge is created and marked with \code{fixed_status = "causal"} (immutable
#' in every model), while the reverse \code{outcome -> exposure} and
#' bidirectional \code{exposure <-> outcome} candidates are excluded. No
#' synthetic timestamps are assigned to any node.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#'
#' reg <- build_component_registry(
#'   nodes = c("SolarRad", "Temp", "Precip"),
#'   timing = c(1, 2, 3)
#' )
#' head(reg)
#' }
#'
#' @export
build_component_registry <- function(nodes,
                                      timing = NULL,
                                      descriptions = NULL,
                                      respect_timing = TRUE,
                                      include_bidirectional = FALSE,
                                      constraints = NULL,
                                      exposure = NULL,
                                      outcome = NULL,
                                      url = getOption("theoRy.engine_url",
                                                      "http://localhost:8000"),
                                      output_path = NULL) {
  if (is.data.frame(nodes)) {
    nms <- nodes$name
    tmg <- if ("timing" %in% names(nodes)) nodes$timing else NULL
    desc <- if ("description" %in% names(nodes)) nodes$description else NULL
  } else {
    nms <- nodes
    tmg <- timing
    desc <- descriptions
  }

  if (is.null(nms) || length(nms) == 0) {
    stop("At least one node is required.")
  }

  if (xor(is.null(exposure), is.null(outcome))) {
    stop("Both or neither of exposure and outcome must be provided.",
         call. = FALSE)
  }
  if (!is.null(exposure)) {
    if (!exposure %in% nms) {
      stop("Exposure '", exposure, "' is not in the node list.", call. = FALSE)
    }
    if (!outcome %in% nms) {
      stop("Outcome '", outcome, "' is not in the node list.", call. = FALSE)
    }
    if (identical(exposure, outcome)) {
      stop("Exposure and outcome must be distinct nodes.", call. = FALSE)
    }
  }

  node_specs <- lapply(seq_along(nms), function(i) {
    entry <- list(name = nms[i])
    if (!is.null(tmg) && !is.na(tmg[i])) {
      entry$timing <- as.integer(tmg[i])
    } else {
      entry$timing <- NULL
    }
    if (!is.null(desc) && !is.na(desc[i])) {
      entry$description <- desc[i]
    } else {
      entry$description <- NULL
    }
    entry
  })

  payload <- list(
    nodes = node_specs,
    respect_timing = respect_timing,
    include_bidirectional = include_bidirectional
  )

  if (!is.null(exposure) && !is.null(outcome)) {
    payload$exposure <- exposure
    payload$outcome <- outcome
  }

  if (!is.null(constraints)) {
    if (is.data.frame(constraints)) {
      constraints <- lapply(seq_len(nrow(constraints)), function(i) {
        as.list(constraints[i, ])
      })
    }
    payload$constraints <- constraints
  }

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_path("api/v1/component-registry") |>
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
      is.null(body$data$registry_data)) {
    stop("Invalid backend response: missing registry_data.", call. = FALSE)
  }

  df <- records_to_df(body$data$registry_data,
                       col_types = c(target = "character",
                                     direction = "character",
                                     fixed_status = "character"))

  # attach node timing metadata so downstream functions can use it
  if (!is.null(tmg)) {
    named_timing <- stats::setNames(as.integer(tmg), nms)
  } else {
    named_timing <- NULL
  }
  if (!is.null(named_timing)) {
    attr(df, "node_timing") <- named_timing
  }
  if (!is.null(exposure) && !is.null(outcome)) {
    attr(df, "exposure") <- exposure
    attr(df, "outcome") <- outcome
  }
  rownames(df) <- NULL

  if (!is.null(output_path)) {
    arrow::write_parquet(df, output_path)
  }

  df
}

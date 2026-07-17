#' Build a Symbolic Multiverse
#'
#' Constructs a symbolic multiverse either from a component registry
#' (\code{registry}) or directly from nodes and timing. When no timing is
#' supplied and both \code{exposure} and \code{outcome} are provided, an
#' implicit \code{exposure -> outcome} edge variable is created and fixed as
#' causal in the symbolic universe, enforcing the causal ordering constraint
#' in every symbolic query.
#'
#' @param nodes A data frame or list of nodes with name and optional timing columns.
#' @param timing An optional named list/vector of integer timing values.
#' @param registry An optional registry data frame (from build_component_registry).
#' @param exposure Name of the exposure variable.
#' @param outcome Name of the outcome variable.
#' @param preferred_model An optional DAG spec list with nodes, edges, exposure, outcome.
#' @param constraints Optional edge constraints list.
#' @param mode Symbolic mode: "full" or "sampled".
#' @param n_samples Number of samples for sampled mode.
#' @param url Base URL for the theory engine API.
#' @return A list with class "theory_symbolic_multiverse".
#' @export
build_symbolic_multiverse <- function(nodes = NULL,
                                      timing = NULL,
                                      registry = NULL,
                                      exposure = NULL,
                                      outcome = NULL,
                                      preferred_model = NULL,
                                      constraints = NULL,
                                      mode = c("full", "sampled"),
                                      n_samples = 5000L,
                                      url = getOption("theoRy.engine_url", "http://localhost:8000")) {
  mode <- match.arg(mode)

  body <- list()
  body$exposure <- exposure
  body$outcome <- outcome
  body$mode <- mode
  body$n_samples <- n_samples

  if (!is.null(nodes)) {
    if (is.data.frame(nodes)) {
      body$nodes <- lapply(seq_len(nrow(nodes)), function(i) {
        list(name = as.character(nodes[i, "name", drop = TRUE]),
             timing = if ("timing" %in% names(nodes)) nodes[i, "timing", drop = TRUE] else NULL)
      })
    } else {
      body$nodes <- nodes
    }
  }

  if (!is.null(registry)) {
    body$registry_data <- lapply(seq_len(nrow(registry)), function(i) {
      row <- registry[i, ]
      entry <- list(
        comp_id = row$comp_id,
        type = row$type,
        source = row$source,
        description = row$description
      )
      entry$target <- if (is.na(row$target)) NULL else row$target
      entry$direction <- if (is.na(row$direction)) NULL else row$direction
      if ("fixed_status" %in% names(row) && !is.null(row$fixed_status) && !is.na(row$fixed_status)) {
        entry$fixed_status <- row$fixed_status
      }
      entry
    })
  }

  if (!is.null(timing)) {
    if (is.null(body$nodes)) {
      body$nodes <- lapply(names(timing), function(nm) list(name = nm, timing = timing[[nm]]))
    }
  }

  if (!is.null(preferred_model)) {
    body$preferred_model <- preferred_model
  }

  if (!is.null(constraints)) {
    body$constraints <- constraints
  }

  resp <- httr2::request(paste0(url, "/api/v1/symbolic/universe")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)
  structure(result$data, class = "theory_symbolic_multiverse")
}

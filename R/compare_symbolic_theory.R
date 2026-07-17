#' Compare Theories Using Symbolic Mode
#'
#' @param universe A symbolic multiverse object from build_symbolic_multiverse.
#' @param theory_a First DAG spec: list with nodes, edges, exposure, outcome.
#' @param theory_b Optional second DAG spec for theory-vs-theory comparison.
#' @param mode Symbolic mode: "full" or "sampled".
#' @param url Base URL for the theory engine API.
#' @return A list with compatibility results.
#' @export
compare_symbolic_theory <- function(universe,
                                    theory_a,
                                    theory_b = NULL,
                                    mode = c("full", "sampled"),
                                    url = getOption("theoRy.engine_url", "http://localhost:8000")) {
  mode <- match.arg(mode)

  body <- list(
    nodes = universe$nodes,
    exposure = universe$exposure,
    outcome = universe$outcome,
    theory_a = theory_a,
    theory_b = theory_b,
    mode = mode
  )

  if (!is.null(universe$edge_variables)) {
    node_names <- unique(c(
      universe$nodes,
      vapply(universe$edge_variables, function(ev) ev$source, character(1)),
      vapply(universe$edge_variables, function(ev) ev$target, character(1))
    ))
    node_records <- lapply(seq_along(node_names), function(i) {
      list(
        comp_id = sprintf("N%04d", i),
        type = "node",
        source = node_names[[i]],
        target = NULL,
        direction = NULL,
        description = node_names[[i]]
      )
    })
    edge_records <- lapply(seq_along(universe$edge_variables), function(i) {
      ev <- universe$edge_variables[[i]]
      comp_id <- if (is.null(ev$comp_id)) sprintf("S%04d", i) else ev$comp_id
      list(
        comp_id = comp_id,
        type = "edge",
        source = ev$source,
        target = ev$target,
        direction = "->",
        description = paste(ev$source, "->", ev$target)
      )
    })
    body$registry_data <- c(node_records, edge_records)
    body$nodes <- NULL
  }

  resp <- httr2::request(paste0(url, "/api/v1/symbolic/compare")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)
  structure(result$data, class = "theory_symbolic_comparison")
}

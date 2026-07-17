#' Compute Symbolic Delta-U
#'
#' @param universe A symbolic multiverse object from build_symbolic_multiverse.
#' @param top_k Number of top components to return.
#' @param mode Symbolic mode: "full" or "sampled".
#' @param n_samples Number of samples for sampled mode.
#' @param signature_policy Signature policy: "paper_v1" or "minimal".
#' @param url Base URL for the theory engine API.
#' @return A list with class "theory_symbolic_delta_u".
#' @export
compute_symbolic_delta_u <- function(universe,
                                     top_k = 10L,
                                     mode = c("full", "sampled"),
                                     n_samples = 5000L,
                                     signature_policy = c("paper_v1", "minimal"),
                                     url = getOption("theoRy.engine_url", "http://localhost:8000")) {
  mode <- match.arg(mode)
  signature_policy <- match.arg(signature_policy)

  body <- list(
    nodes = universe$nodes,
    exposure = universe$exposure,
    outcome = universe$outcome,
    mode = mode,
    top_k = top_k,
    n_samples = n_samples,
    signature_policy = signature_policy
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

  resp <- httr2::request(paste0(url, "/api/v1/symbolic/delta-u")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  result <- httr2::resp_body_json(resp)
  structure(result$data, class = "theory_symbolic_delta_u")
}

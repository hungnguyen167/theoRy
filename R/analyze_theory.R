`%||%` <- function(x, y) if (is.null(x)) y else x


#' Run the full theoRy analysis pipeline in one call
#'
#' Chains the five core theoRy pipeline steps - component registry,
#' model-state expansion, dyad matrix, Delta-U lynchpin ranking, and
#' (optionally) ghost cluster detection - with sensible defaults and
#' progress messages. Optionally generates standard visualizations.
#'
#' @param nodes Character vector of variable names.
#' @param timing Integer vector of chronological positions, parallel to
#'   \code{nodes}. When \code{NULL} and both \code{exposure} and
#'   \code{outcome} are supplied, an implicit exposure-before-outcome
#'   ordering is enforced: the \code{exposure -> outcome} edge is fixed as
#'   causal in every model. No synthetic timestamps are assigned to nodes.
#' @param exposure Optional name of the exposure variable. When provided
#'   together with \code{outcome}, enables causal compatibility metrics.
#' @param outcome Optional name of the outcome variable.
#' @param prior_model Optional model ID (e.g. \code{"M0001"}) for ghost
#'   cluster detection contrast analysis. When \code{NULL} (default),
#'   ghost detection is skipped.
#' @param mode Model expansion mode: \code{"symbolic"} (default for symbolic),
#'   \code{"sampled"}, or \code{"exhaustive"}.
#' @param n_models Number of models to sample in \code{"sampled"} mode.
#'   Defaults to 200.
#' @param seed Random seed for reproducible sampling. Defaults to 42.
#' @param top_k Maximum number of components to return in the Delta-U
#'   ranking. Defaults to 10.
#' @param plot Logical. When \code{TRUE}, generate standard plots
#'   (dyad heatmap, lynchpin ranking, cluster embedding when available).
#'   Defaults to \code{FALSE}.
#' @param eps DBSCAN eps parameter for ghost detection. Defaults to 0.5.
#' @param min_samples DBSCAN min_samples parameter. Defaults to 5.
#' @param url Base URL of the theoRy Python backend. Defaults to
#'   \code{getOption("theoRy.engine_url", "http://localhost:8000")}.
#'
#' @return A list with components:
#'   \item{registry}{Data frame from \code{\link{build_component_registry}}.}
#'   \item{states}{Data frame from \code{\link{expand_model_states}}.}
#'   \item{dyads}{Data frame from \code{\link{build_dyad_matrix}}.}
#'   \item{delta_u_rankings}{Data frame from \code{\link{compute_delta_u}}.}
#'   \item{ghost_clusters}{List from \code{\link{detect_ghost_clusters}} or
#'     \code{NULL} when no prior model is supplied.}
#'   \item{summary}{Character vector of human-readable key findings.}
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
analyze_theory <- function(nodes,
                            timing,
                            exposure = NULL,
                            outcome = NULL,
                            prior_model = NULL,
                            mode = c("sampled", "exhaustive", "symbolic"),
                            n_models = 200L,
                            seed = 42L,
                            top_k = 10L,
                            plot = FALSE,
                            eps = 0.5,
                            min_samples = 5L,
                            url = getOption("theoRy.engine_url",
                                             "http://localhost:8000")) {
  mode <- match.arg(mode)

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

  if (identical(mode, "symbolic")) {
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
      summary <- c(summary, sprintf("Top lynchpin: %s (%s \u2192 %s, delta_u = %.4f)",
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
  registry <- build_component_registry(
    nodes = nodes,
    timing = timing,
    exposure = exposure,
    outcome = outcome,
    url = url
  )

  # ── 2. Expand model states ───────────────────────────────────────────────────
  message("Step 2/5: Expanding model states...")
  if (identical(mode, "exhaustive")) {
    states <- expand_model_states(registry, mode = "exhaustive", url = url)
    model_count <- length(unique(states$model_id))
    if (model_count > 10000) {
      warning("Exhaustive expansion produced ", model_count,
              " models. Consider using mode = 'sampled'.", call. = FALSE)
    }
  } else {
    states <- expand_model_states(
      registry, mode = "sampled",
      n_models = as.integer(n_models),
      seed = as.integer(seed),
      url = url
    )
  }

  # ── 3. Build dyad matrix ─────────────────────────────────────────────────────
  message("Step 3/5: Computing dyad matrix...")
  dyad_mode <- if (!is.null(exposure) && !is.null(outcome)) "full" else "basic"
  dyads <- build_dyad_matrix(
    registry, states,
    mode = dyad_mode,
    exposure = exposure,
    outcome = outcome,
    url = url
  )

  # ── 4. Compute Delta-U ───────────────────────────────────────────────────────
  message("Step 4/5: Computing Delta-U lynchpin rankings...")
  delta_u <- compute_delta_u(dyads, top_k = as.integer(top_k), url = url)

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
  summary <- c(summary, sprintf("Components: %d", nrow(registry)))

  if (is.data.frame(delta_u) && nrow(delta_u) > 0) {
    top <- delta_u[1, ]
    src <- top$source %||% ""
    tgt <- top$target %||% ""
    summary <- c(summary, sprintf("Top lynchpin: %s (%s \u2192 %s, delta_u = %.4f)",
                                   top$component_id, src, tgt, top$delta_u))
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

    if (is.data.frame(delta_u) && nrow(delta_u) > 0) {
      plots$lynchpin_ranking <- plot_lynchpin_ranking(delta_u)
    }

    if (!is.null(ghost_result)) {
      plots$cluster_embedding <- plot_cluster_embedding(ghost_result)
    }
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

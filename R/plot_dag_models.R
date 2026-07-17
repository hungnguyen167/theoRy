`%||%` <- function(x, y) if (is.null(x)) y else x


#' Plot DAG models from a component registry and model states
#'
#' Creates one DAG plot per model, showing causal edges as solid arrows and
#' unknown-status edges as dashed gray arrows. Non-causal edges are omitted.
#' Node positions are determined by timing values from the registry
#' \code{node_timing} attribute.
#'
#' @param registry A data frame returned by \code{\link{build_component_registry}}
#'   with columns \code{comp_id}, \code{type}, \code{source}, \code{target},
#'   \code{direction}. The \code{node_timing} attribute (named integer vector)
#'   is used to position nodes; when absent, timing is inferred from node names
#'   matching \code{X\\d+} or defaults to a flat layout.
#' @param states A data frame returned by \code{\link{expand_model_states}}
#'   with columns \code{model_id}, \code{comp_id}, \code{status}.
#' @param model_ids Optional character vector of model IDs to plot. When
#'   \code{NULL}, the first 6 unique model IDs are selected (with a message).
#' @param show_mas Logical. When \code{TRUE}, annotate each plot with the
#'   minimal adjustment set for the exposure-outcome effect. Requires
#'   \code{exposure} and \code{outcome} attributes on \code{registry}.
#'   Defaults to \code{FALSE}.
#' @param save_path Optional directory path. When supplied, each plot is
#'   saved as a PNG file named \code{model_{model_id}.png} using
#'   \code{ragg::agg_png()} at 2400x1200 pixels, resolution 360.
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list of \code{ggplot} objects, one per model. Names are
#'   the model IDs.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' reg <- build_component_registry(c("X", "Y", "Z"),
#'   timing = c(1, 2, 3), exposure = "X", outcome = "Y")
#' states <- expand_model_states(reg, mode = "exhaustive",
#'   edge_statuses = c("causal", "unknown"))
#' plots <- plot_dag_models(reg, states, model_ids = c("M0001", "M0002"))
#' for (p in plots) print(p)
#' }
#'
#' @export
plot_dag_models <- function(registry,
                             states,
                             model_ids = NULL,
                             show_mas = FALSE,
                             save_path = NULL,
                             ...) {
  if (!is.data.frame(registry)) {
    stop("registry must be a data frame from build_component_registry().",
         call. = FALSE)
  }
  if (!is.data.frame(states)) {
    stop("states must be a data frame from expand_model_states().",
         call. = FALSE)
  }

  reg_required <- c("comp_id", "type", "source", "target")
  reg_missing <- setdiff(reg_required, names(registry))
  if (length(reg_missing) > 0) {
    stop("registry is missing column(s): ",
         paste(reg_missing, collapse = ", "), call. = FALSE)
  }

  state_required <- c("model_id", "comp_id", "status")
  state_missing <- setdiff(state_required, names(states))
  if (length(state_missing) > 0) {
    stop("states is missing column(s): ",
         paste(state_missing, collapse = ", "), call. = FALSE)
  }

  # Collect unique model IDs from states
  available_ids <- unique(as.character(states$model_id))
  if (length(available_ids) == 0) {
    stop("states has no model IDs.", call. = FALSE)
  }

  if (is.null(model_ids)) {
    model_ids <- available_ids[seq_len(min(6, length(available_ids)))]
    if (length(available_ids) > 6) {
      message("Plotting the first 6 models. Pass model_ids to select others.")
    }
  } else {
    model_ids <- as.character(model_ids)
    missing_ids <- setdiff(model_ids, available_ids)
    if (length(missing_ids) > 0) {
      stop("Model(s) not found in state data: ",
           paste(missing_ids, collapse = ", "), call. = FALSE)
    }
  }

  # Extract node names and timing
  node_names <- unique(as.character(registry$source[registry$type == "node"]))
  node_timing <- .resolve_node_timing(registry, node_names)

  exposure <- attr(registry, "exposure") %||% NULL
  outcome <- attr(registry, "outcome") %||% NULL

  # Validate MAS prerequisites once before the model loop
  if (isTRUE(show_mas) && (is.null(exposure) || is.null(outcome))) {
    message("MAS annotation skipped: exposure/outcome not set on registry.")
    show_mas <- FALSE
  }

  # Compute coordinates for all nodes
  coords <- .compute_node_coords(node_names, node_timing, exposure, outcome)

  # Build plots
  plots <- vector("list", length(model_ids))
  names(plots) <- model_ids

  for (i in seq_along(model_ids)) {
    mid <- model_ids[i]
    plots[[i]] <- .build_single_dag_plot(
      registry = registry,
      states = states,
      model_id = mid,
      coords = coords,
      node_names = node_names,
      exposure = exposure,
      outcome = outcome,
      show_mas = show_mas
    )
  }

  # Save plots if requested
  if (!is.null(save_path)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path, recursive = TRUE)
    }
    for (mid in names(plots)) {
      file_path <- file.path(save_path, paste0("model_", mid, ".png"))
      ragg::agg_png(filename = file_path, width = 2400, height = 1200, res = 360)
      tryCatch(print(plots[[mid]]), finally = invisible(dev.off()))
    }
    message("Saved ", length(plots), " plots to ", save_path)
  }

  plots
}


.resolve_node_timing <- function(registry, node_names) {
  nt <- attr(registry, "node_timing")
  if (!is.null(nt)) {
    nt <- as.integer(nt)
    # Ensure all nodes are represented
    missing <- setdiff(node_names, names(nt))
    if (length(missing) > 0) {
      nt[missing] <- 1L
    }
    return(nt[node_names])
  }

  # Try to infer from node names matching X\\d+ pattern
  inferred <- vapply(node_names, function(nm) {
    m <- regmatches(nm, regexec("^X(\\d+)$", nm))[[1]]
    if (length(m) >= 2) {
      return(as.integer(m[2]))
    }
    NA_integer_
  }, integer(1), USE.NAMES = TRUE)

  if (!any(is.na(inferred))) {
    return(inferred)
  }

  # Fallback: flat layout (all timing = 1)
  stats::setNames(rep(1L, length(node_names)), node_names)
}


.compute_node_coords <- function(node_names, node_timing, exposure = NULL,
                                  outcome = NULL) {
  timings <- node_timing[node_names]
  unique_timings <- sort(unique(timings), decreasing = TRUE)

  # x-coords: latest timing = 1 (right), earliest = -1 (left)
  n_t <- length(unique_timings)
  if (n_t == 1) {
    x_positions <- 0
  } else {
    x_positions <- seq(1, -1, length.out = n_t)
  }
  x_map <- stats::setNames(x_positions, as.character(unique_timings))

  x <- x_map[as.character(timings)]
  names(x) <- node_names

  # y-coords: spread nodes at the same timing vertically
  y <- stats::setNames(numeric(length(node_names)), node_names)
  buffer_y <- 1.5

  for (t in unique_timings) {
    nodes_at_t <- node_names[timings == t]
    # Place exposure/outcome at y = 0 first
    special <- intersect(nodes_at_t, c(exposure, outcome))
    others <- setdiff(nodes_at_t, c(exposure, outcome))

    for (s in special) y[s] <- 0

    n <- length(others)
    if (n == 1) {
      y[others] <- 0.5
    } else if (n > 1) {
      positions <- seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * buffer_y
      y[others] <- positions
    }
  }

  list(x = x, y = y)
}


.build_single_dag_plot <- function(registry, states, model_id, coords,
                                    node_names, exposure, outcome,
                                    show_mas) {
  # Filter states to this model
  model_states <- states[as.character(states$model_id) == model_id, , drop = FALSE]

  # Join with registry to get type/source/target per component
  joined <- merge(model_states,
                  registry[, c("comp_id", "type", "source", "target", "direction")],
                  by = "comp_id", all.x = TRUE)

  # Identify edges by status
  edge_rows <- joined[joined$type == "edge", , drop = FALSE]
  causal_edges <- edge_rows[edge_rows$status == "causal", , drop = FALSE]
  unknown_edges <- edge_rows[edge_rows$status == "unknown", , drop = FALSE]

  # Build formulas for causal edges
  causal_formulas <- list()
  if (nrow(causal_edges) > 0) {
    causal_formulas <- lapply(seq_len(nrow(causal_edges)), function(i) {
      src <- as.character(causal_edges$source[i])
      tgt <- as.character(causal_edges$target[i])
      as.formula(paste0(tgt, " ~ ", src))
    })
  }

  # Build dagify object only when there are causal edges
  # (ggdag fails on edgeless DAGs)
  dag <- NULL
  if (nrow(causal_edges) > 0) {
    dag_args <- causal_formulas
    dag_args$coords <- coords
    if (!is.null(exposure)) dag_args$exposure <- exposure
    if (!is.null(outcome)) dag_args$outcome <- outcome

    dag <- tryCatch(
      do.call(ggdag::dagify, dag_args),
      error = function(e) NULL
    )
  }

  # Build edge segment data for unknown edges (dashed gray overlay)
  unknown_segments <- NULL
  if (nrow(unknown_edges) > 0) {
    unknown_segments <- data.frame(
      x = coords$x[as.character(unknown_edges$source)],
      y = coords$y[as.character(unknown_edges$source)],
      xend = coords$x[as.character(unknown_edges$target)],
      yend = coords$y[as.character(unknown_edges$target)],
      stringsAsFactors = FALSE
    )
  }

  # Build the plot
  if (!is.null(dag)) {
    p <- dag %>%
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
      ggdag::geom_dag_point(color = "white", na.rm = FALSE) +
      ggdag::geom_dag_edges() +
      ggdag::geom_dag_text(color = "black", na.rm = FALSE) +
      ggdag::theme_dag()
  } else {
    # No causal edges - render nodes manually
    node_df <- data.frame(
      x = coords$x[node_names],
      y = coords$y[node_names],
      label = node_names,
      stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(node_df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(color = "white", size = 8, shape = 21,
                          fill = "grey90") +
      ggplot2::geom_text(ggplot2::aes(label = label), color = "black") +
      ggdag::theme_dag()
  }

  # Overlay unknown edges as dashed gray segments
  if (!is.null(unknown_segments) && nrow(unknown_segments) > 0) {
    p <- p + ggplot2::geom_segment(
      data = unknown_segments,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = "grey60", linetype = "dashed",
      arrow = grid::arrow(length = grid::unit(0.08, "inches"), type = "closed"),
      inherit.aes = FALSE, na.rm = TRUE
    )
  }

  # Title
  p <- p + ggplot2::ggtitle(paste("Model", model_id))

  # MAS annotation
  if (show_mas) {
    mas_label <- .compute_mas_label(dag, exposure, outcome)
    if (!is.null(mas_label)) {
      p <- p + ggplot2::annotate(
        "text", label = mas_label,
        x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
        size = 3.5
      )
    }
  }

  p
}


.compute_mas_label <- function(dag, exposure, outcome) {
  if (is.null(dag) || is.null(exposure) || is.null(outcome)) {
    return(NULL)
  }

  mas <- tryCatch(
    dagitty::adjustmentSets(dag, exposure = exposure,
                            outcome = outcome, effect = "direct"),
    error = function(e) NULL
  )

  if (is.null(mas)) {
    return(paste0("MAS = (error)"))
  }

  if (length(unlist(mas)) == 0) {
    return("MAS = {}")
  }

  mas_text <- vapply(mas, function(s) {
    paste0("{", paste(sort(s), collapse = ", "), "}")
  }, character(1))
  paste0("MAS = ", paste(mas_text, collapse = " | "))
}

#' Plot DAG models from a component registry and model states
#'
#' Creates one DAG plot per model, showing causal edges as solid arrows,
#' unknown-status edges as dashed gray arrows, and bidirectional residual
#' covariances (<->) as curved dotted two-headed arrows.
#'
#' @param registry A data frame returned by \code{\link{build_component_registry}}
#'   with columns \code{comp_id}, \code{type}, \code{source}, \code{target},
#'   \code{direction}. The \code{node_timing} or \code{timing_options} attribute
#'   is used to position nodes; when absent, timing is inferred from node names
#'   matching \code{X\\d+} or defaults to a flat layout.
#' @param states A data frame returned by \code{\link{expand_model_states}}
#'   with columns \code{model_id}, \code{comp_id}, \code{status}.
#' @param model_ids Optional character vector of model IDs to plot. When
#'   \code{NULL}, defaults to the first 6 unique model IDs (unless \code{plot_all = TRUE}).
#' @param plot_all Logical. When \code{TRUE}, plots all models in \code{states}
#'   when \code{model_ids} is \code{NULL}. Defaults to \code{FALSE}.
#' @param show_mas Logical. When \code{TRUE}, annotate each plot with the
#'   minimal adjustment set for the exposure-outcome effect. Requires
#'   \code{exposure} and \code{outcome} attributes on \code{registry}.
#'   Defaults to \code{FALSE}.
#' @param save_path Optional directory path. When supplied, each plot is
#'   saved as a PNG file named \code{model_{model_id}.png}.
#' @param width Integer pixel width for saved PNG images. Defaults to \code{2400}.
#' @param height Integer pixel height for saved PNG images. Defaults to \code{1200}.
#' @param res Integer resolution (PPI) for saved PNG images. Defaults to \code{360}.
#' @param scale Numeric multiplier to adjust visual element sizes (node circles,
#'   font sizes, arrow lengths). Useful when creating smaller plots so text and
#'   arrows stay well-proportioned. Defaults to \code{1.0}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list of \code{ggplot} objects, one per model. Names are
#'   the model IDs.
#'
#' @export
plot_dag_models <- function(registry,
                            states,
                            model_ids = NULL,
                            plot_all = FALSE,
                            show_mas = FALSE,
                            save_path = NULL,
                            width = 2400,
                            height = 1200,
                            res = 360,
                            scale = 1.0,
                            ...) {
    if (!is.data.frame(registry)) {
        stop("registry must be a data frame from build_component_registry().", call. = FALSE)
    }
    if (!is.data.frame(states)) {
        stop("states must be a data frame from expand_model_states().", call. = FALSE)
    }

    reg_required <- c("comp_id", "type", "source", "target")
    reg_missing <- setdiff(reg_required, names(registry))
    if (length(reg_missing) > 0) {
        stop("registry is missing column(s): ", paste(reg_missing, collapse = ", "), call. = FALSE)
    }

    state_required <- c("model_id", "comp_id", "status")
    state_missing <- setdiff(state_required, names(states))
    if (length(state_missing) > 0) {
        stop("states is missing column(s): ", paste(state_missing, collapse = ", "), call. = FALSE)
    }

    available_ids <- unique(as.character(states$model_id))
    if (length(available_ids) == 0) {
        stop("states has no model IDs.", call. = FALSE)
    }

    if (is.null(model_ids)) {
        if (isTRUE(plot_all)) {
            model_ids <- available_ids
        } else {
            model_ids <- available_ids[seq_len(min(6, length(available_ids)))]
            if (length(available_ids) > 6) {
                message("Plotting the first 6 models. Set plot_all = TRUE or pass model_ids to select others.")
            }
        }
    } else {
        model_ids <- as.character(model_ids)
        missing_ids <- setdiff(model_ids, available_ids)
        if (length(missing_ids) > 0) {
            stop("Model(s) not found in state data: ", paste(missing_ids, collapse = ", "), call. = FALSE)
        }
    }

    node_names <- unique(as.character(registry$source[registry$type == "node"]))
    node_timing <- .resolve_node_timing(registry, node_names)

    exposure <- attr(registry, "exposure") %||% NULL
    outcome <- attr(registry, "outcome") %||% NULL

    if (isTRUE(show_mas) && (is.null(exposure) || is.null(outcome))) {
        message("MAS annotation skipped: exposure/outcome not set on registry.")
        show_mas <- FALSE
    }

    coords <- .compute_node_coords(node_names, node_timing, exposure, outcome)

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
            show_mas = show_mas,
            scale = scale
        )
    }

    if (!is.null(save_path)) {
        if (!dir.exists(save_path)) {
            dir.create(save_path, recursive = TRUE)
        }
        for (mid in names(plots)) {
            file_path <- file.path(save_path, paste0("model_", mid, ".png"))
            ragg::agg_png(filename = file_path, width = width, height = height, res = res)
            tryCatch(print(plots[[mid]]), finally = invisible(dev.off()))
        }
        message("Saved ", length(plots), " plots to ", save_path)
    }

    plots
}


.resolve_node_timing <- function(registry, node_names) {
    nt <- attr(registry, "node_timing")

    if (is.null(nt)) {
        to <- attr(registry, "timing_options")
        if (!is.null(to)) {
            nt <- vapply(node_names, function(nm) {
                if (nm %in% names(to) && length(to[[nm]]) > 0) as.integer(to[[nm]][[1]]) else 1L
            }, integer(1), USE.NAMES = TRUE)
        }
    }

    if (!is.null(nt)) {
        nt <- stats::setNames(as.integer(nt), names(nt))
        missing <- setdiff(node_names, names(nt))
        if (length(missing) > 0) {
            fill <- stats::setNames(rep(1L, length(missing)), missing)
            nt <- c(nt, fill)
        }
        return(nt[node_names])
    }

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

    stats::setNames(rep(1L, length(node_names)), node_names)
}


.compute_node_coords <- function(node_names, node_timing, exposure = NULL, outcome = NULL) {
    timings <- node_timing[node_names]
    unique_timings <- sort(unique(timings), decreasing = TRUE)

    n_t <- length(unique_timings)
    if (n_t == 1) {
        x_positions <- 0
    } else {
        x_positions <- seq(1, -1, length.out = n_t)
    }
    x_map <- stats::setNames(x_positions, as.character(unique_timings))

    x <- x_map[as.character(timings)]
    names(x) <- node_names

    y <- stats::setNames(numeric(length(node_names)), node_names)
    buffer_y <- 1.0

    unique_timings_asc <- sort(unique(timings))
    offset_seq <- c(0.75, -0.75, 1.25, -1.25, 1.75, -1.75, 2.25, -2.25)
    offset_idx <- 1

    for (t in unique_timings_asc) {
        nodes_at_t <- node_names[timings == t]
        special <- intersect(nodes_at_t, c(exposure, outcome))
        others <- setdiff(nodes_at_t, c(exposure, outcome))

        for (s in special) y[s] <- 0

        n <- length(others)
        if (n > 0) {
            if (length(special) > 0) {
                offsets <- c(1.0, -1.0, 2.0, -2.0, 3.0, -3.0)
                y[others] <- offsets[1:n] * buffer_y
            } else {
                base_y <- offset_seq[offset_idx] * buffer_y
                offset_idx <- offset_idx + 1

                if (n == 1) {
                    y[others] <- base_y
                } else {
                    y[others] <- base_y + seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * buffer_y
                }
            }
        }
    }

    list(x = x, y = y)
}


.build_single_dag_plot <- function(registry, states, model_id, coords,
                                   node_names, exposure, outcome,
                                   show_mas, scale = 1.0) {
    model_states <- states[as.character(states$model_id) == model_id, , drop = FALSE]

    merge_cols <- intersect(c("comp_id", "type", "source", "target", "direction", "description"), names(registry))
    joined <- merge(model_states, registry[, merge_cols, drop = FALSE], by = "comp_id", all.x = TRUE)

    edge_types <- c("edge", "bidirected", "bidirected_edge", "covariance")
    edge_rows <- joined[tolower(as.character(joined$type)) %in% edge_types, , drop = FALSE]

    if (nrow(edge_rows) == 0) {
        edge_rows <- joined[joined$type != "node", , drop = FALSE]
    }

    dir_col <- if ("direction" %in% names(edge_rows)) as.character(edge_rows$direction) else rep("", nrow(edge_rows))
    desc_col <- if ("description" %in% names(edge_rows)) as.character(edge_rows$description) else rep("", nrow(edge_rows))
    dir_col[is.na(dir_col)] <- ""
    desc_col[is.na(desc_col)] <- ""

    is_bidirected <- (dir_col == "<->") | grepl("<->", desc_col, fixed = TRUE)

    inactive_statuses <- c("non-causal", "absent", "forbidden", "false")
    status_clean <- tolower(trimws(as.character(edge_rows$status)))

    causal_edges <- edge_rows[status_clean == "causal" & !is_bidirected, , drop = FALSE]
    unknown_edges <- edge_rows[status_clean == "unknown" & !is_bidirected, , drop = FALSE]
    cov_edges <- edge_rows[!status_clean %in% inactive_statuses & is_bidirected, , drop = FALSE]

    causal_formulas <- list()
    if (nrow(causal_edges) > 0) {
        causal_formulas <- lapply(seq_len(nrow(causal_edges)), function(i) {
            src <- as.character(causal_edges$source[i])
            tgt <- as.character(causal_edges$target[i])
            as.formula(paste0(tgt, " ~ ", src))
        })
    }

    dag <- NULL
    if (length(causal_formulas) > 0) {
        dag_args <- causal_formulas
        dag_args$coords <- coords
        if (!is.null(exposure)) dag_args$exposure <- exposure
        if (!is.null(outcome)) dag_args$outcome <- outcome

        dag <- tryCatch(
            do.call(ggdag::dagify, dag_args),
            error = function(e) NULL
        )
    }

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

    cov_segments <- NULL
    if (nrow(cov_edges) > 0) {
        srcs <- as.character(cov_edges$source)
        tgts <- as.character(cov_edges$target)

        for (idx in seq_len(nrow(cov_edges))) {
            if ((is.na(srcs[idx]) || srcs[idx] == "") && "description" %in% names(cov_edges)) {
                parts <- trimws(strsplit(as.character(cov_edges$description[idx]), "<->", fixed = TRUE)[[1]])
                if (length(parts) == 2) {
                    srcs[idx] <- parts[1]
                    tgts[idx] <- parts[2]
                }
            }
        }

        valid_idx <- srcs %in% names(coords$x) & tgts %in% names(coords$x)
        if (any(valid_idx)) {
            cov_segments <- data.frame(
                x = coords$x[srcs[valid_idx]],
                y = coords$y[srcs[valid_idx]],
                xend = coords$x[tgts[valid_idx]],
                yend = coords$y[tgts[valid_idx]],
                stringsAsFactors = FALSE
            )
        }
    }

    # Dynamically calculate sizes based on scale multiplier
    pt_size <- 8 * scale
    txt_size <- 3.8 * scale
    arrow_len <- 0.08 * scale

    # Base DAG / Causal Edges
    if (!is.null(dag)) {
        p <- ggplot2::ggplot(dag, ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
            ggdag::geom_dag_edges()
    } else {
        node_df <- data.frame(x = coords$x[node_names], y = coords$y[node_names], label = node_names, stringsAsFactors = FALSE)
        p <- ggplot2::ggplot(node_df, ggplot2::aes(x = x, y = y))
    }

    # Add Unknown Edges (Straight dashed gray)
    if (!is.null(unknown_segments) && nrow(unknown_segments) > 0) {
        p <- p + ggplot2::geom_segment(
            data = unknown_segments,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            color = "grey60", linetype = "dashed",
            arrow = grid::arrow(length = grid::unit(arrow_len, "inches"), type = "closed"),
            inherit.aes = FALSE, na.rm = TRUE
        )
    }

    # Add Residual Covariances (Curved, dotted black, two-headed)
    if (!is.null(cov_segments) && nrow(cov_segments) > 0) {
        p <- p + ggplot2::geom_curve(
            data = cov_segments,
            ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            color = "black", linetype = "dotted", curvature = -0.3,
            arrow = grid::arrow(length = grid::unit(arrow_len, "inches"), ends = "both", type = "closed"),
            inherit.aes = FALSE, na.rm = TRUE
        )
    }

    # Draw Nodes and Labels on top
    if (!is.null(dag)) {
        p <- p +
            ggdag::geom_dag_point(color = "white", size = pt_size, na.rm = FALSE) +
            ggdag::geom_dag_text(color = "black", size = txt_size, na.rm = FALSE) +
            ggdag::theme_dag()
    } else {
        p <- p +
            ggplot2::geom_point(color = "white", size = pt_size, shape = 21, fill = "grey90") +
            ggplot2::geom_text(ggplot2::aes(label = label), color = "black", size = txt_size) +
            ggdag::theme_dag()
    }

    # Title
    p <- p + ggplot2::ggtitle(paste("Model", model_id)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 12 * scale))

    # MAS annotation
    if (show_mas) {
        mas_label <- .compute_mas_label(dag, exposure, outcome)
        if (!is.null(mas_label)) {
            p <- p + ggplot2::annotate(
                "text", label = mas_label,
                x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
                size = 3.5 * scale
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

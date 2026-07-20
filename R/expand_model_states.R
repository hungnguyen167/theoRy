#' Expand Model States using Fixed Parameters
#'
#' @export
expand_model_states <- function(registry, host = "127.0.0.1", port = 8000L) {
    if (!is.data.frame(registry)) {
        stop("Input must be a component registry data frame.")
    }

    url <- sprintf("http://%s:%d/api/v1/model-states", host, port)

    time_orders <- attr(registry, "time_orders")
    optional_nodes <- attr(registry, "optional_nodes") %||% character(0)
    constraints <- attr(registry, "constraints") %||% list() # Retrieve constraints

    timing_dict <- list()
    if (!is.null(time_orders)) {
        for (name in names(time_orders)) {
            timing_dict[[name]] <- as.integer(time_orders[[name]][1])
        }
    }

    n_opt <- length(optional_nodes)
    opt_grid <- if (n_opt > 0) expand.grid(rep(list(c(TRUE, FALSE)), n_opt)) else data.frame(dummy = TRUE)
    if (n_opt > 0) names(opt_grid) <- optional_nodes

    all_states <- list()
    model_offset <- 0

    for (i in seq_len(nrow(opt_grid))) {
        current_registry <- registry
        current_constraints <- constraints # Copy base constraints

        if (n_opt > 0) {
            absent_nodes <- optional_nodes[!as.logical(opt_grid[i, ])]
            if (length(absent_nodes) > 0) {
                # 1. Prune nodes and edges
                current_registry <- current_registry[!(current_registry$type == "node" & current_registry$source %in% absent_nodes), ]
                current_registry <- current_registry[!(current_registry$type == "edge" &
                                                           (current_registry$source %in% absent_nodes | current_registry$target %in% absent_nodes)), ]

                # 2. Prune constraints referring to absent nodes
                current_constraints <- Filter(function(c) !(c$source %in% absent_nodes || c$target %in% absent_nodes), current_constraints)
            }
        }

        registry_records <- lapply(seq_len(nrow(current_registry)), function(j) {
            row <- current_registry[j, ]
            entry <- list(
                comp_id = row$comp_id,
                type = row$type,
                source = row$source,
                description = row$description,
                observed = as.logical(row$observed)
            )
            if (!is.na(row$target) && row$target != "") entry$target <- row$target
            if (!is.na(row$direction) && row$direction != "") entry$direction <- row$direction

            # Pass fixed_status so Python's ComponentRecord locks the edge!
            if ("fixed_status" %in% names(row) && !is.na(row$fixed_status) && row$fixed_status != "") {
                entry$fixed_status <- row$fixed_status
            }

            entry
        })

        # Inject constraints into the payload
        payload <- list(
            registry_data = registry_records,
            constraints = current_constraints,
            mode = "exhaustive",
            node_timing = timing_dict,
            max_models = 50000L,
            node_policy = "all-present",
            exposure = attr(registry, "exposure") %||% "X1",
            outcome = attr(registry, "outcome") %||% "Y"
        )

        resp <- httr2::request(url) |>
            httr2::req_method("POST") |>
            httr2::req_body_json(payload) |>
            httr2::req_perform()

        multiverse_data <- httr2::resp_body_json(resp)
        state_list <- multiverse_data$data$state_data

        if (length(state_list) > 0) {
            batch_model_ids <- unique(vapply(state_list, function(x) x$model_id, character(1)))
            new_ids <- sprintf("M%04d", seq_along(batch_model_ids) + model_offset)
            id_map <- setNames(new_ids, batch_model_ids)
            for (k in seq_along(state_list)) state_list[[k]]$model_id <- id_map[state_list[[k]]$model_id]
            model_offset <- model_offset + length(batch_model_ids)
            all_states <- c(all_states, state_list)
        }
    }
    return(all_states)
}

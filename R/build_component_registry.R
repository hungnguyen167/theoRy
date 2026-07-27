#' Build the Component Registry (Interactive or Manual)
#'
#' @export
build_component_registry <- function(n_x = NULL, time_orders = NULL, post_y = NULL,
                                     forced_edges = list(), forbidden_edges = list(),
                                     confounded_pairs = list(), optional_nodes = character(0),
                                     host = "127.0.0.1", port = 8000L) {

    parse_pairs <- function(str) {
        if (trimws(str) == "") return(list())
        matches <- regmatches(str, gregexpr("\\(.*?\\)", str))[[1]]
        if (length(matches) == 0) return(list())

        lapply(matches, function(m) {
            clean <- gsub("\\(|\\)|\\s", "", m)
            parts <- strsplit(clean, ",")[[1]]
            if (length(parts) != 2) stop("Invalid pair format. Expected (A,B).")
            parts
        })
    }

    format_pair_list <- function(pair_list) {
        if (length(pair_list) == 0) return("list()")
        items <- sapply(pair_list, function(p) sprintf("c(\"%s\", \"%s\")", p[1], p[2]))
        sprintf("list(%s)", paste(items, collapse = ", "))
    }

    # --- INTERACTIVE LOOP ---
    if (is.null(n_x)) {
        if (!interactive()) stop("Interactive mode requires an active R session.")

        cat("Welcome to the theoRy Component Registry Builder.\n")
        confirmed <- FALSE
        while (!confirmed) {
            cat("How many X variables are there in your theory or theories? (max 7):\n")
            n_x_str <- readline("> ")
            n_x <- as.integer(trimws(n_x_str))
            if (is.na(n_x) || n_x < 1) { message("Invalid input."); next }
            if (n_x > 7) {
                message("Error: 7 is the current maximum, until we introduce geometric computing time reductions.\n")
                next
            }

            cat("\nPlease order variables X1 through X", n_x, " chronologically.\n", sep="")
            time_orders <- list()
            multi_time_count <- 0
            invalid_time <- FALSE

            for (i in 1:n_x) {
                var_name <- paste0("X", i)
                t_input <- readline(sprintf("%s: ", var_name))
                t_vals <- as.integer(strsplit(trimws(t_input), ",")[[1]])
                if (any(is.na(t_vals))) { message("Invalid integer."); invalid_time <- TRUE; break }
                if (length(t_vals) > 3) { message("Max 3 time points per variable allowed."); invalid_time <- TRUE; break }
                if (length(t_vals) > 1) {
                    multi_time_count <- multi_time_count + 1
                    if (multi_time_count > 2) { message("Max 2 variables with multiple times allowed."); invalid_time <- TRUE; break }
                }
                time_orders[[var_name]] <- sort(unique(t_vals))
            }
            if (invalid_time) next

            all_times <- unlist(time_orders)
            y_time <- max(all_times) + 1
            time_orders[["Y"]] <- y_time

            cat("\nCan any variable potentially occur after Y?\nIf so, please name it; if not hit enter:\n")
            post_y_input <- trimws(readline("> "))
            if (nzchar(post_y_input)) {
                if (!post_y_input %in% paste0("X", 1:n_x)) { message("Invalid variable choice."); next }
                post_y <- post_y_input
                time_orders[[post_y]] <- c(time_orders[[post_y]], y_time + 1)
            } else {
                post_y <- NULL
            }

            cat("\nWhich paths are known to be causal?\n(Note: X1 -> Y is automatically forced as the core exposure/outcome).\n(Pairs like (X1,X2),(X2,Y),(X3,X4) or enter for none):\n")
            forced_str <- readline("> ")
            forced_edges <- tryCatch(parse_pairs(forced_str), error = function(e) { message(e$message); return(NULL) })
            if (is.null(forced_edges)) next

            cat("\nWhich paths are known to not be causal?\n(Pairs like (X1,X2) or enter for none):\n")
            forbidden_str <- readline("> ")
            forbidden_edges <- tryCatch(parse_pairs(forbidden_str), error = function(e) { message(e$message); return(NULL) })
            if (is.null(forbidden_edges)) next

            # Check for forbidden X1 -> Y violation
            has_forbidden_x1_y <- any(sapply(forbidden_edges, function(p) p[1] == "X1" && p[2] == "Y"))
            if (has_forbidden_x1_y) {
                message("Error: X1 -> Y cannot be marked non-causal. It is the primary focal exposure/outcome path.")
                next
            }

            if (length(forced_edges) > 0 && length(forbidden_edges) > 0) {
                forced_char <- sapply(forced_edges, paste, collapse="->")
                forbidden_char <- sapply(forbidden_edges, paste, collapse="->")
                if (any(forced_char %in% forbidden_char)) { message("Conflict found between causal and non-causal paths. Restarting."); next }
            }

            cat("\nShould variables that are simultaneous in time get a bi-directional arrow?\n(Pairs like (X1,X2) or enter for none):\n")
            confounded_str <- readline("> ")
            confounded_pairs <- tryCatch(parse_pairs(confounded_str), error = function(e) { message(e$message); return(NULL) })
            if (is.null(confounded_pairs)) next

            cat("\nGenerate multiverse models where the following variables are removed\n(separate multiple with a comma), or hit enter to not allow any subset models:\n")
            opt_input <- trimws(readline("> "))
            if (nzchar(opt_input)) {
                clean_opt <- gsub("\\s+", "", opt_input)
                optional_nodes <- strsplit(clean_opt, ",")[[1]]

                valid_opts <- paste0("X", 2:n_x)
                invalid_opt <- setdiff(optional_nodes, valid_opts)
                if (length(invalid_opt) > 0) {
                    message("Error: Cannot omit Exposure (X1) or unrecognized variables. Invalid inputs: ", paste(invalid_opt, collapse=", "))
                    next
                }
            } else {
                optional_nodes <- character(0)
            }

            # Enforce core path in display
            display_forced <- unique(c(list(c("X1", "Y")), forced_edges))

            cat("\n--- THEORETICAL PARAMETERS REPORT ---\n")
            cat("Exposure: X1\nOutcome: Y\n\n")
            cat("Variables & Chronological Order:\n")
            for (v in names(time_orders)) {
                cat(sprintf("  %s: %s\n", v, paste(time_orders[[v]], collapse=", ")))
            }
            cat("\nCausal Paths (Forced Edges):\n")
            for (edge in display_forced) cat(sprintf("  %s -> %s\n", edge[1], edge[2]))

            cat("\nNon-Causal Paths (Forbidden Edges):\n")
            if (length(forbidden_edges) == 0) cat("  None\n") else {
                for (edge in forbidden_edges) cat(sprintf("  %s !-> %s\n", edge[1], edge[2]))
            }
            cat("\nBi-directional Covariances (Confounded Pairs):\n")
            if (length(confounded_pairs) == 0) cat("  None\n") else {
                for (edge in confounded_pairs) cat(sprintf("  %s <-> %s\n", edge[1], edge[2]))
            }
            cat("\nSubset Models (Optional Nodes):\n")
            if (length(optional_nodes) == 0) cat("  None\n") else {
                cat(sprintf("  %s\n", paste(optional_nodes, collapse=", ")))
            }

            # --- MANUAL REPRODUCTION COMMAND CODE BLOCK ---
            cat("\n--- REPRODUCIBLE MANUAL CODE ---\n")
            cat("To recreate this registry programmatically, run:\n\n")

            time_orders_str <- sprintf(
                "list(%s)",
                paste(sapply(names(time_orders), function(k) {
                    sprintf("%s = c(%s)", k, paste(time_orders[[k]], collapse = ", "))
                }), collapse = ", ")
            )

            post_y_str <- if (is.null(post_y)) "NULL" else sprintf("\"%s\"", post_y)
            opt_nodes_str <- if (length(optional_nodes) == 0) "character(0)" else sprintf("c(%s)", paste(sprintf("\"%s\"", optional_nodes), collapse = ", "))

            cat(sprintf("registry_df <- build_component_registry(\n"))
            cat(sprintf("  n_x = %d,\n", n_x))
            cat(sprintf("  time_orders = %s,\n", time_orders_str))
            cat(sprintf("  post_y = %s,\n", post_y_str))
            cat(sprintf("  forced_edges = %s,\n", format_pair_list(forced_edges)))
            cat(sprintf("  forbidden_edges = %s,\n", format_pair_list(forbidden_edges)))
            cat(sprintf("  confounded_pairs = %s,\n", format_pair_list(confounded_pairs)))
            cat(sprintf("  optional_nodes = %s\n", opt_nodes_str))
            cat(")\n")
            cat("-------------------------------------\n\n")

            cat("Do you agree with these parameters? (Y/N):\n")
            confirm_str <- readline("> ")
            if (toupper(trimws(confirm_str)) == "Y") confirmed <- TRUE
        }
    }

    # --- ALWAYS ENFORCE X1 -> Y AS A FORCED CAUSAL EDGE ---
    forced_edges <- unique(c(list(c("X1", "Y")), forced_edges))


    # --- MAP TO PYTHON PYDANTIC SCHEMAS ---
    nodes_list <- list()
    for (name in names(time_orders)) {
        nodes_list[[length(nodes_list) + 1]] <- list(
            name = name,
            timing = as.integer(time_orders[[name]][1]),
            description = paste("Variable", name),
            observed = TRUE
        )
    }

    constraints_list <- list()
    if (length(forced_edges) > 0) {
        for (edge in forced_edges) {
            constraints_list[[length(constraints_list) + 1]] <- list(
                source = edge[1], target = edge[2], direction = "->", rule = "require"
            )
        }
    }
    if (length(forbidden_edges) > 0) {
        for (edge in forbidden_edges) {
            constraints_list[[length(constraints_list) + 1]] <- list(
                source = edge[1], target = edge[2], direction = "->", rule = "forbid"
            )
        }
    }
    if (length(confounded_pairs) > 0) {
        for (edge in confounded_pairs) {
            constraints_list[[length(constraints_list) + 1]] <- list(
                source = edge[1], target = edge[2], direction = "<->", rule = "require"
            )
        }
    }

    payload <- list(
        nodes = nodes_list,
        respect_timing = TRUE,
        include_bidirectional = (length(confounded_pairs) > 0),
        constraints = constraints_list,
        exposure = "X1",
        outcome = "Y"
    )

    url <- sprintf("http://%s:%d/api/v1/component-registry", host, port)

    resp <- httr2::request(url) |>
        httr2::req_method("POST") |>
        httr2::req_body_json(payload) |>
        httr2::req_perform()

    registry_data <- httr2::resp_body_json(resp)

    # --- CONVERT TO ORIGINAL FLAT DATA FRAME FORMAT ---
    raw_records <- registry_data$data$registry_data

    df_list <- lapply(raw_records, function(r) {
        # Check if this edge is the primary exposure -> outcome path
        is_exposure_edge <- !is.null(r$source) && !is.null(r$target) &&
            r$source == "X1" && r$target == "Y" &&
            r$type == "edge"

        data.frame(
            comp_id      = r$comp_id %||% NA_character_,
            type         = r$type %||% NA_character_,
            source       = r$source %||% NA_character_,
            target       = r$target %||% NA_character_,
            direction    = r$direction %||% NA_character_,
            description  = r$description %||% NA_character_,
            fixed_status = if (is_exposure_edge) "causal" else (r$fixed_status %||% NA_character_),
            observed     = r$observed %||% TRUE,
            stringsAsFactors = FALSE
        )
    })
    registry_df <- do.call(rbind, df_list)

    # Preserve attributes
    attr(registry_df, "exposure") <- "X1"
    attr(registry_df, "outcome") <- "Y"
    attr(registry_df, "time_orders") <- time_orders
    attr(registry_df, "optional_nodes") <- optional_nodes
    attr(registry_df, "constraints") <- constraints_list

    message("\nComponent registry built and locked successfully!")
    return(registry_df)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

library(dplyr)

#' Print a Semantic Text Summary of One or Two Models
#'
#' @export
print_model_summary <- function(models, states, registry, dyads = NULL) {

    # 1. Parse 'models' argument ("M0001", c("M0001", "M0004"), or "M0001, M0004")
    if (length(models) == 1 && grepl(",", models)) {
        models <- strsplit(models, ",")[[1]]
    }
    models <- trimws(models)

    if (length(models) == 0 || length(models) > 2) {
        stop("Please provide exactly 1 or 2 model IDs.")
    }

    # Standardize registry
    if (is.data.frame(registry)) {
        registry_df <- registry %>% mutate(comp_id = as.character(comp_id))
    } else {
        registry_df <- as.data.frame(registry, stringsAsFactors = FALSE) %>%
            mutate(comp_id = as.character(comp_id))
    }

    # 2. Robust Parser for Nested 'states' List
    flatten_states_list <- function(raw_states) {
        if (is.data.frame(raw_states)) {
            return(raw_states %>% mutate(comp_id = as.character(comp_id)))
        }

        # Unwrap top-level API wrappers if present (e.g., states$data$model_states)
        if (is.list(raw_states) && "data" %in% names(raw_states)) {
            raw_states <- raw_states$data
        }
        if (is.list(raw_states) && "model_states" %in% names(raw_states)) {
            raw_states <- raw_states$model_states
        }

        # Iterate over models and nested components safely
        rows <- list()

        for (item in raw_states) {
            # Extract model ID
            m_id <- item$model_id %||% item[["model_id"]]

            # Extract components/states array inside this model
            comps <- item$components %||% item$states %||% item$components_state %||% list()

            if (length(comps) > 0) {
                for (comp in comps) {
                    rows[[length(rows) + 1]] <- data.frame(
                        model_id = as.character(m_id %||% NA_character_),
                        comp_id  = as.character(comp$comp_id %||% comp[["comp_id"]] %||% NA_character_),
                        status   = as.character(comp$status %||% comp[["status"]] %||% NA_character_),
                        stringsAsFactors = FALSE
                    )
                }
            } else if (!is.null(m_id) && !is.null(item$comp_id)) {
                # Case where states is a flat list of individual component entries
                rows[[length(rows) + 1]] <- data.frame(
                    model_id = as.character(m_id),
                    comp_id  = as.character(item$comp_id),
                    status   = as.character(item$status %||% NA_character_),
                    stringsAsFactors = FALSE
                )
            }
        }

        if (length(rows) == 0) {
            stop("Could not parse components from 'states' list. Verify the structure of the states object.")
        }

        return(do.call(rbind, rows))
    }

    flat_states <- flatten_states_list(states)

    # 3. Helper to extract and format model text
    get_model_summary <- function(target_model_id) {

        m_states <- flat_states %>%
            filter(as.character(model_id) == as.character(target_model_id))

        if (nrow(m_states) == 0) {
            return(list(nodes = "None (Model ID not found)", edges = "  - None"))
        }

        # Merge with registry
        m_full <- m_states %>% left_join(registry_df, by = "comp_id")

        # Resolve column names if duplicates exist from join
        col_resolve <- function(df, col_name) {
            x_col <- paste0(col_name, ".x")
            y_col <- paste0(col_name, ".y")
            if (x_col %in% names(df)) df[[col_name]] <- df[[x_col]]
            else if (y_col %in% names(df) && !col_name %in% names(df)) df[[col_name]] <- df[[y_col]]
            return(df)
        }

        m_full <- col_resolve(m_full, "type")
        m_full <- col_resolve(m_full, "source")
        m_full <- col_resolve(m_full, "target")

        # Extract Nodes
        nodes_df <- m_full %>% filter(type == "node", status %in% c("present", "observed", "active", "causal"))
        nodes <- if (nrow(nodes_df) > 0) unique(nodes_df$source) else character(0)

        # Fallback if 'node' type isn't explicit in states
        if (length(nodes) == 0) {
            active_edges <- m_full %>% filter(type == "edge", status %in% c("causal", "present"))
            nodes <- unique(c(active_edges$source, active_edges$target))
            nodes <- nodes[!is.na(nodes) & nodes != ""]
        }

        nodes_str <- if (length(nodes) > 0) paste(sort(nodes), collapse = ", ") else "None"

        # Extract Edges
        edges_df <- m_full %>%
            filter(type == "edge", status %in% c("causal", "non-causal", "unknown"))

        if (length(nodes) > 0) {
            edges_df <- edges_df %>% filter(source %in% nodes & target %in% nodes)
        }

        if (nrow(edges_df) == 0) {
            edges_str <- "  - None"
        } else {
            edges_str <- edges_df %>%
                arrange(source, target) %>%
                mutate(desc = sprintf("  - %s, %s: %s", source, target, status)) %>%
                pull(desc) %>%
                paste(collapse = "\n")
        }

        return(list(nodes = nodes_str, edges = edges_str))
    }

    # ---------------------------------------------------------
    # CASE 1: SINGLE MODEL
    # ---------------------------------------------------------
    if (length(models) == 1) {
        mod_id <- models[1]
        summary_data <- get_model_summary(mod_id)

        cat(rep("=", 60), "\n", sep = "")
        cat(sprintf("MODEL SUMMARY: %s\n", mod_id))
        cat(rep("=", 60), "\n\n", sep = "")

        cat("Nodes:", summary_data$nodes, "\n")
        cat("Edges:\n")
        cat(summary_data$edges, "\n\n")

        cat(rep("-", 60), "\n", sep = "")
        return(invisible(TRUE))
    }

    # ---------------------------------------------------------
    # CASE 2: TWO MODELS (COMPARISON)
    # ---------------------------------------------------------
    ego_id <- models[1]
    alter_id <- models[2]

    ego_summary <- get_model_summary(ego_id)
    alter_summary <- get_model_summary(alter_id)

    cat(rep("=", 60), "\n", sep = "")
    cat(sprintf("DYAD COMPARISON: %s vs %s\n", ego_id, alter_id))
    cat(rep("=", 60), "\n\n", sep = "")

    # Ego
    cat(sprintf("[ MODEL 1 (EGO): %s ]\n", ego_id))
    cat("Nodes:", ego_summary$nodes, "\n")
    cat("Edges:\n")
    cat(ego_summary$edges, "\n\n")

    cat(rep("-", 60), "\n\n", sep = "")

    # Alter
    cat(sprintf("[ MODEL 2 (ALTER): %s ]\n", alter_id))
    cat("Nodes:", alter_summary$nodes, "\n")
    cat("Edges:\n")
    cat(alter_summary$edges, "\n\n")

    # Print Metadata if dyads dataset is provided
    if (!is.null(dyads)) {
        flat_dyads <- if (is.data.frame(dyads)) dyads else bind_rows(lapply(dyads, as.data.frame))

        # Use !! to prevent dplyr from matching column names against themselves
        dyad_row <- flat_dyads %>% filter(
            (as.character(ego_id) == !!ego_id & as.character(alter_id) == !!alter_id) |
                (as.character(ego_id) == !!alter_id & as.character(alter_id) == !!ego_id) |
                as.character(dyad_id) == paste0(!!ego_id, "__", !!alter_id) |
                as.character(dyad_id) == paste0(!!alter_id, "__", !!ego_id)
        ) %>% head(1)

        if (nrow(dyad_row) > 0) {
            mas_ego_text <- if("mas_ego" %in% names(dyad_row)) dyad_row$mas_ego[1] else "N/A"
            mas_alter_text <- if("mas_alter" %in% names(dyad_row)) dyad_row$mas_alter[1] else "N/A"
            mas_compat <- if("mas_compatible" %in% names(dyad_row)) dyad_row$mas_compatible[1] else "N/A"
            id_ego <- if("identified_ego" %in% names(dyad_row)) dyad_row$identified_ego[1] else "N/A"
            id_alter <- if("identified_alter" %in% names(dyad_row)) dyad_row$identified_alter[1] else "N/A"
            id_compat <- if("identified_compatible" %in% names(dyad_row)) dyad_row$identified_compatible[1] else "N/A"

            sim_rate <- if("similarity_rate" %in% names(dyad_row)) dyad_row$similarity_rate[1] * 100 else NA
            time_comp <- if("timing_compatible" %in% names(dyad_row)) dyad_row$timing_compatible[1] else "N/A"
            exist_conf <- if("existence_conflict" %in% names(dyad_row)) dyad_row$existence_conflict[1] else "N/A"
            rep_cost <- if("repair_cost" %in% names(dyad_row)) dyad_row$repair_cost[1] else "N/A"

            cat(rep("=", 60), "\n", sep = "")
            cat("METADATA & CAUSAL METRICS\n")
            cat(rep("=", 60), "\n", sep = "")
            cat(sprintf("Structural Similarity : %.2f%%\n", sim_rate))
            cat(sprintf("Timing Compatible     : %s\n", time_comp))
            cat(sprintf("Existence Conflict    : %s\n", exist_conf))
            cat(sprintf("Repair Cost           : %s\n\n", rep_cost))

            cat("--- Causal Metrics ---\n")
            cat(sprintf("MAS Ego               : %s (Identified: %s)\n", mas_ego_text, id_ego))
            cat(sprintf("MAS Alter             : %s (Identified: %s)\n", mas_alter_text, id_alter))
            cat(sprintf("MAS Compatible        : %s\n", mas_compat))
            cat(sprintf("Identified Compatible : %s\n", id_compat))
            cat(rep("-", 60), "\n", sep = "")
        } else {
            cat(rep("=", 60), "\n", sep = "")
            cat("METADATA & CAUSAL METRICS\n")
            cat(rep("=", 60), "\n", sep = "")
            cat("No matching dyad comparison found in the provided 'dyads' dataset.\n")
            cat(rep("-", 60), "\n", sep = "")
        }
    }

    invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Alias for print_model_summary
#' @export
print_model_comparison <- print_model_summary

#' Build a dyadic compatibility matrix
#'
#' @export
build_dyad_matrix <- function(registry,
                              states,
                              mode = c("basic", "full", "single-ref", "two-stage", "symbolic"),
                              reference_id = NULL,
                              top_k = NULL,
                              exposure = NULL,
                              outcome = NULL,
                              url = getOption("theoRy.engine_url",
                                              "http://localhost:8000")) {

    mode <- match.arg(mode)
    `%||%` <- function(x, y) if (is.null(x)) y else x

    if (xor(is.null(exposure), is.null(outcome))) {
        stop("Both or neither of exposure and outcome must be provided.", call. = FALSE)
    }

    if (identical(mode, "single-ref") && is.null(reference_id)) {
        stop("reference_id is required when mode = 'single-ref'", call. = FALSE)
    }

    if (identical(mode, "two-stage")) {
        if (is.null(top_k)) top_k <- 100L
        if (!is.numeric(top_k) || top_k <= 0) {
            stop("top_k must be positive when mode = 'two-stage'", call. = FALSE)
        }
    }

    if (is.data.frame(registry)) {
        registry_df <- registry
    } else if (is.character(registry) && length(registry) == 1 && file.exists(registry)) {
        registry_df <- arrow::read_parquet(registry)
    } else {
        stop("registry must be a data frame or a path to a Parquet file.")
    }

    registry_data <- lapply(seq_len(nrow(registry_df)), function(i) {
        row <- registry_df[i, ]
        entry <- list(
            comp_id = row$comp_id,
            type = row$type,
            source = row$source,
            description = row$description
        )
        if (is.na(row$target)) entry$target <- NULL else entry$target <- row$target
        if (is.na(row$direction)) entry$direction <- NULL else entry$direction <- row$direction
        if ("fixed_status" %in% names(row) && !is.null(row$fixed_status) && !is.na(row$fixed_status)) {
            entry$fixed_status <- row$fixed_status
        }
        entry
    })

    if (is.data.frame(states)) {
        state_list <- lapply(seq_len(nrow(states)), function(i) {
            row <- states[i, ]
            entry <- list(model_id = row$model_id, comp_id = row$comp_id, status = row$status)
            if (!is.null(row$timing) && !is.na(row$timing)) entry$timing <- as.integer(row$timing)
            entry
        })
    } else {
        state_list <- states
    }

    # Resolve exposure/outcome
    if (is.null(exposure) && is.null(outcome)) {
        if (is.data.frame(states)) {
            st_exposure <- attr(states, "exposure") %||% NULL
            st_outcome <- attr(states, "outcome") %||% NULL
            if (!is.null(st_exposure) && !is.null(st_outcome)) {
                exposure <- st_exposure
                outcome <- st_outcome
            }
        }
        if (is.null(exposure)) {
            rg_exposure <- attr(registry_df, "exposure") %||% NULL
            rg_outcome <- attr(registry_df, "outcome") %||% NULL
            if (!is.null(rg_exposure) && !is.null(rg_outcome)) {
                exposure <- rg_exposure
                outcome <- rg_outcome
            }
        }
    }

    if (!is.null(exposure) && !is.null(outcome)) {
        node_names <- unique(registry_df$source[registry_df$type == "node"])
        if (!exposure %in% node_names) stop("Exposure '", exposure, "' is not in the registry node list.", call. = FALSE)
        if (!outcome %in% node_names) stop("Outcome '", outcome, "' is not in the registry node list.", call. = FALSE)
    }

    model_ids <- unique(vapply(state_list, function(x) x$model_id, character(1), USE.NAMES = FALSE))
    model_ids <- sort(model_ids)

    # Prepare base payload request
    execute_request <- function(current_mode) {
        if (identical(current_mode, "symbolic")) {
            sym_payload <- list(
                registry_data = registry_data, exposure = exposure, outcome = outcome,
                mode = "sampled", n_samples = 500L, signature_policy = "paper_v1"
            )
            httr2::request(url) |>
                httr2::req_url_path("api/v1/symbolic/query-classes") |>
                httr2::req_method("POST") |>
                httr2::req_body_json(sym_payload)
        } else {
            payload <- list(registry_data = registry_data, state_data = state_list, model_ids = I(model_ids), mode = current_mode)
            if (!is.null(reference_id)) payload$reference_id <- reference_id
            if (!is.null(top_k)) payload$top_k <- as.integer(top_k)
            if (!is.null(exposure) && !is.null(outcome)) {
                payload$exposure <- exposure
                payload$outcome <- outcome
            }
            httr2::request(url) |>
                httr2::req_url_path("api/v1/dyad-matrix") |>
                httr2::req_method("POST") |>
                httr2::req_body_json(payload)
        }
    }

    req <- execute_request(mode)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) {
        stop("Python backend not reachable at ", url, ". Start the server with start_theory_engine().", call. = FALSE)
    })

    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    # --- AUTOMATED WINDOWS RPY2 FALLBACK BACKEND EXCEPTION INTERCEPTOR ---
    if (identical(body$status, "error") &&
        grepl("rpy2|R_getVar|R.dll", body$message) &&
        mode %in% c("full", "two-stage")) {

        warning(
            "Windows environment issue detected linking rpy2/R.dll dependencies.\n",
            "Automatically falling back to structural matrix comparison mode safely.",
            call. = FALSE
        )

        # Retry the request cleanly using basic mode structural fallback
        req <- execute_request("basic")
        resp <- httr2::req_perform(req)
        body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
        mode <- "basic"
    }

    if (identical(body$status, "error")) {
        stop("Backend error [", body$code, "]: ", body$message, call. = FALSE)
    }

    if (identical(mode, "symbolic")) {
        result <- body$data
        class(result) <- c("theory_symbolic_classes", "list")
        return(result)
    }

    if (identical(mode, "two-stage")) {
        dc_df <- .parse_dyads_to_df(body$data$detailed_comparisons, full = TRUE)
        return(list(heatmap_summary = body$data$heatmap_summary, detailed_comparisons = dc_df))
    }

    result <- .parse_dyads_to_df(body$data$dyads, full = identical(mode, "full"))
    attr(result, "theory_context") <- list(
        registry_data = registry_data, state_data = state_list,
        model_ids = model_ids, mode = mode, exposure = exposure, outcome = outcome
    )
    return(result)
}


.parse_dyads_to_df <- function(dyads, full = FALSE) {
    base_fields <- list(
        dyad_id = vapply(dyads, function(d) d$dyad_id, character(1), USE.NAMES = FALSE),
        ego_id = vapply(dyads, function(d) d$ego_id, character(1), USE.NAMES = FALSE),
        alter_id = vapply(dyads, function(d) d$alter_id, character(1), USE.NAMES = FALSE),
        similarity_rate = vapply(dyads, function(d) d$similarity_rate, numeric(1), USE.NAMES = FALSE),
        timing_compatible = vapply(dyads, function(d) d$timing_compatible, logical(1), USE.NAMES = FALSE),
        existence_conflict = vapply(dyads, function(d) d$existence_conflict, logical(1), USE.NAMES = FALSE),
        repair_cost = vapply(dyads, function(d) d$repair_cost, integer(1), USE.NAMES = FALSE)
    )

    if (!full) {
        return(data.frame(base_fields, stringsAsFactors = FALSE))
    }

    normalize_mas <- function(x) {
        if (is.null(x)) {
            return(NULL)
        }
        lapply(x, function(set) {
            if (is.null(set) || length(set) == 0) {
                character(0)
            } else {
                as.character(unlist(set, use.names = FALSE))
            }
        })
    }

    mas_ego_col <- lapply(dyads, function(d) {
        normalize_mas(d$mas_ego)
    })
    mas_alter_col <- lapply(dyads, function(d) {
        normalize_mas(d$mas_alter)
    })

    data.frame(
        base_fields,
        mas_ego = I(mas_ego_col),
        mas_alter = I(mas_alter_col),
        mas_compatible = vapply(dyads, function(d) {
            if (is.null(d$mas_compatible)) NA else d$mas_compatible
        }, logical(1), USE.NAMES = FALSE),
        identified_ego = vapply(dyads, function(d) {
            if (is.null(d$identified_ego)) NA else d$identified_ego
        }, logical(1), USE.NAMES = FALSE),
        identified_alter = vapply(dyads, function(d) {
            if (is.null(d$identified_alter)) NA else d$identified_alter
        }, logical(1), USE.NAMES = FALSE),
        identified_compatible = vapply(dyads, function(d) {
            if (is.null(d$identified_compatible)) NA else d$identified_compatible
        }, logical(1), USE.NAMES = FALSE),
        stringsAsFactors = FALSE
    )
}

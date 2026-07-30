#' Build a component registry from a causal theory specification
#'
#' Builds a registry programmatically from named variables and their temporal
#' positions. An exposure and outcome are always required; their directed path
#' is fixed as causal in every generated model. Use
#' \code{build_component_registry_interactive()} for a guided alternative.
#'
#' @param nodes A character vector of variable names, or a data frame with
#'   columns \code{name}, \code{timing} (integer), and optionally
#'   \code{description}.
#' @param timing Integer vector of fixed chronological positions parallel to
#'   \code{nodes}. Use \code{NA} for a non-focal node whose possible positions
#'   are supplied by \code{time_points}. Ignored when \code{nodes} is a data
#'   frame.
#' @param descriptions Optional character vector of descriptions parallel to
#'   \code{nodes}. Ignored when \code{nodes} is a data frame.
#' @param respect_timing When \code{TRUE} (default), directed candidates must
#'   be temporally possible.
#' @param include_bidirectional When \code{TRUE}, generate candidate
#'   bidirected edges for every unordered node pair. Prefer explicit
#'   \code{<->} \code{"allow"} constraints for selected possible confounding,
#'   or \code{"require"} constraints to enforce named confounding pairs.
#' @param constraints Optional data frame or list with \code{source},
#'   \code{target}, \code{direction}, and \code{rule}. Directed
#'   and bidirected \code{"require"} edges are fixed causal paths or
#'   confounding pairs, \code{"forbid"} removes a path or pair, and
#'   \code{"allow"} permits a selected candidate.
#' @param exposure Name of the focal exposure. Must be a node with exactly one
#'   allowed time.
#' @param outcome Name of the focal outcome. Must be a node with exactly one
#'   allowed time later than \code{exposure}.
#' @param time_points Finite integer vector available to non-focal nodes whose
#'   \code{timing} is \code{NA}. Required whenever timing is unspecified.
#' @param timing_options Optional named list of allowed integer positions. It
#'   overrides \code{timing} for named nodes. Each model chooses one position
#'   from each present node's options.
#' @param optional_nodes Node names that may be absent in subset models.
#'   Exposure, outcome, and endpoints of required paths cannot be optional.
#' @param url Base URL of the theoRy Python backend.
#' @param output_path Optional Parquet path for the registry table. Timing and
#'   theory metadata are R attributes and are not retained by Parquet.
#'
#' @return A registry data frame. It retains \code{exposure}, \code{outcome},
#'   \code{node_timing}, \code{timing_options}, \code{optional_nodes}, and
#'   \code{constraints} attributes for downstream expansion.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' registry <- build_component_registry(
#'   nodes = c("Education", "Income", "Health"),
#'   timing = c(1, 2, 3),
#'   exposure = "Education",
#'   outcome = "Health"
#' )
#' }
#'
#' @export
build_component_registry <- function(nodes,
                                     timing = NULL,
                                     descriptions = NULL,
                                     respect_timing = TRUE,
                                     include_bidirectional = FALSE,
                                     constraints = NULL,
                                     exposure = NULL,
                                     outcome = NULL,
                                     time_points = NULL,
                                     timing_options = NULL,
                                     optional_nodes = character(),
                                     url = getOption("theoRy.engine_url",
                                                     "http://localhost:8000"),
                                     output_path = NULL) {
  spec <- .registry_node_spec(nodes, timing, descriptions)
  node_names <- spec$names

  if (is.null(exposure) || is.null(outcome)) {
    stop("exposure and outcome must both be supplied.", call. = FALSE)
  }
  if (!is.character(exposure) || length(exposure) != 1L ||
      !is.character(outcome) || length(outcome) != 1L ||
      !nzchar(exposure) || !nzchar(outcome)) {
    stop("exposure and outcome must each be one non-empty node name.",
         call. = FALSE)
  }
  if (identical(exposure, outcome)) {
    stop("Exposure and outcome must be distinct nodes.", call. = FALSE)
  }
  if (!exposure %in% node_names || !outcome %in% node_names) {
    stop("Exposure and outcome must be names in nodes.", call. = FALSE)
  }

  option_map <- .registry_timing_options(
    node_names, spec$timing, time_points, timing_options
  )
  if (length(option_map[[exposure]]) != 1L ||
      length(option_map[[outcome]]) != 1L) {
    stop("exposure and outcome must each have exactly one allowed time.",
         call. = FALSE)
  }
  if (option_map[[exposure]][[1]] >= option_map[[outcome]][[1]]) {
    stop("exposure must occur before outcome.", call. = FALSE)
  }

  constraint_list <- .registry_constraints(constraints, node_names)
  focal <- list(
    source = exposure,
    target = outcome,
    direction = "->",
    rule = "require"
  )
  focal_forbidden <- vapply(constraint_list, function(x) {
    identical(x$source, exposure) && identical(x$target, outcome) &&
      identical(x$direction, "->") && identical(x$rule, "forbid")
  }, logical(1))
  if (any(focal_forbidden)) {
    stop("The focal exposure -> outcome path cannot be forbidden.",
         call. = FALSE)
  }
  focal_required <- vapply(constraint_list, function(x) {
    identical(x$source, exposure) && identical(x$target, outcome) &&
      identical(x$direction, "->") && identical(x$rule, "require")
  }, logical(1))
  if (!any(focal_required)) {
    constraint_list <- c(constraint_list, list(focal))
  }

  optional_nodes <- as.character(optional_nodes)
  if (anyNA(optional_nodes) || any(!nzchar(optional_nodes)) ||
      anyDuplicated(optional_nodes)) {
    stop("optional_nodes must contain unique non-empty node names.",
         call. = FALSE)
  }
  unknown_optional <- setdiff(optional_nodes, node_names)
  if (length(unknown_optional)) {
    stop("optional_nodes contains unknown node(s): ",
         paste(unknown_optional, collapse = ", "), call. = FALSE)
  }
  required_endpoints <- unique(unlist(lapply(constraint_list, function(x) {
    if (identical(x$rule, "require")) {
      c(x$source, x$target)
    } else {
      character()
    }
  })))
  blocked_optional <- intersect(optional_nodes, required_endpoints)
  if (length(blocked_optional)) {
    stop("optional_nodes cannot contain exposure, outcome, or endpoints of ",
         "required paths: ", paste(blocked_optional, collapse = ", "),
         call. = FALSE)
  }

  node_specs <- lapply(node_names, function(name) {
    options <- option_map[[name]]
    list(
      name = name,
      timing = if (length(options) == 1L) options[[1]] else NULL,
      timing_options = unname(as.list(options)),
      description = spec$descriptions[[name]]
    )
  })
  payload <- list(
    nodes = node_specs,
    respect_timing = isTRUE(respect_timing),
    include_bidirectional = isTRUE(include_bidirectional),
    constraints = constraint_list,
    exposure = exposure,
    outcome = outcome
  )

  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_url_path("api/v1/component-registry") |>
      httr2::req_method("POST") |>
      httr2::req_body_json(payload) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      stop("Python backend not reachable at ", url,
           ". Start the server with start_theory_engine().", call. = FALSE)
    }
  )
  body <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = FALSE),
    error = function(e) {
      stop("Invalid backend response: expected JSON.", call. = FALSE)
    }
  )
  if (identical(body$status, "error")) {
    code <- if (is.null(body$code)) "UNKNOWN" else body$code
    message <- if (is.null(body$message)) "Unknown backend error" else body$message
    stop("Backend error [", code, "]: ", message, call. = FALSE)
  }
  if (!identical(body$status, "success") || is.null(body$data$registry_data)) {
    stop("Invalid backend response: missing registry_data.", call. = FALSE)
  }

  registry <- records_to_df(
    body$data$registry_data,
    col_types = c(target = "character", direction = "character",
                  fixed_status = "character")
  )
  fixed_timing <- vapply(option_map, function(x) {
    if (length(x) == 1L) x[[1]] else NA_integer_
  }, integer(1))
  attr(registry, "node_timing") <- fixed_timing[!is.na(fixed_timing)]
  attr(registry, "timing_options") <- option_map
  attr(registry, "time_points") <- sort(unique(unlist(option_map)))
  attr(registry, "optional_nodes") <- optional_nodes
  attr(registry, "constraints") <- constraint_list
  attr(registry, "exposure") <- exposure
  attr(registry, "outcome") <- outcome
  rownames(registry) <- NULL

  if (!is.null(output_path)) {
    warning("Parquet output does not retain exposure/outcome, timing, or ",
            "constraint attributes; supply them explicitly when reloading ",
            "the registry.", call. = FALSE)
    arrow::write_parquet(registry, output_path)
  }
  registry
}


#' Build a component registry through a guided questionnaire
#'
#' Collects named variables, a required exposure/outcome pair, chronological
#' positions, theory constraints, required or possible latent confounding, and
#' optional nodes. The answers are validated and delegated to
#' \code{build_component_registry()}, so interactive and programmatic results
#' share the same backend contract.
#'
#' @param url Base URL of the theoRy Python backend.
#' @param input Function used to collect one answer. Defaults to
#'   \code{readline()} and can be replaced for automated use.
#'
#' @return A component registry data frame.
#'
#' @export
build_component_registry_interactive <- function(
    url = getOption("theoRy.engine_url", "http://localhost:8000"),
    input = readline) {
  if (!is.function(input)) {
    stop("input must be a function accepting one prompt string.", call. = FALSE)
  }
  ask <- function(prompt) trimws(as.character(input(prompt)))

  raw_nodes <- ask("Variable names (comma-separated): ")
  nodes <- trimws(strsplit(raw_nodes, ",", fixed = TRUE)[[1]])
  if (!length(nodes) || any(!nzchar(nodes)) || anyDuplicated(nodes)) {
    stop("Enter unique, non-empty comma-separated variable names.", call. = FALSE)
  }
  exposure <- ask("Exposure variable: ")
  outcome <- ask("Outcome variable: ")
  if (!exposure %in% nodes || !outcome %in% nodes || identical(exposure, outcome)) {
    stop("Exposure and outcome must be distinct variables from the node list.",
         call. = FALSE)
  }

  timing_options <- stats::setNames(vector("list", length(nodes)), nodes)
  multi_time_nodes <- character()
  for (name in nodes) {
    values <- .registry_parse_times(ask(paste0("Time for ", name,
                                              " (one integer; up to two for non-focal nodes): ")))
    if (name %in% c(exposure, outcome) && length(values) != 1L) {
      stop("Exposure and outcome must each have exactly one time.", call. = FALSE)
    }
    if (length(values) > 2L) {
      stop("A variable may have at most two allowed times.", call. = FALSE)
    }
    if (length(values) == 2L) {
      multi_time_nodes <- c(multi_time_nodes, name)
    }
    timing_options[[name]] <- values
  }
  if (length(multi_time_nodes) > 2L) {
    stop("Interactive mode permits two variables with two allowed times.",
         call. = FALSE)
  }

  required_pairs <- .registry_parse_pairs(
    ask("Required directed paths, e.g. (A,B),(B,C), or Enter: "), nodes
  )
  forbidden_pairs <- .registry_parse_pairs(
    ask("Forbidden directed paths, e.g. (A,B), or Enter: "), nodes
  )
  required_confounded_pairs <- .registry_parse_pairs(
    ask("Required unmeasured-confounding pairs, e.g. (A,B),(C,D), or Enter: "),
    nodes
  )
  confounded_pairs <- .registry_parse_pairs(
    ask("Possible unmeasured-confounding pairs, e.g. (A,B), or Enter: "), nodes
  )
  optional_raw <- ask("Variables allowed to be absent, comma-separated, or Enter: ")
  optional_nodes <- if (nzchar(optional_raw)) {
    trimws(strsplit(optional_raw, ",", fixed = TRUE)[[1]])
  } else {
    character()
  }

  constraints <- c(
    lapply(required_pairs, function(pair) {
      list(source = pair[[1]], target = pair[[2]], direction = "->", rule = "require")
    }),
    lapply(forbidden_pairs, function(pair) {
      list(source = pair[[1]], target = pair[[2]], direction = "->", rule = "forbid")
    }),
    lapply(required_confounded_pairs, function(pair) {
      list(source = pair[[1]], target = pair[[2]], direction = "<->", rule = "require")
    }),
    lapply(confounded_pairs, function(pair) {
      list(source = pair[[1]], target = pair[[2]], direction = "<->", rule = "allow")
    })
  )

  cat("\n--- theoRy parameters ---\n")
  cat("Exposure:", exposure, "\nOutcome:", outcome, "\n")
  for (name in nodes) {
    cat(name, ":", paste(timing_options[[name]], collapse = ", "), "\n")
  }
  cat("Required paths:", .registry_format_pairs(required_pairs, " -> "), "\n")
  cat("Forbidden paths:", .registry_format_pairs(forbidden_pairs, " !-> "), "\n")
  cat("Required confounding:",
      .registry_format_pairs(required_confounded_pairs, " <-> "), "\n")
  cat("Possible confounding:", .registry_format_pairs(confounded_pairs, " <-> "), "\n")
  cat("Optional nodes:", if (length(optional_nodes)) {
    paste(optional_nodes, collapse = ", ")
  } else "none", "\n")
  cat("The focal path", exposure, "->", outcome, "will be fixed as causal.\n")
  confirmation <- tolower(ask("Build this registry? [y/N]: "))
  if (!confirmation %in% c("y", "yes")) {
    stop("Registry creation cancelled.", call. = FALSE)
  }

  timing <- vapply(timing_options, function(x) {
    if (length(x) == 1L) x[[1]] else NA_integer_
  }, integer(1))
  build_component_registry(
    nodes = nodes,
    timing = timing,
    timing_options = timing_options,
    constraints = constraints,
    exposure = exposure,
    outcome = outcome,
    optional_nodes = optional_nodes,
    url = url
  )
}


.registry_node_spec <- function(nodes, timing, descriptions) {
  if (is.data.frame(nodes)) {
    if (!"name" %in% names(nodes)) {
      stop("nodes data frame must contain a name column.", call. = FALSE)
    }
    names <- as.character(nodes$name)
    timing <- if ("timing" %in% names(nodes)) nodes$timing else NULL
    descriptions <- if ("description" %in% names(nodes)) nodes$description else NULL
  } else {
    names <- as.character(nodes)
  }
  if (!length(names)) {
    stop("At least one node is required.", call. = FALSE)
  }
  if (anyNA(names) || any(!nzchar(names)) || anyDuplicated(names)) {
    stop("nodes must contain unique, non-empty names.", call. = FALSE)
  }
  if (is.null(timing)) {
    timing <- rep(NA_integer_, length(names))
  }
  if (length(timing) != length(names) ||
      any(!is.na(timing) & (!is.finite(timing) | timing != as.integer(timing)))) {
    stop("timing must be an integer vector parallel to nodes, with NA allowed.",
         call. = FALSE)
  }
  if (is.null(descriptions)) {
    descriptions <- rep(NA_character_, length(names))
  }
  if (length(descriptions) != length(names)) {
    stop("descriptions must be parallel to nodes.", call. = FALSE)
  }
  list(
    names = names,
    timing = as.integer(timing),
    descriptions = stats::setNames(as.character(descriptions), names)
  )
}


.registry_timing_options <- function(node_names, timing, time_points, timing_options) {
  if (!is.null(time_points)) {
    if (!is.numeric(time_points) || !length(time_points) || anyNA(time_points) ||
        any(!is.finite(time_points)) || any(time_points != as.integer(time_points))) {
      stop("time_points must be a non-empty integer vector.", call. = FALSE)
    }
    time_points <- sort(unique(as.integer(time_points)))
  }
  supplied <- stats::setNames(vector("list", length(node_names)), node_names)
  if (!is.null(timing_options)) {
    if (!is.list(timing_options) || is.null(names(timing_options))) {
      stop("timing_options must be a named list.", call. = FALSE)
    }
    unknown <- setdiff(names(timing_options), node_names)
    if (length(unknown)) {
      stop("timing_options contains unknown node(s): ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    for (name in names(timing_options)) {
      values <- timing_options[[name]]
      if (!is.numeric(values) || !length(values) || anyNA(values) ||
          any(!is.finite(values)) || any(values != as.integer(values))) {
        stop("timing_options for ", name, " must be non-empty integers.",
             call. = FALSE)
      }
      supplied[[name]] <- sort(unique(as.integer(values)))
    }
  }
  for (i in seq_along(node_names)) {
    name <- node_names[[i]]
    if (!is.null(supplied[[name]])) {
      next
    }
    if (!is.na(timing[[i]])) {
      supplied[[name]] <- as.integer(timing[[i]])
    } else {
      if (is.null(time_points)) {
        stop("time_points is required when timing is unspecified for ", name,
             ".", call. = FALSE)
      }
      supplied[[name]] <- time_points
    }
  }
  supplied
}


.registry_constraints <- function(constraints, node_names) {
  if (is.null(constraints)) {
    return(list())
  }
  rows <- if (is.data.frame(constraints)) {
    required <- c("source", "target", "direction", "rule")
    missing <- setdiff(required, names(constraints))
    if (length(missing)) {
      stop("constraints is missing column(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    lapply(seq_len(nrow(constraints)), function(i) as.list(constraints[i, required]))
  } else if (is.list(constraints) && all(c("source", "target") %in% names(constraints))) {
    list(constraints)
  } else if (is.list(constraints)) {
    constraints
  } else {
    stop("constraints must be a data frame or list.", call. = FALSE)
  }
  lapply(rows, function(row) {
    if (!is.list(row) || !all(c("source", "target", "rule") %in% names(row))) {
      stop("Each constraint needs source, target, and rule.", call. = FALSE)
    }
    source <- as.character(row$source)
    target <- as.character(row$target)
    direction <- if (is.null(row$direction)) "->" else as.character(row$direction)
    rule <- as.character(row$rule)
    if (length(source) != 1L || length(target) != 1L ||
        !source %in% node_names || !target %in% node_names || identical(source, target)) {
      stop("Constraints must reference distinct known nodes.", call. = FALSE)
    }
    if (!direction %in% c("->", "<->") || !rule %in% c("allow", "forbid", "require")) {
      stop("Constraint direction must be -> or <-> and rule must be allow, ",
           "forbid, or require.", call. = FALSE)
    }
    if (identical(direction, "<->")) {
      pair <- sort(c(source, target))
      source <- pair[[1]]
      target <- pair[[2]]
    }
    list(source = source, target = target, direction = direction, rule = rule)
  })
}


.registry_parse_times <- function(value) {
  pieces <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (!length(pieces) || any(!grepl("^-?[0-9]+$", pieces))) {
    stop("Times must be one or two comma-separated integers.", call. = FALSE)
  }
  sort(unique(as.integer(pieces)))
}


.registry_parse_pairs <- function(value, node_names) {
  if (!nzchar(value)) {
    return(list())
  }
  matches <- regmatches(value, gregexpr("\\([^()]+\\)", value))[[1]]
  if (!length(matches)) {
    stop("Pairs must use the form (A,B),(C,D).", call. = FALSE)
  }
  lapply(matches, function(match) {
    parts <- trimws(strsplit(substr(match, 2L, nchar(match) - 1L), ",", fixed = TRUE)[[1]])
    if (length(parts) != 2L || any(!parts %in% node_names) || identical(parts[[1]], parts[[2]])) {
      stop("Each pair must contain two distinct listed variables.", call. = FALSE)
    }
    parts
  })
}


.registry_format_pairs <- function(pairs, separator) {
  if (!length(pairs)) {
    return("none")
  }
  paste(vapply(pairs, function(pair) paste(pair, collapse = separator), character(1)),
        collapse = ", ")
}

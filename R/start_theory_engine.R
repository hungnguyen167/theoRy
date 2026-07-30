.theory_engine_state <- new.env(parent = emptyenv())
.theory_engine_state$processes <- list()


.theory_engine_target <- function(host, port, timeout) {
  if (!is.character(host) || length(host) != 1L || is.na(host) || !nzchar(host)) {
    stop("`host` must be one non-empty string.", call. = FALSE)
  }
  if (!is.numeric(port) || length(port) != 1L || is.na(port) ||
      !is.finite(port) || port != as.integer(port) || port < 1 || port > 65535) {
    stop("`port` must be an integer between 1 and 65535.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout < 0) {
    stop("`timeout` must be a non-negative number of seconds.", call. = FALSE)
  }

  list(host = host, port = as.integer(port), timeout = as.numeric(timeout))
}


.theory_engine_key <- function(host, port) {
  paste(host, port, sep = ":")
}


.theory_engine_url <- function(host, port, path = "") {
  address <- host
  if (grepl(":", address, fixed = TRUE) &&
      !startsWith(address, "[")) {
    address <- paste0("[", address, "]")
  }
  sprintf("http://%s:%d%s", address, port, path)
}


.theory_engine_python <- function(python) {
  if (!is.character(python) || length(python) != 1L || is.na(python) ||
      !nzchar(python)) {
    stop("`python` must be one non-empty executable path or command.", call. = FALSE)
  }

  is_path <- grepl("[/\\\\]", python)
  executable <- if (is_path) python else Sys.which(python)
  if (!nzchar(executable) || !file.exists(executable)) {
    stop("Python executable not found: ", python, call. = FALSE)
  }
  if (file.access(executable, mode = 1) != 0L) {
    stop("Python executable is not executable: ", executable, call. = FALSE)
  }

  normalizePath(executable, winslash = "/", mustWork = TRUE)
}


.theory_engine_environment <- function(engine_dir) {
  env <- Sys.getenv()
  pythonpath <- Sys.getenv("PYTHONPATH", unset = "")
  env[["PYTHONPATH"]] <- if (nzchar(pythonpath)) {
    paste(engine_dir, pythonpath, sep = .Platform$path.sep)
  } else {
    engine_dir
  }
  env
}


.theory_engine_get <- function(key) {
  .theory_engine_state$processes[[key]]
}


.theory_engine_set <- function(key, process) {
  .theory_engine_state$processes[[key]] <- process
  invisible(process)
}


.theory_engine_clear <- function(key, remove_logs = TRUE) {
  process <- .theory_engine_get(key)
  if (!is.null(process) && remove_logs) {
    logs <- c(process$stdout_log, process$stderr_log)
    logs <- logs[!is.na(logs) & nzchar(logs)]
    unlink(logs)
  }
  .theory_engine_state$processes[[key]] <- NULL
  invisible(NULL)
}


.theory_engine_is_alive <- function(process) {
  !is.null(process) && isTRUE(tryCatch(
    process$process$is_alive(),
    error = function(e) FALSE
  ))
}


.theory_engine_is_healthy <- function(base, timeout = 2) {
  health <- tryCatch(
    httr2::request(paste0(base, "/api/v1/health")) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )
  !is.null(health) && identical(health$status, "success")
}


.theory_engine_is_compatible <- function(base, timeout = 2) {
  if (!.theory_engine_is_healthy(base, timeout)) {
    return(FALSE)
  }

  openapi <- tryCatch(
    httr2::request(paste0(base, "/openapi.json")) |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )
  required <- c(
    "/api/v1/component-registry",
    "/api/v1/model-states",
    "/api/v1/dyad-matrix"
  )
  !is.null(openapi) && all(required %in% names(openapi$paths))
}


.theory_engine_wait_for_exit <- function(process, timeout) {
  deadline <- Sys.time() + timeout
  while (.theory_engine_is_alive(process) && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }
  !.theory_engine_is_alive(process)
}


.theory_engine_request_shutdown <- function(base, timeout) {
  if (timeout <= 0 || !.theory_engine_is_healthy(base, timeout = min(2, timeout))) {
    return(FALSE)
  }
  tryCatch({
    httr2::request(paste0(base, "/api/v1/shutdown")) |>
      httr2::req_method("POST") |>
      httr2::req_timeout(min(2, timeout)) |>
      httr2::req_perform()
    TRUE
  }, error = function(e) FALSE)
}


.theory_engine_wait_for_shutdown <- function(base, timeout) {
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (!.theory_engine_is_healthy(base, timeout = 1)) {
      return(TRUE)
    }
    Sys.sleep(0.05)
  }
  !.theory_engine_is_healthy(base, timeout = 1)
}


.theory_engine_stop_process <- function(process) {
  if (!.theory_engine_is_alive(process)) {
    return(TRUE)
  }

  tryCatch(process$process$kill(), error = function(e) NULL)
  .theory_engine_wait_for_exit(process, timeout = 2)
}


.theory_engine_startup_logs <- function(process) {
  read_log <- function(path) {
    if (is.na(path) || !nzchar(path) || !file.exists(path)) {
      return(character())
    }
    tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  }

  stdout <- read_log(process$stdout_log)
  stderr <- read_log(process$stderr_log)
  logs <- c(
    if (length(stdout)) c("stdout:", stdout),
    if (length(stderr)) c("stderr:", stderr)
  )
  if (!length(logs)) {
    return("No startup output was captured.")
  }
  paste(utils::tail(logs, 100L), collapse = "\n")
}


.theory_engine_wait_for_ready <- function(process, base, timeout) {
  deadline <- Sys.time() + timeout
  repeat {
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (!.theory_engine_is_alive(process) || remaining <= 0) {
      return(FALSE)
    }
    if (.theory_engine_is_healthy(base, timeout = min(1, remaining))) {
      return(TRUE)
    }
    Sys.sleep(0.1)
  }
}


.theory_engine_startup_failed <- function(key, process, base, timeout) {
  .theory_engine_stop_process(process)
  logs <- .theory_engine_startup_logs(process)
  .theory_engine_clear(key)
  stop(
    "Theory engine did not start at ", base, " within ", timeout,
    " seconds. Startup logs:\n", logs,
    call. = FALSE
  )
}


#' Start the theoRy Python backend engine
#'
#' Launches the Python FastAPI server and blocks until the health endpoint
#' responds. The process is owned by the current R session and can be stopped
#' with [stop_theory_engine()]. A compatible backend started outside this R
#' session is left running.
#'
#' @param port Port number. Defaults to \code{8000}.
#' @param host Host address. Defaults to \code{"127.0.0.1"}.
#' @param timeout Maximum seconds to wait for the server to become ready.
#'   Defaults to 30.
#' @param python Path to the Python interpreter to use. Defaults to
#'   \code{"python"} on the system \code{PATH}.
#'
#' @return Invisibly returns \code{TRUE} when the server is ready.
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' }
#'
#' @export
start_theory_engine <- function(port = 8000L,
                                host = "127.0.0.1",
                                timeout = 30,
                                python = "python") {
  target <- .theory_engine_target(host, port, timeout)
  key <- .theory_engine_key(target$host, target$port)
  base <- .theory_engine_url(target$host, target$port)
  process <- .theory_engine_get(key)

  if (!is.null(process)) {
    if (.theory_engine_is_alive(process)) {
      if (.theory_engine_wait_for_ready(process, base, target$timeout)) {
        message("Theory engine already running at ", base,
                " (managed by this R session).")
        return(invisible(TRUE))
      }
      .theory_engine_startup_failed(key, process, base, target$timeout)
    }
    .theory_engine_clear(key)
  }

  if (.theory_engine_is_healthy(base, timeout = 2)) {
    if (.theory_engine_is_compatible(base, timeout = 2)) {
      message("Theory engine already running at ", base,
              " (externally managed; leaving it running).")
      return(invisible(TRUE))
    }
    stop(
      "A service at ", base,
      " is not a compatible theoRy engine. It was not stopped.",
      call. = FALSE
    )
  }

  engine_dir <- system.file("python", package = "theoRy")
  if (!nzchar(engine_dir) || !dir.exists(engine_dir)) {
    stop("Python submodule not found. Expected: inst/python/ under the package root.",
         call. = FALSE)
  }
  executable <- .theory_engine_python(python)
  stdout_log <- tempfile("theory-engine-stdout-", fileext = ".log")
  stderr_log <- tempfile("theory-engine-stderr-", fileext = ".log")

  process <- tryCatch(
    list(
      process = processx::process$new(
        command = executable,
        args = c(
          "-m", "uvicorn", "api.main:app",
          "--host", target$host,
          "--port", as.character(target$port)
        ),
        wd = engine_dir,
        env = .theory_engine_environment(engine_dir),
        stdout = stdout_log,
        stderr = stderr_log,
        cleanup_tree = TRUE
      ),
      stdout_log = stdout_log,
      stderr_log = stderr_log
    ),
    error = function(e) {
      unlink(c(stdout_log, stderr_log))
      stop("Could not start Python executable '", executable, "': ",
           conditionMessage(e), call. = FALSE)
    }
  )
  .theory_engine_set(key, process)

  message("Starting theory engine at ", base, " ...")
  if (!.theory_engine_wait_for_ready(process, base, target$timeout)) {
    .theory_engine_startup_failed(key, process, base, target$timeout)
  }

  message("Theory engine ready.")
  invisible(TRUE)
}


#' Stop the theoRy Python backend engine
#'
#' Sends a graceful shutdown request to an engine started by the current R
#' session. Set \code{stop_external = TRUE} to request graceful shutdown of a
#' compatible externally managed theoRy engine. This never kills a process by
#' port, so an unresponsive external engine remains protected.
#'
#' @param port Port number. Defaults to \code{8000}.
#' @param host Host address. Defaults to \code{"127.0.0.1"}.
#' @param timeout Maximum seconds to wait for graceful shutdown.
#'   Defaults to 3.
#' @param stop_external Whether to also request graceful shutdown of a
#'   compatible engine not launched by this R session. Defaults to
#'   \code{FALSE}.
#'
#' @return Invisibly returns \code{TRUE} if the managed engine was stopped
#'   (or no managed engine was running).
#'
#' @examples
#' \dontrun{
#' start_theory_engine()
#' # ... use theoRy functions ...
#' stop_theory_engine()
#' }
#'
#' @export
stop_theory_engine <- function(port = 8000L,
                               host = "127.0.0.1",
                               timeout = 3,
                               stop_external = FALSE) {
  if (!is.logical(stop_external) || length(stop_external) != 1L ||
      is.na(stop_external)) {
    stop("`stop_external` must be TRUE or FALSE.", call. = FALSE)
  }
  target <- .theory_engine_target(host, port, timeout)
  key <- .theory_engine_key(target$host, target$port)
  base <- .theory_engine_url(target$host, target$port)
  process <- .theory_engine_get(key)

  if (is.null(process)) {
    if (.theory_engine_is_compatible(base, timeout = 2)) {
      if (!isTRUE(stop_external)) {
        message("Theory engine at ", base,
                " is externally managed; leaving it running.")
        return(invisible(TRUE))
      }
      if (!.theory_engine_request_shutdown(base, target$timeout)) {
        stop("Could not request graceful shutdown from the externally managed ",
             "theoRy engine at ", base, ". It was left running.", call. = FALSE)
      }
      if (!.theory_engine_wait_for_shutdown(base, target$timeout)) {
        stop("The externally managed theoRy engine at ", base,
             " accepted no shutdown within ", target$timeout,
             " seconds. It was not force-killed.", call. = FALSE)
      }
      message("Externally managed theory engine stopped gracefully.")
    } else {
      message("No theory engine managed by this R session is running at ", base,
               ".")
    }
    return(invisible(TRUE))
  }

  if (!.theory_engine_is_alive(process)) {
    .theory_engine_clear(key)
    message("The managed theory engine is no longer running.")
    return(invisible(TRUE))
  }

  shutdown_ok <- .theory_engine_request_shutdown(base, target$timeout)

  stopped <- .theory_engine_wait_for_exit(process, target$timeout)
  if (!stopped) {
    stopped <- .theory_engine_stop_process(process)
  }
  if (!stopped) {
    stop("Managed theory engine at ", base, " could not be stopped.", call. = FALSE)
  }

  .theory_engine_clear(key)
  if (shutdown_ok) {
    message("Theory engine stopped gracefully.")
  } else {
    message("Theory engine stopped.")
  }
  invisible(TRUE)
}

# Internal helper: kill any process bound to the given port
kill_port <- function(port, host = "127.0.0.1") {
  if (.Platform$OS.type == "windows") {
    # netstat -ano | findstr :<port> then taskkill /PID ...
    out <- suppressWarnings(
      system(sprintf('netstat -ano | findstr ":%d"', port), intern = TRUE)
    )
    if (length(out) == 0) return(invisible(FALSE))
    pids <- character(0)
    for (line in out) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) >= 5 && grepl("LISTENING", parts[[4]], ignore.case = TRUE)) {
        pids <- c(pids, parts[[5]])
      }
    }
    if (length(pids) == 0) return(invisible(FALSE))
    for (pid in unique(pids)) {
      suppressWarnings(system(sprintf("taskkill /F /PID %s", pid),
                              ignore.stdout = TRUE, ignore.stderr = TRUE))
    }
  } else {
    suppressWarnings(
      system(sprintf("fuser -k %d/tcp 2>/dev/null", port),
             ignore.stdout = TRUE, ignore.stderr = TRUE)
    )
  }
  invisible(TRUE)
}


#' Start the theoRy Python backend engine
#'
#' Launches the Python FastAPI server in the background and blocks until
#' the health endpoint responds.  Call this once per R session before
#' using any of the other theoRy functions that require the backend.
#'
#' @param port Port number.  Defaults to \code{8000}.
#' @param host Host address.  Defaults to \code{"127.0.0.1"}.
#' @param timeout Maximum seconds to wait for the server to become
#'   ready.  Defaults to 30.
#' @param python Path to the Python interpreter to use.  Defaults to
#'   \code{"python"} (on the system \code{PATH}).
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
  url <- sprintf("http://%s:%d/api/v1/health", host, port)

  libstdcpp <- "/usr/lib/x86_64-linux-gnu/libstdc++.so.6"
  ld_preload <- ""
  if (.Platform$OS.type == "unix" && file.exists(libstdcpp)) {
    ld_preload <- sprintf("LD_PRELOAD=%s ", libstdcpp)
  }

  # Already running?
  alive <- tryCatch(
    httr2::request(url) |>
      httr2::req_timeout(2) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )
  if (!is.null(alive) && identical(alive$status, "success")) {
    # verify required endpoints exist (not a stale backend)
    ep <- tryCatch(
      httr2::request(sprintf("http://%s:%d/openapi.json", host, port)) |>
        httr2::req_timeout(2) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) NULL
    )
    required <- c(
      "/api/v1/component-registry",
      "/api/v1/model-states",
      "/api/v1/dyad-matrix"
    )
    if (!is.null(ep) && all(required %in% names(ep$paths))) {
      if (nzchar(ld_preload)) {
        message("Theory engine already running on ", url,
                "; restarting to apply R/rpy2 library preload...")
        stop_theory_engine(port, host)
        Sys.sleep(1)
      } else {
        message("Theory engine already running on ", url)
        return(invisible(TRUE))
      }
    } else {
      message("Stale backend detected (missing new endpoints). Stopping old backend...")
      stop_theory_engine(port, host)
      Sys.sleep(1)
    }
  }

  pkg_dir <- system.file("python", package = "theoRy")
  if (!nzchar(pkg_dir) || !dir.exists(pkg_dir)) {
    stop("Python submodule not found.  Expected: inst/python/ under the package root.")
  }

  cmd <- sprintf(
    "cd %s && %s%s -m uvicorn api.main:app --host %s --port %d",
    shQuote(pkg_dir), ld_preload, shQuote(python), host, port
  )

  message("Starting theory engine on ", url, " ...")
  system(cmd, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  start_time <- Sys.time()
  while (difftime(Sys.time(), start_time, units = "secs") < timeout) {
    Sys.sleep(0.5)
    alive <- tryCatch(
      httr2::request(url) |>
        httr2::req_timeout(2) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) NULL
    )
    if (!is.null(alive) && identical(alive$status, "success")) {
      message("Theory engine ready.")
      return(invisible(TRUE))
    }
  }

  stop("Theory engine did not start within ", timeout, " seconds.")
}


#' Stop the theoRy Python backend engine
#'
#' Sends a graceful shutdown request to the running theory engine.
#' If the backend is unreachable or the shutdown endpoint is
#' unavailable, it falls back to killing the process bound to the
#' given port.
#'
#' @param port Port number.  Defaults to \code{8000}.
#' @param host Host address.  Defaults to \code{"127.0.0.1"}.
#' @param timeout Maximum seconds to wait for graceful shutdown.
#'   Defaults to 3.
#'
#' @return Invisibly returns \code{TRUE} if the engine was stopped
#'   (or was not running).
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
                               timeout = 3) {
  base <- sprintf("http://%s:%d", host, port)

  # Check if anything is even running
  alive <- tryCatch(
    httr2::request(paste0(base, "/api/v1/health")) |>
      httr2::req_timeout(2) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) NULL
  )
  if (is.null(alive) || !identical(alive$status, "success")) {
    message("Theory engine is not running.")
    return(invisible(TRUE))
  }

  # Try graceful shutdown endpoint
  shutdown_ok <- FALSE
  tryCatch({
    resp <- httr2::request(paste0(base, "/api/v1/shutdown")) |>
      httr2::req_method("POST") |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()
    shutdown_ok <- TRUE
  }, error = function(e) NULL)

  if (shutdown_ok) {
    message("Theory engine stopped gracefully.")
  } else {
    message("Graceful shutdown timed out. Force-killing by port...")
    kill_port(port, host)
    Sys.sleep(0.5)
    message("Theory engine stopped.")
  }

  invisible(TRUE)
}

test_that("python executable validation rejects a missing command", {
  expect_error(
    theoRy:::.theory_engine_python("theory-python-command-that-does-not-exist"),
    "Python executable not found"
  )
})

test_that("stop_theory_engine only terminates a managed process", {
  state <- theoRy:::.theory_engine_state
  saved_processes <- state$processes
  state$processes <- list()
  on.exit({
    state$processes <- saved_processes
  }, add = TRUE)

  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )
  process <- processx::process$new(
    rscript,
    args = c("-e", "Sys.sleep(30)")
  )
  on.exit({
    if (isTRUE(process$is_alive())) process$kill()
  }, add = TRUE)

  host <- "127.0.0.1"
  port <- as.integer(sample(49152:65535, 1L))
  key <- theoRy:::.theory_engine_key(host, port)
  state$processes[[key]] <- list(
    process = process,
    stdout_log = tempfile(),
    stderr_log = tempfile()
  )

  expect_message(stop_theory_engine(port, host, timeout = 0), "stopped")
  expect_false(process$is_alive())
  expect_null(state$processes[[key]])
})

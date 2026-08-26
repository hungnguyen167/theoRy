test_that("build_component_registry returns correct structure via mocked HTTP", {
  mock_body <- '{"status":"success","data":{"registry_data":[{"comp_id":"C0001","type":"node","source":"X","target":null,"direction":null,"description":"X"},{"comp_id":"C0002","type":"node","source":"Y","target":null,"direction":null,"description":"Y"},{"comp_id":"C0003","type":"edge","source":"X","target":"Y","direction":"->","description":"X -> Y"}],"summary":{"total_components":3,"nodes":2,"edges":1,"directed_edges":1,"bidirectional_edges":0}}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- build_component_registry(
        nodes = c("X", "Y"),
        timing = c(1, 2),
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_named(result, c("comp_id", "type", "source", "target", "direction", "description"))
      expect_equal(nrow(result), 3)
      expect_equal(result$comp_id, c("C0001", "C0002", "C0003"))
    }
  )
})

test_that("build_component_registry accepts data frame nodes input", {
  mock_body <- '{"status":"success","data":{"registry_data":[{"comp_id":"C0001","type":"node","source":"A","target":null,"direction":null,"description":"A"},{"comp_id":"C0002","type":"node","source":"B","target":null,"direction":null,"description":"B"}],"summary":{"total_components":2,"nodes":2,"edges":0,"directed_edges":0,"bidirectional_edges":0}}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      nodes_df <- data.frame(
        name = c("A", "B"),
        timing = c(1L, 2L),
        stringsAsFactors = FALSE
      )
      result <- build_component_registry(
        nodes = nodes_df,
        exposure = "A",
        outcome = "B",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_equal(result$comp_id, c("C0001", "C0002"))
    }
  )
})

test_that("build_component_registry validates empty nodes locally", {
  expect_error(
    build_component_registry(
      nodes = character(0),
      url = "http://localhost:8000"
    ),
    "At least one node is required"
  )
})

test_that("build_component_registry rejects non-positive timing locally", {
  expect_error(
    build_component_registry(
      nodes = c("X", "Y"), timing = c(0, 2),
      exposure = "X", outcome = "Y"
    ),
    "values >= 1"
  )

  expect_error(
    build_component_registry(
      nodes = data.frame(name = c("X", "Y"), timing = c(-1, 2)),
      exposure = "X", outcome = "Y"
    ),
    "values >= 1"
  )

  expect_error(
    build_component_registry(
      nodes = c("X", "Mediator", "Y"), timing = c(1, NA, 3),
      time_points = c(0, 1), exposure = "X", outcome = "Y"
    ),
    "time_points.*values >= 1"
  )

  expect_error(
    build_component_registry(
      nodes = c("X", "Y"), timing = c(1, 2),
      timing_options = list(X = c(0, 1)), exposure = "X", outcome = "Y"
    ),
    "timing_options.*values >= 1"
  )
})

test_that("build_component_registry rejects non-finite, fractional, and overflow timing", {
  bad_values <- list(NaN, Inf, -Inf, 1.5, 2147483648)
  for (bad in bad_values) {
    expect_error(
      build_component_registry(
        nodes = c("X", "Y"), timing = c(bad, 2),
        exposure = "X", outcome = "Y"
      ),
      "timing.*values >= 1"
    )
  }

  expect_error(
    build_component_registry(
      nodes = c("X", "Y"), timing = c(1, 2),
      time_points = c(1, 2147483648), exposure = "X", outcome = "Y"
    ),
    "time_points.*values >= 1"
  )

  expect_error(
    build_component_registry(
      nodes = c("X", "Y"), timing = c(1, 2),
      timing_options = list(X = c(1, 2147483648)),
      exposure = "X", outcome = "Y"
    ),
    "timing_options.*values >= 1"
  )
})

test_that("interactive registry input rejects non-positive timing", {
  answers <- c("X,Y", "X", "Y", "0")
  input <- function(prompt) {
    value <- answers[[1]]
    answers <<- answers[-1]
    value
  }

  expect_error(
    build_component_registry_interactive(input = input),
    "integers >= 1"
  )
})

test_that("build_component_registry errors when server unreachable", {
  httr2::with_mocked_responses(
    function(req) {
      stop("Connection refused")
    },
    {
      expect_error(
        build_component_registry(
          nodes = c("X", "Y"),
          timing = c(1, 2),
          exposure = "X",
          outcome = "Y",
          url = "http://localhost:8000"
        ),
        "Python backend not reachable"
      )
    }
  )
})

test_that("build_component_registry forwards and returns the fixed focal path", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{"registry_data":[',
    '{"comp_id":"C0001","type":"node","source":"X","target":null,"direction":null,"description":"X","fixed_status":null},',
    '{"comp_id":"C0002","type":"node","source":"Y","target":null,"direction":null,"description":"Y","fixed_status":null},',
    '{"comp_id":"C0003","type":"edge","source":"X","target":"Y","direction":"->","description":"X -> Y","fixed_status":"causal"}',
    '],"summary":{"total_components":3,"nodes":2,"edges":1,"directed_edges":1,"bidirectional_edges":0}}}'
  )

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- build_component_registry(
        nodes = c("X", "Y"),
        timing = c(1, 2),
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )
    }
  )

  expect_equal(captured$body$data$exposure, "X")
  expect_equal(captured$body$data$outcome, "Y")
  expect_equal(result$fixed_status, c(NA_character_, NA_character_, "causal"))
  expect_equal(attr(result, "exposure"), "X")
  expect_equal(attr(result, "outcome"), "Y")
})

test_that("build_component_registry requires a finite time set for unknown non-focal timing", {
  expect_error(
    build_component_registry(
      nodes = c("X", "Mediator", "Y"),
      timing = c(1, NA, 3),
      exposure = "X",
      outcome = "Y"
    ),
    "time_points is required"
  )
})

test_that("interactive registry input delegates to the programmatic contract", {
  answers <- c(
    "X, Mediator, Y", "X", "Y", "1", "2,3", "4", "", "", "", "(Mediator,Y)",
    "Mediator", "yes"
  )
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{"registry_data":[',
    '{"comp_id":"C0001","type":"node","source":"X","target":null,"direction":null,"description":"X"},',
    '{"comp_id":"C0002","type":"node","source":"Mediator","target":null,"direction":null,"description":"Mediator"},',
    '{"comp_id":"C0003","type":"node","source":"Y","target":null,"direction":null,"description":"Y"}',
    ']}}'
  )
  input <- function(prompt) {
    value <- answers[[1]]
    answers <<- answers[-1]
    value
  }

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- build_component_registry_interactive(
        url = "http://localhost:8000", input = input
      )
    }
  )

  expect_equal(attr(result, "exposure"), "X")
  expect_equal(attr(result, "outcome"), "Y")
  expect_equal(attr(result, "timing_options")$Mediator, c(2L, 3L))
  expect_equal(attr(result, "optional_nodes"), "Mediator")
  expect_equal(captured$body$data$constraints[[1]]$direction, "<->")
  expect_equal(captured$body$data$constraints[[1]]$rule, "allow")
})

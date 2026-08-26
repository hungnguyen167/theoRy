test_that("build_dyad_matrix returns a data frame with directed pairs", {
  mock_body <- '{"status":"success","data":{"dyads":[{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002","similarity_rate":0.75,"timing_compatible":true,"existence_conflict":false,"conflicting_components":[],"repair_cost":1}],"model_count":2,"dyad_count":1}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      reg <- data.frame(
        comp_id = c("C0001", "C0002"),
        type = c("node", "node"),
        source = c("X", "Y"),
        target = c(NA_character_, NA_character_),
        direction = c(NA_character_, NA_character_),
        description = c("X", "Y"),
        stringsAsFactors = FALSE
      )

      states <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal"),
        list(model_id = "M0001", comp_id = "C0002", status = "causal"),
        list(model_id = "M0002", comp_id = "C0001", status = "causal"),
        list(model_id = "M0002", comp_id = "C0002", status = "unknown")
      )

      result <- build_dyad_matrix(
        registry = reg,
        states = states,
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_named(result, c("dyad_id", "ego_id", "alter_id", "similarity_rate",
                             "timing_compatible", "existence_conflict", "repair_cost"))
      expect_equal(nrow(result), 1)
      expect_equal(result$ego_id[1], "M0001")
      expect_equal(result$alter_id[1], "M0002")
      expect_equal(result$dyad_id[1], "M0001__M0002")
      expect_type(attr(result, "theory_context"), "list")
      expect_equal(length(attr(result, "theory_context")$registry_data), 2)
      expect_equal(length(attr(result, "theory_context")$state_data), 4)
    }
  )
})

test_that("build_dyad_matrix validates direct state timing before HTTP", {
  reg <- data.frame(
    comp_id = "C0001", type = "node", source = "X",
    target = NA_character_, direction = NA_character_, description = "X",
    stringsAsFactors = FALSE
  )
  bad_values <- list(NaN, Inf, -Inf, 1.5, 0, -1, 2147483648)

  for (bad in bad_values) {
    states_df <- data.frame(
      model_id = "M0001", comp_id = "C0001", status = "present",
      timing = bad, stringsAsFactors = FALSE
    )
    expect_error(
      build_dyad_matrix(registry = reg, states = states_df,
                        url = "http://localhost:8000"),
      "states timing.*values >= 1"
    )

    states_list <- list(list(
      model_id = "M0001", comp_id = "C0001", status = "present",
      timing = bad
    ))
    expect_error(
      build_dyad_matrix(registry = reg, states = states_list,
                        url = "http://localhost:8000"),
      "states timing.*values >= 1"
    )
  }
})

test_that("build_dyad_matrix defaults to the general-identification backend", {
  expect_identical(
    eval(formals(build_dyad_matrix)$causal_backend),
    c("r", "auto", "native")
  )
})

test_that("build_dyad_matrix returns correct column types", {
  mock_body <- '{"status":"success","data":{"dyads":[{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002","similarity_rate":1.0,"timing_compatible":true,"existence_conflict":false,"conflicting_components":[],"repair_cost":0}],"model_count":2,"dyad_count":1}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      reg <- data.frame(
        comp_id = "C0001",
        type = "node",
        source = "X",
        target = NA_character_,
        direction = NA_character_,
        description = "X",
        stringsAsFactors = FALSE
      )

      states <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal"),
        list(model_id = "M0002", comp_id = "C0001", status = "causal")
      )

      result <- build_dyad_matrix(
        registry = reg,
        states = states,
        url = "http://localhost:8000"
      )

      expect_type(result$dyad_id, "character")
      expect_type(result$ego_id, "character")
      expect_type(result$alter_id, "character")
      expect_type(result$similarity_rate, "double")
      expect_type(result$timing_compatible, "logical")
      expect_type(result$existence_conflict, "logical")
      expect_type(result$repair_cost, "integer")
    }
  )
})

test_that("build_dyad_matrix errors when server unreachable", {
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw('{"status": "error"}')
      )
    },
    {
      reg <- data.frame(
        comp_id = "C0001",
        type = "node",
        source = "X",
        target = NA_character_,
        direction = NA_character_,
        description = "X",
        stringsAsFactors = FALSE
      )

      expect_error(
        build_dyad_matrix(
          registry = reg,
          states = list(list(model_id = "M0001", comp_id = "C0001", status = "causal")),
          url = "http://localhost:8000"
        ),
        "Backend error"
      )
    }
  )
})

test_that("build_dyad_matrix errors on backend error wrapper", {
  mock_err_body <- '{"status":"error","code":"INTERNAL_ERROR","message":"An unexpected error occurred"}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_err_body)
      )
    },
    {
      reg <- data.frame(
        comp_id = "C0001",
        type = "node",
        source = "X",
        target = NA_character_,
        direction = NA_character_,
        description = "X",
        stringsAsFactors = FALSE
      )

      states <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal", timing = 1)
      )

      expect_error(
        build_dyad_matrix(
          registry = reg,
          states = states,
          url = "http://localhost:8000"
        ),
        "Backend error \\[INTERNAL_ERROR\\]"
      )
    }
  )
})

test_that("build_dyad_matrix returns tidy format for multiple pairs", {
  mock_body <- '{"status":"success","data":{"dyads":[{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002","similarity_rate":0.5,"timing_compatible":false,"existence_conflict":false,"conflicting_components":[],"repair_cost":1},{"dyad_id":"M0001__M0003","ego_id":"M0001","alter_id":"M0003","similarity_rate":0.0,"timing_compatible":true,"existence_conflict":true,"conflicting_components":["C0001"],"repair_cost":2},{"dyad_id":"M0002__M0003","ego_id":"M0002","alter_id":"M0003","similarity_rate":0.0,"timing_compatible":true,"existence_conflict":true,"conflicting_components":["C0001"],"repair_cost":2}],"model_count":3,"dyad_count":3}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      reg <- data.frame(
        comp_id = "C0001",
        type = "node",
        source = "X",
        target = NA_character_,
        direction = NA_character_,
        description = "X",
        stringsAsFactors = FALSE
      )

      states <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal", timing = 1),
        list(model_id = "M0002", comp_id = "C0001", status = "causal", timing = 2),
        list(model_id = "M0003", comp_id = "C0001", status = "non-causal")
      )

      result <- build_dyad_matrix(
        registry = reg,
        states = states,
        url = "http://localhost:8000"
      )

      expect_equal(nrow(result), 3)
      expect_equal(result$dyad_id[1], "M0001__M0002")
      expect_equal(result$ego_id[1], "M0001")
      expect_equal(result$alter_id[1], "M0002")
      expect_equal(result$ego_id[1], "M0001")
    }
  )
})

test_that("build_dyad_matrix includes exposure/outcome in payload", {
  captured <- NULL

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw('{"status":"success","data":{"dyads":[{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002","similarity_rate":0.75,"timing_compatible":true,"existence_conflict":false,"conflicting_components":[],"repair_cost":1}],"model_count":2,"dyad_count":1}}')
      )
    },
    {
      reg <- data.frame(
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X->Y"),
        fixed_status = c(NA_character_, NA_character_, "causal"),
        stringsAsFactors = FALSE
      )

      states <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal"),
        list(model_id = "M0001", comp_id = "C0002", status = "causal"),
        list(model_id = "M0001", comp_id = "C0003", status = "causal"),
        list(model_id = "M0002", comp_id = "C0001", status = "causal"),
         list(model_id = "M0002", comp_id = "C0002", status = "causal"),
        list(model_id = "M0002", comp_id = "C0003", status = "causal")
      )

      result <- build_dyad_matrix(
        registry = reg,
        states = states,
        mode = "full",
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      body <- captured$body$data
      expect_equal(body$exposure, "X")
      expect_equal(body$outcome, "Y")
      expect_equal(body$mode, "full")
    }
  )
})

test_that("build_dyad_matrix uses state exposure/outcome attributes", {
  captured <- NULL

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw('{"status":"success","data":{"dyads":[{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002","similarity_rate":1,"timing_compatible":true,"existence_conflict":false,"conflicting_components":[],"repair_cost":0}],"model_count":2,"dyad_count":1}}')
      )
    },
    {
      reg <- data.frame(
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X->Y"),
        fixed_status = c(NA_character_, NA_character_, "causal"),
        stringsAsFactors = FALSE
      )

      states <- data.frame(
        model_id = c("M0001", "M0001", "M0001", "M0002", "M0002", "M0002"),
        comp_id = c("C0001", "C0002", "C0003", "C0001", "C0002", "C0003"),
        status = c("causal", "causal", "causal", "causal", "causal", "causal"),
        timing = c(1L, 2L, NA_integer_, 1L, 2L, NA_integer_),
        stringsAsFactors = FALSE
      )
      attr(states, "exposure") <- "X"
      attr(states, "outcome") <- "Y"

      build_dyad_matrix(
        registry = reg,
        states = states,
        mode = "full",
        url = "http://localhost:8000"
      )

      body <- captured$body$data
      expect_equal(body$exposure, "X")
      expect_equal(body$outcome, "Y")
    }
  )
})

test_that("build_dyad_matrix errors on single exposure/outcome", {
  reg <- data.frame(
    comp_id = "C0001",
    type = "node",
    source = "X",
    target = NA_character_,
    direction = NA_character_,
    description = "X",
    stringsAsFactors = FALSE
  )

  states <- list(
    list(model_id = "M0001", comp_id = "C0001", status = "causal")
  )

  expect_error(
    build_dyad_matrix(reg, states, exposure = "X", url = "http://localhost:8000"),
    "Both or neither"
  )
  expect_error(
    build_dyad_matrix(reg, states, outcome = "Y", url = "http://localhost:8000"),
    "Both or neither"
  )
})

test_that("build_dyad_matrix preserves all MAS list-column states", {
  mock_body <- paste0(
    '{"status":"success","data":{"dyads":[',
    '{"dyad_id":"M0001__M0002","ego_id":"M0001","alter_id":"M0002",',
    '"similarity_rate":0.5,"timing_compatible":true,',
    '"existence_conflict":false,"repair_cost":0,',
    '"mas_ego":null,"mas_alter":null,"mas_compatible":null,',
    '"identified_ego":null,"identified_alter":null,',
    '"identified_compatible":null},',
    '{"dyad_id":"M0002__M0003","ego_id":"M0002","alter_id":"M0003",',
    '"similarity_rate":0.5,"timing_compatible":true,',
    '"existence_conflict":false,"repair_cost":0,',
    '"mas_ego":[],"mas_alter":[],"mas_compatible":false,',
    '"identified_ego":false,"identified_alter":false,',
    '"identified_compatible":false},',
    '{"dyad_id":"M0003__M0001","ego_id":"M0003","alter_id":"M0001",',
    '"similarity_rate":1.0,"timing_compatible":true,',
    '"existence_conflict":false,"repair_cost":0,',
    '"mas_ego":[[]],"mas_alter":[[]],"mas_compatible":true,',
    '"identified_ego":true,"identified_alter":true,',
    '"identification_nodes_ego":["X","Y"],',
    '"identification_nodes_alter":["X","Y"],',
    '"identified_compatible":true}],"model_count":3,"dyad_count":3}}'
  )

  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003"),
    type = c("node", "node", "edge"),
    source = c("X", "Y", "X"),
    target = c(NA_character_, NA_character_, "Y"),
    direction = c(NA_character_, NA_character_, "->"),
    description = c("X", "Y", "X->Y"),
    fixed_status = c(NA_character_, NA_character_, "causal"),
    stringsAsFactors = FALSE
  )
  states <- list(
    list(model_id = "M0001", comp_id = "C0001", status = "causal"),
    list(model_id = "M0001", comp_id = "C0002", status = "causal"),
    list(model_id = "M0001", comp_id = "C0003", status = "causal"),
    list(model_id = "M0002", comp_id = "C0001", status = "causal"),
    list(model_id = "M0002", comp_id = "C0002", status = "causal"),
    list(model_id = "M0002", comp_id = "C0003", status = "causal"),
    list(model_id = "M0003", comp_id = "C0001", status = "causal"),
    list(model_id = "M0003", comp_id = "C0002", status = "causal"),
    list(model_id = "M0003", comp_id = "C0003", status = "causal")
  )

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- build_dyad_matrix(
        registry, states, mode = "full", exposure = "X", outcome = "Y",
        url = "http://localhost:8000"
      )

      expect_null(result$mas_ego[[1]])
      expect_identical(result$mas_ego[[2]], list())
      expect_identical(result$mas_ego[[3]], list(character(0)))
      expect_null(result$mas_alter[[1]])
      expect_identical(result$mas_alter[[2]], list())
      expect_identical(result$mas_alter[[3]], list(character(0)))
      expect_identical(result$mas_compatible, c(NA, FALSE, TRUE))
      expect_null(result$identification_nodes_ego[[1]])
      expect_identical(result$identification_nodes_ego[[3]], c("X", "Y"))
      expect_identical(result$identification_nodes_alter[[3]], c("X", "Y"))
    }
  )
})

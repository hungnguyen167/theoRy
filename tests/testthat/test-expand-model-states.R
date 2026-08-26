test_that("expand_model_states sampled mode with seed_claims works", {
  mock_body <- '{"status":"success","data":{"state_data":[{"model_id":"M0001","comp_id":"C0001","status":"causal","timing":null,"seeded":true},{"model_id":"M0001","comp_id":"C0002","status":"unknown","timing":null,"seeded":true},{"model_id":"M0002","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0002","status":"non-causal","timing":null,"seeded":false}],"model_count":2,"component_count":2,"seeded_model_ids":["M0001"]}}'

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

      claims <- list(
        list(model_id = "M0001", comp_id = "C0001", status = "causal")
      )

      result <- expand_model_states(
        registry = reg,
        mode = "sampled",
        n_models = 2L,
        seed_claims = claims,
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_true("seeded" %in% names(result))
      expect_equal(nrow(result), 4)
      expect_equal(attr(result, "seeded_model_ids"), "M0001")
    }
  )
})

test_that("expand_model_states accepts data frame seed_claims", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{"state_data":[',
    '{"model_id":"M0001","comp_id":"C0001","status":"causal","timing":1,"seeded":true},',
    '{"model_id":"M0001","comp_id":"C0002","status":"causal","timing":2,"seeded":true},',
    '{"model_id":"M0001","comp_id":"C0003","status":"causal","timing":null,"seeded":true},',
    '{"model_id":"M0002","comp_id":"C0001","status":"causal","timing":2,"seeded":false},',
    '{"model_id":"M0002","comp_id":"C0002","status":"causal","timing":1,"seeded":false},',
    '{"model_id":"M0002","comp_id":"C0003","status":"unknown","timing":null,"seeded":false}',
    '],"model_count":2,"component_count":3,"seeded_model_ids":["M0001"]}}'
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
      reg <- data.frame(
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X->Y"),
        stringsAsFactors = FALSE
      )

      claims <- data.frame(
        model_id = c("M0001", "M0001", "M0001"),
        comp_id = c("C0001", "C0002", "C0003"),
        status = c("causal", "causal", "causal"),
        timing = c(1L, 2L, NA),
        stringsAsFactors = FALSE
      )

      result <- expand_model_states(
        registry = reg,
        mode = "sampled",
        n_models = 2L,
        seed_claims = claims,
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      body <- captured$body$data
      expect_equal(length(body$seed_claims), 3)
      expect_equal(body$seed_claims[[1]]$model_id, "M0001")
      expect_equal(body$seed_claims[[1]]$timing, 1L)
      expect_null(body$seed_claims[[3]]$timing)
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 6)
      expect_true(all(result$seeded[result$model_id == "M0001"]))
    }
  )
})

test_that("expand_model_states accepts Parquet file path as registry", {
  mock_body <- '{"status":"success","data":{"state_data":[{"model_id":"M0001","comp_id":"C0001","status":"causal","timing":1,"seeded":false}],"model_count":1,"component_count":1,"seeded_model_ids":[]}}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- expand_model_states(
        registry = system.file(
          "extdata", "component_registry.parquet", package = "theoRy"
        ),
        mode = "sampled",
        n_models = 1L,
        exposure = "SolarRad",
        outcome = "Visibility",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
    }
  )
})

test_that("expand_model_states exhaustive mode returns rejected with tri-state default", {
  mock_body <- '{"status":"success","data":{"state_data":[{"model_id":"M0001","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0001","comp_id":"C0002","status":"causal","timing":null,"seeded":false},{"model_id":"M0001","comp_id":"C0003","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0002","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0003","status":"unknown","timing":null,"seeded":false},{"model_id":"M0003","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0003","comp_id":"C0002","status":"causal","timing":null,"seeded":false},{"model_id":"M0003","comp_id":"C0003","status":"non-causal","timing":null,"seeded":false}],"model_count":3,"component_count":3,"seeded_model_ids":[]}}'

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
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X->Y"),
        stringsAsFactors = FALSE
      )

      result <- expand_model_states(
        registry = reg,
        mode = "exhaustive",
        node_timing = c(X = 1, Y = 2),
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_equal(length(unique(result$model_id)), 3)
      statuses <- unique(result$status)
      expect_true("non-causal" %in% statuses)
    }
  )
})

test_that("expand_model_states edge_statuses binary compatibility", {
  mock_body <- '{"status":"success","data":{"state_data":[{"model_id":"M0001","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0001","comp_id":"C0002","status":"causal","timing":null,"seeded":false},{"model_id":"M0001","comp_id":"C0003","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0001","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0002","status":"causal","timing":null,"seeded":false},{"model_id":"M0002","comp_id":"C0003","status":"unknown","timing":null,"seeded":false}],"model_count":2,"component_count":3,"seeded_model_ids":[]}}'

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
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X->Y"),
        stringsAsFactors = FALSE
      )

      result <- expand_model_states(
        registry = reg,
        mode = "exhaustive",
        node_timing = c(X = 1, Y = 2),
        edge_statuses = c("causal", "unknown"),
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )

      expect_s3_class(result, "data.frame")
      expect_equal(length(unique(result$model_id)), 2)
      statuses <- unique(result$status)
      expect_true("causal" %in% statuses)
      expect_true("unknown" %in% statuses)
      expect_false("non-causal" %in% statuses)
    }
  )
})

test_that("expand_model_states errors on backend error", {
  mock_body <- '{"status":"error","code":"EXPANSION_ERROR","message":"Too many models"}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 400L,
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

      expect_error(
        expand_model_states(
          registry = reg,
          mode = "exhaustive",
          exposure = "X",
          outcome = "Y",
          url = "http://localhost:8000"
        ),
        "Backend error \\[EXPANSION_ERROR\\]"
      )
    }
  )
})

test_that("expand_model_states errors on deprecated 'seeded' mode", {
  reg <- data.frame(
    comp_id = "C0001", type = "node", source = "X",
    target = NA_character_, direction = NA_character_,
    description = "X", stringsAsFactors = FALSE
  )

  expect_error(
    expand_model_states(registry = reg, mode = "seeded"),
    "sampled|exhaustive"
  )
})

test_that("expand_model_states rejects non-positive timing before HTTP", {
  reg <- data.frame(
    comp_id = c("C0001", "C0002"),
    type = c("node", "node"),
    source = c("X", "Y"),
    target = c(NA_character_, NA_character_),
    direction = c(NA_character_, NA_character_),
    description = c("X", "Y"),
    stringsAsFactors = FALSE
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled", node_timing = c(X = 0, Y = 2),
      exposure = "X", outcome = "Y"
    ),
    "node_timing.*values >= 1"
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      timing_options = list(X = c(1, -1)), exposure = "X", outcome = "Y"
    ),
    "timing_options.*values >= 1"
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      seed_claims = data.frame(
        model_id = "M0001", comp_id = "C0001", status = "present",
        timing = 0L
      ),
      exposure = "X", outcome = "Y"
    ),
    "seed_claims timing.*values >= 1"
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      seed_claims = list(list(
        model_id = "M0001", comp_id = "C0001", status = "present",
        timing = -1L
      )),
      exposure = "X", outcome = "Y"
    ),
    "seed_claims timing.*values >= 1"
  )
})

test_that("expand_model_states rejects non-finite, fractional, and overflow timing", {
  reg <- data.frame(
    comp_id = c("C0001", "C0002"),
    type = c("node", "node"),
    source = c("X", "Y"),
    target = c(NA_character_, NA_character_),
    direction = c(NA_character_, NA_character_),
    description = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  bad_values <- list(NaN, Inf, -Inf, 1.5, 2147483648)

  for (bad in bad_values) {
    expect_error(
      expand_model_states(
        registry = reg, mode = "sampled",
        node_timing = c(X = bad, Y = 2), exposure = "X", outcome = "Y"
      ),
      "node_timing.*values >= 1"
    )
  }

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      timing_options = list(X = c(1, NaN)), exposure = "X", outcome = "Y"
    ),
    "timing_options.*values >= 1"
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      seed_claims = data.frame(
        model_id = "M0001", comp_id = "C0001", status = "present",
        timing = 2147483648
      ),
      exposure = "X", outcome = "Y"
    ),
    "seed_claims timing.*values >= 1"
  )

  expect_error(
    expand_model_states(
      registry = reg, mode = "sampled",
      seed_claims = list(list(
        model_id = "M0001", comp_id = "C0001", status = "present",
        timing = NaN
      )),
      exposure = "X", outcome = "Y"
    ),
    "seed_claims timing.*values >= 1"
  )
})

test_that("expand_model_states preserves fixed_status in the backend payload", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{"state_data":[',
    '{"model_id":"M0001","comp_id":"C0001","status":"present","timing":null,"seeded":false},',
    '{"model_id":"M0001","comp_id":"C0002","status":"present","timing":null,"seeded":false},',
    '{"model_id":"M0001","comp_id":"C0003","status":"causal","timing":null,"seeded":false}',
    '],"model_count":1,"component_count":3,"seeded_model_ids":[]}}'
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
      registry <- data.frame(
        comp_id = c("C0001", "C0002", "C0003"),
        type = c("node", "node", "edge"),
        source = c("X", "Y", "X"),
        target = c(NA_character_, NA_character_, "Y"),
        direction = c(NA_character_, NA_character_, "->"),
        description = c("X", "Y", "X -> Y"),
        fixed_status = c(NA_character_, NA_character_, "causal"),
        stringsAsFactors = FALSE
      )
      result <- expand_model_states(
        registry = registry,
        mode = "sampled",
        n_models = 1L,
        exposure = "X",
        outcome = "Y",
        url = "http://localhost:8000"
      )
    }
  )

  payload_registry <- captured$body$data$registry_data
  expect_equal(payload_registry[[3]]$fixed_status, "causal")
  expect_equal(result$status[result$comp_id == "C0003"], "causal")
})

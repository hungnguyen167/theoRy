test_that("run_simulation with symbolic mode returns structured result", {
  result <- list(
    scenario = "consensus_illusion",
    results = list(
      list(class_id = "Q0001", mass = 10, adjustment_identifiable = TRUE)
    ),
    artifacts = list(
      shared_edges = list(c("X", "Y")),
      critical_edges = list()
    ),
    summary = "Symbolic simulation complete: consensus_illusion"
  )
  expect_equal(result$scenario, "consensus_illusion")
  expect_true(length(result$results) > 0)
  expect_true(length(result$summary) > 0)
})

test_that("all concrete simulation wrappers forward the causal metric", {
  calls <- list(
    run_simulation = function() {
      run_simulation(
        "consensus_illusion", n_models = 10, n_components = 5,
        compatibility_metric = "mas_compatible",
        crux_mode = "marginal",
        exposure = "Treatment", outcome = "Response",
        url = "http://localhost:8000"
      )
    },
    run_simulation_consensus = function() {
      run_simulation_consensus(
        n_models = 10, n_components = 5,
        compatibility_metric = "mas_compatible",
        crux_mode = "marginal",
        exposure = "Treatment", outcome = "Response",
        url = "http://localhost:8000"
      )
    },
    run_simulation_lynchpin = function() {
      run_simulation_lynchpin(
        n_models = 10, n_components = 5,
        compatibility_metric = "mas_compatible",
        crux_mode = "marginal",
        exposure = "Treatment", outcome = "Response",
        url = "http://localhost:8000"
      )
    },
    run_simulation_crux = function() {
      run_simulation_crux(
        n_models = 10, n_components = 5,
        compatibility_metric = "mas_compatible",
        crux_mode = "marginal",
        exposure = "Treatment", outcome = "Response",
        url = "http://localhost:8000"
      )
    },
    run_simulation_ghost = function() {
      run_simulation_ghost(
        n_models = 10, n_components = 5,
        compatibility_metric = "mas_compatible",
        crux_mode = "marginal",
        exposure = "Treatment", outcome = "Response",
        url = "http://localhost:8000"
      )
    }
  )

  for (wrapper in names(calls)) {
    captured <- NULL
    httr2::with_mocked_responses(
      function(req) {
        captured <<- req
        httr2::response(
          status_code = 500L,
          headers = list("content-type" = "application/json"),
          body = charToRaw(
            '{"status":"error","code":"TEST","message":"captured"}'
          )
        )
      },
      expect_error(calls[[wrapper]](), "Backend error \\[TEST\\]")
    )

    body <- captured$body$data
    expect_equal(body$compatibility_metric, "mas_compatible", info = wrapper)
    if (wrapper %in% c("run_simulation_lynchpin", "run_simulation_crux")) {
      expect_equal(body$crux_mode, "marginal", info = wrapper)
    } else {
      # Consensus Illusion and Ghost scenarios do not use crux semantics.
      expect_null(body$crux_mode, info = wrapper)
      expect_null(body$global_status, info = wrapper)
    }
    expect_equal(body$exposure, "Treatment", info = wrapper)
    expect_equal(body$outcome, "Response", info = wrapper)
  }
})

test_that("Consensus Illusion results parse only the canonical schema", {
  parsed <- .parse_consensus_results(list(
    mean_similarity_rate = 0.88,
    compatibility_metric = "mas_compatible",
    compatibility_rate = 0.41,
    consensus_illusion_gap = 0.47,
    resolved_model_count = 128,
    partial_model_count = 64,
    design = "mas_adjustment_sets",
    exposure = "X1",
    outcome = "Y",
    n_dyads = 36672,
    n_comparable_dyads = 36672,
    n_unavailable_dyads = 0,
    analysis_model_count = 192,
    completion_support_model_count = 0
  ))

  expect_named(parsed, c(
    "mean_similarity_rate", "compatibility_metric", "compatibility_rate",
    "consensus_illusion_gap", "resolved_model_count", "partial_model_count",
    "design", "diagnostics"
  ))
  expect_identical(parsed$resolved_model_count, 128L)
  expect_identical(parsed$partial_model_count, 64L)
  expect_identical(parsed$diagnostics$exposure, "X1")
})

test_that("generated Consensus Illusion defaults to MAS and lets backend infer its query", {
  captured <- NULL
  response_json <- paste0(
    '{"status":"success","data":{',
    '"scenario":"consensus_illusion",',
    '"results":{',
    '"mean_similarity_rate":0.88,',
    '"compatibility_metric":"mas_compatible",',
    '"compatibility_rate":0.41,',
    '"consensus_illusion_gap":0.47,',
    '"resolved_model_count":128,',
    '"partial_model_count":64,',
    '"design":"mas_adjustment_sets",',
    '"exposure":"X1","outcome":"Y",',
    '"n_dyads":36672,"n_comparable_dyads":36672,',
    '"n_unavailable_dyads":0,"analysis_model_count":192,',
    '"completion_support_model_count":0},',
    '"artifacts":{"registry_data":[],"state_data":[],',
    '"model_ids":[],"summary_stats":{}}}}'
  )

  result <- httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(response_json)
      )
    },
    suppressMessages(run_simulation(
      "consensus_illusion", url = "http://localhost:8000"
    ))
  )

  body <- captured$body$data
  expect_identical(body$compatibility_metric, "mas_compatible")
  expect_null(body$crux_mode)
  expect_null(body$global_status)
  expect_null(body$exposure)
  expect_null(body$outcome)
  expect_identical(result$results$design, "mas_adjustment_sets")
  expect_identical(result$summary, c(
    "Mean structural similarity: 0.88",
    "MAS Compatibility rate: 0.41",
    "Consensus illusion gap: 0.47",
    "Models: 128 resolved, 64 partial",
    "Design: mas_adjustment_sets"
  ))
})

test_that("generated Consensus Illusion accepts both causal metrics without a query", {
  for (metric in c("mas_compatible", "identified_compatible")) {
    captured <- NULL
    httr2::with_mocked_responses(
      function(req) {
        captured <<- req
        httr2::response(
          status_code = 500L,
          headers = list("content-type" = "application/json"),
          body = charToRaw(
            '{"status":"error","code":"TEST","message":"captured"}'
          )
        )
      },
      expect_error(
        run_simulation_consensus(
          compatibility_metric = metric,
          url = "http://localhost:8000"
        ),
        "Backend error \\[TEST\\]"
      )
    )
    expect_identical(captured$body$data$compatibility_metric, metric)
    expect_null(captured$body$data$exposure)
    expect_null(captured$body$data$outcome)
  }
})

test_that("seeded crux simulations forward crux arguments", {
  captured <- NULL
  registry <- data.frame(
    comp_id = "C0001", type = "node", source = "X", target = NA_character_,
    direction = NA_character_, description = "X", stringsAsFactors = FALSE
  )
  states <- data.frame(
    model_id = "M0001", comp_id = "C0001", status = "causal",
    stringsAsFactors = FALSE
  )

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(
          '{"status":"error","code":"TEST","message":"captured"}'
        )
      )
    },
    expect_error(
      run_simulation_crux(
        registry = registry,
        states = states,
        crux_mode = "global",
        global_status = "causal",
        url = "http://localhost:8000"
      ),
      "Backend error \\[TEST\\]"
    )
  )

  expect_identical(captured$body$data$crux_mode, "global")
  expect_identical(captured$body$data$global_status, "causal")
})


test_that("seeded simulations validate timing before record conversion", {
  registry <- data.frame(
    comp_id = "C0001", type = "node", source = "X",
    target = NA_character_, direction = NA_character_, description = "X",
    stringsAsFactors = FALSE
  )
  bad_values <- list(
    NaN, Inf, -Inf, 1.5, 0, -1, .Machine$integer.max + 1, "1"
  )

  for (bad in bad_values) {
    states <- data.frame(
      model_id = "M0001", comp_id = "C0001", status = "present",
      timing = bad, stringsAsFactors = FALSE
    )
    expect_error(
      run_simulation_ghost(
        registry = registry, states = states, url = "http://localhost:8000"
      ),
      "states timing.*integer values >= 1",
      info = paste("timing =", deparse(bad))
    )
  }

  states <- data.frame(
    model_id = c("M0001", "M0002"),
    comp_id = c("C0001", "C0001"),
    status = c("present", "present"),
    timing = c(NA_real_, 1),
    stringsAsFactors = FALSE
  )
  captured <- NULL
  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(
          '{"status":"error","code":"TEST","message":"captured"}'
        )
      )
    },
    expect_error(
      run_simulation_ghost(
        registry = registry, states = states, url = "http://localhost:8000"
      ),
      "Backend error \\[TEST\\]"
    )
  )
  expect_false("timing" %in% names(captured$body$data$state_data[[1]]))
  expect_identical(captured$body$data$state_data[[2]]$timing, 1)
})


test_that("global crux simulation summary omits an empty lynchpin", {
  response_json <- paste0(
    '{"status":"success","data":{',
    '"scenario":"crux_of_certainty","n_models":3,"n_components":3,',
    '"results":{"compatibility_metric":"similarity_rate",',
    '"baseline_compatibility":0.25,"post_resolution_compatibility":0.5,',
    '"phase_transition_score":0.25,"lynchpin_component_id":null,',
    '"lynchpin_rank":null,"compatibility_timeline":[],',
    '"crux_mode":"global","target_status":"causal"},',
    '"artifacts":{"registry_data":[],"state_data":[],',
    '"model_ids":[],"summary_stats":{}}}}'
  )

  result <- httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(response_json)
      )
    },
    suppressMessages(run_simulation_crux(
      crux_mode = "global", global_status = "causal",
      url = "http://localhost:8000"
    ))
  )

  expect_true("Global crux status: causal" %in% result$summary)
  expect_false(any(grepl("Lynchpin component", result$summary)))
})

test_that("simulation parsers retain crux metadata", {
  parsed <- .parse_lynchpin_results(list(
    compatibility_metric = "similarity_rate",
    baseline_compatibility = 0.3,
    post_resolution_compatibility = 0.6,
    phase_transition_score = 0.3,
    lynchpin_component_id = "C0004",
    crux_component_id = "C0004",
    lynchpin_rank = 1,
    compatibility_timeline = list(),
    crux_mode = "marginal",
    target_status = "causal",
    models_retained = 8,
    dyads_retained = 56,
    models_changed = 2,
    mapping_coverage = 1.0
  ))

  expect_identical(parsed$crux_mode, "marginal")
  expect_identical(parsed$target_status, "causal")
  expect_identical(parsed$crux_component_id, "C0004")
  expect_identical(parsed$models_retained, 8L)
  expect_identical(parsed$dyads_retained, 56L)
  expect_identical(parsed$models_changed, 2L)
  expect_equal(parsed$mapping_coverage, 1.0)

  artifacts <- .parse_extra_artifacts(list(
    rankings = list(list(
      rank = 1, component_id = "C0004", type = "edge",
      source = "X", target = "Y", delta_u = 0.3,
      best_resolution = "positive", dyads_improved = 4,
      dyads_worsened = 0, models_changed_causal = 8,
      instances_forced_causal = 8, crux_mode = "marginal"
    )),
    crux_mode = "marginal",
    models_retained = 8,
    dyads_retained = 56
  ))

  expect_s3_class(artifacts$rankings, "data.frame")
  expect_identical(artifacts$rankings$models_changed_causal, 8L)
  expect_identical(artifacts$rankings$instances_forced_causal, 8L)
  expect_identical(artifacts$rankings$crux_mode, "marginal")
  expect_identical(artifacts$models_retained, 8)
  expect_identical(artifacts$dyads_retained, 56)
})

test_that("simulations reject invalid crux modes", {
  expect_error(
    run_simulation_lynchpin(crux_mode = "replace_all"),
    "'arg' should be one of"
  )
  expect_error(
    run_simulation_lynchpin(crux_mode = "global"),
    "global_status"
  )
  expect_error(
    run_simulation_lynchpin(global_status = "causal"),
    "only valid with crux_mode"
  )
})


test_that("non-crux simulations reject crux-only arguments clearly", {
  expect_error(
    run_simulation_ghost(crux_mode = "global", global_status = "causal"),
    "only apply to lynchpin/crux"
  )
})


test_that("symbolic simulations reject global crux arguments", {
  expect_error(
    run_simulation(
      "lynchpin_of_certainty", mode = "symbolic",
      crux_mode = "global", global_status = "causal"
    ),
    "do not support global crux"
  )
})


test_that("seeded simulation sample_n requires at least two models", {
  registry <- data.frame(
    comp_id = "C0001", type = "node", source = "X",
    target = NA_character_, direction = NA_character_, description = "X"
  )
  states <- data.frame(
    model_id = "M0001", comp_id = "C0001", status = "present"
  )
  expect_error(
    run_simulation_ghost(
      registry = registry, states = states, sample_n = 1L,
      url = "http://localhost:8000"
    ),
    "at least 2"
  )
})

test_that("Consensus Illusion rejects structural compatibility and seeded missing queries", {
  expect_error(
    run_simulation(
      "consensus_illusion", compatibility_metric = "similarity_rate"
    ),
    "requires compatibility_metric"
  )

  expect_error(
    .run_simulation_internal(
      scenario = "consensus_illusion",
      n_models = 10, n_components = 5,
      registry = data.frame(), states = data.frame(),
      random_state = 42,
      compatibility_metric = "mas_compatible",
      url = "http://localhost:8000"
    ),
    "require exposure and outcome"
  )
})

test_that("all simulation entry points consistently reject bidirectional mode", {
  calls <- list(
    general_symbolic = function() run_simulation(
      "consensus_illusion", mode = "symbolic",
      compatibility_metric = "identified_compatible",
      include_bidirectional = TRUE
    ),
    consensus = function() run_simulation_consensus(include_bidirectional = TRUE),
    lynchpin = function() run_simulation_lynchpin(include_bidirectional = TRUE),
    crux = function() run_simulation_crux(include_bidirectional = TRUE),
    ghost = function() run_simulation_ghost(include_bidirectional = TRUE)
  )

  for (entry_point in names(calls)) {
    expect_error(
      calls[[entry_point]](),
      "Simulations support directed components only.*must be FALSE",
      info = entry_point
    )
  }
})

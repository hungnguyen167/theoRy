mock_delta_u_dyads <- function() {
  dyads <- data.frame(
    dyad_id = c("M0001__M0002", "M0002__M0001"),
    ego_id = c("M0001", "M0002"),
    alter_id = c("M0002", "M0001"),
    similarity_rate = c(0.75, 0.75),
    timing_compatible = c(TRUE, TRUE),
    existence_conflict = c(FALSE, FALSE),
    repair_cost = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  attr(dyads, "theory_context") <- list(
    registry_data = list(
      list(comp_id = "C0001", type = "node", source = "X",
           target = NULL, direction = NULL, description = "X"),
      list(comp_id = "C0002", type = "node", source = "Y",
           target = NULL, direction = NULL, description = "Y"),
      list(comp_id = "C0004", type = "edge", source = "X",
           target = "Y", direction = "->", description = "X->Y")
    ),
    state_data = list(
      list(model_id = "M0001", comp_id = "C0001", status = "causal", timing = 1L),
      list(model_id = "M0001", comp_id = "C0002", status = "causal", timing = 2L),
      list(model_id = "M0001", comp_id = "C0004", status = "causal"),
      list(model_id = "M0002", comp_id = "C0001", status = "causal", timing = 1L),
      list(model_id = "M0002", comp_id = "C0002", status = "causal", timing = 2L),
      list(model_id = "M0002", comp_id = "C0004", status = "unknown")
    ),
    model_ids = c("M0001", "M0002")
  )
  dyads
}


test_that("compute_delta_u ranking returns a data frame with expected columns", {
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"rankings":[',
    '{"rank":1,"component_id":"C0004","type":"edge","source":"X","target":"Y",',
    '"direction":"->","delta_u":0.25,"delta_u_causal":0.25,',
    '"delta_u_non_causal":0.0,"best_resolution":"causal",',
    '"dyads_improved":2,"dyads_worsened":0}',
    '],',
    '"component_count":4,',
    '"computation_mode":"exhaustive"',
    '}}'
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
      result <- compute_delta_u(mock_delta_u_dyads(), top_k = 5,
                                url = "http://localhost:8000")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
       expect_named(result, c(
         "rank", "component_id", "type", "source", "target", "direction",
         "delta_u", "delta_u_causal", "delta_u_non_causal", "best_resolution",
         "dyads_improved", "dyads_worsened", "crux_mode"
       ))
      expect_equal(result$component_id[1], "C0004")
      expect_equal(result$delta_u[1], 0.25)
       expect_equal(result$best_resolution[1], "causal")
    }
  )
})

test_that("compute_delta_u single component returns one row", {
  mock_body <- paste0(
    '{"status":"success","data":{',
     '"result":{"component_id":"C0004","direction":"->",',
     '"delta_u_causal":0.25,"delta_u_non_causal":0.0,"delta_u":0.25,',
     '"best_resolution":"causal","dyads_improved":2,"dyads_worsened":0},',
    '"computation_mode":"exhaustive"',
    '}}'
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
      result <- compute_delta_u(mock_delta_u_dyads(), component_id = "C0004",
                                url = "http://localhost:8000")

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(result$component_id[1], "C0004")
      expect_equal(result$type[1], "edge")
      expect_equal(result$source[1], "X")
      expect_equal(result$target[1], "Y")
      expect_equal(result$rank[1], 1)
    }
  )
})

test_that("compute_delta_u two-stage passes mode and threshold", {
  captured <- NULL

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(paste0(
          '{"status":"success","data":{"rankings":[{"rank":1,',
          '"component_id":"C0004","type":"edge","source":"X","target":"Y",',
          '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,',
          '"dyads_worsened":0}],"component_count":4,"computation_mode":"two-stage"}}'
        ))
      )
    },
    {
      compute_delta_u(
        mock_delta_u_dyads(), top_k = 5, mode = "two-stage",
        heatmap_threshold = 0.05, crux_mode = "marginal",
        url = "http://localhost:8000"
      )

      body <- captured$body$data
      expect_equal(length(body$registry_data), 3)
      expect_equal(length(body$state_data), 6)
      expect_equal(length(body$dyads), 2)
      expect_equal(body$mode, "two-stage")
      expect_equal(body$heatmap_threshold, 0.05)
      expect_equal(body$crux_mode, "marginal")
    }
  )
})

test_that("compute_delta_u handles NO_DYADS error", {
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 400L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(
          '{"status":"error","code":"NO_DYADS","message":"Run dyad-matrix computation first"}'
        )
      )
    },
    {
      expect_error(
        compute_delta_u(mock_delta_u_dyads(), top_k = 5,
                        url = "http://localhost:8000"),
        "No dyad records available"
      )
    }
  )
})


test_that("compute_delta_u requires integer top_k", {
  expect_error(
    compute_delta_u(mock_delta_u_dyads(), top_k = 2.7),
    "positive integer"
  )
})

test_that("compute_delta_u handles backend error wrapper", {
  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(
          '{"status":"error","code":"INTERNAL_ERROR","message":"Something went wrong"}'
        )
      )
    },
    {
      expect_error(
        compute_delta_u(mock_delta_u_dyads(), top_k = 5,
                        url = "http://localhost:8000"),
        "Backend error \\[INTERNAL_ERROR\\]"
      )
    }
  )
})

test_that("compute_delta_u synergistic returns list with two data frames", {
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"rankings":[{"rank":1,"component_id":"C0004","type":"edge","source":"X","target":"Y",',
    '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,"dyads_worsened":0}],',
    '"component_count":4,',
    '"computation_mode":"exhaustive",',
    '"synergistic_sets":[',
    '{"components":["C0004","C0006"],"delta_u_combined":0.38,"delta_u_individual_sum":0.30,',
    '"synergy_score":0.08,"label":"super-additive"}',
    ']}}'
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
      result <- compute_delta_u(
        mock_delta_u_dyads(), top_k = 10, synergistic_set_size = 2,
        url = "http://localhost:8000"
      )

      expect_type(result, "list")
      expect_named(result, c("rankings", "synergistic_sets"))
      expect_s3_class(result$rankings, "data.frame")
      expect_s3_class(result$synergistic_sets, "data.frame")
      expect_equal(nrow(result$synergistic_sets), 1)
      expect_equal(result$synergistic_sets$label[1], "super-additive")
    }
  )
})

test_that("compute_delta_u rejects invalid top_k", {
  expect_error(
    compute_delta_u(mock_delta_u_dyads(), top_k = 0, url = "http://localhost:8000"),
    "top_k must be positive"
  )
  expect_error(
    compute_delta_u(mock_delta_u_dyads(), top_k = -1, url = "http://localhost:8000"),
    "top_k must be positive"
  )
})

test_that("compute_delta_u requires dyad matrix context", {
  expect_error(
    compute_delta_u(top_k = 5, url = "http://localhost:8000"),
    "dyads must be a data frame"
  )

  dyads <- mock_delta_u_dyads()
  attr(dyads, "theory_context") <- NULL
  expect_error(
    compute_delta_u(dyads, top_k = 5, url = "http://localhost:8000"),
    "missing theory_context"
  )
})

test_that("compute_delta_u rejects invalid heatmap_threshold", {
  expect_error(
    compute_delta_u(
      top_k = 5, mode = "two-stage",
      dyads = mock_delta_u_dyads(),
      heatmap_threshold = 1.5, url = "http://localhost:8000"
    ),
    "heatmap_threshold must be between 0 and 1"
  )
  expect_error(
    compute_delta_u(
      top_k = 5, mode = "two-stage",
      dyads = mock_delta_u_dyads(),
      heatmap_threshold = -0.1, url = "http://localhost:8000"
    ),
    "heatmap_threshold must be between 0 and 1"
  )
})

test_that("compute_delta_u validates synergistic_set_size", {
  expect_error(
    compute_delta_u(mock_delta_u_dyads(), top_k = 10, synergistic_set_size = 1, url = "http://localhost:8000"),
    "synergistic_set_size must be at least 2"
  )
})

test_that("compute_delta_u sends and parses marginal metadata", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{"rankings":[{',
    '"rank":1,"component_id":"C0004","type":"edge",',
    '"source":"X","target":"Y","delta_u":0.25,',
    '"best_resolution":"positive","dyads_improved":2,"dyads_worsened":0,',
    '"baseline_compatibility":0.4,',
    '"post_compatibility_causal":0.65,',
    '"post_compatibility_non_causal":0.3,',
    '"models_changed_causal":8,"models_changed_non_causal":6,',
    '"instances_forced_causal":8,"instances_forced_non_causal":6,',
    '"mapping_coverage_causal":0.8,"mapping_coverage_non_causal":0.6,',
    '"crux_mode":"marginal"}]}}'
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
      result <- compute_delta_u(
        mock_delta_u_dyads(), url = "http://localhost:8000"
      )

      expect_identical(captured$body$data$crux_mode, "marginal")
      expect_equal(result$baseline_compatibility, 0.4)
      expect_equal(result$post_compatibility_causal, 0.65)
      expect_identical(result$models_changed_causal, 8L)
      expect_identical(result$instances_forced_non_causal, 6L)
      expect_equal(result$mapping_coverage_non_causal, 0.6)
      expect_identical(result$crux_mode, "marginal")
    }
  )
})

test_that("compute_delta_u single-result parser retains resolution metadata", {
  parsed <- .parse_single_result(list(
    component_id = "C0004",
    baseline_compatibility = 0.4,
    post_compatibility_causal = 0.6,
    post_compatibility_non_causal = 0.3,
    models_changed_causal = 8,
    models_changed_non_causal = 6,
    instances_forced_causal = 8,
    instances_forced_non_causal = 6,
    mapping_coverage_causal = 0.8,
    mapping_coverage_non_causal = 0.6,
    crux_mode = "marginal"
  ))

  expect_identical(parsed$models_changed_non_causal, 6L)
  expect_identical(parsed$instances_forced_causal, 8L)
  expect_equal(parsed$post_compatibility_non_causal, 0.3)
  expect_identical(parsed$crux_mode, "marginal")
})

test_that("compute_delta_u rejects invalid crux modes", {
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), crux_mode = "replace_all",
      url = "http://localhost:8000"
    ),
    "'arg' should be one of"
  )
})

test_that("compute_delta_u validates global crux arguments", {
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), crux_mode = "global", global_status = "invalid",
      url = "http://localhost:8000"
    ),
    "'arg' should be one of"
  )
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), crux_mode = "global",
      component_id = "C0004", url = "http://localhost:8000"
    ),
    "component_id"
  )
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), crux_mode = "global",
      mode = "two-stage", url = "http://localhost:8000"
    ),
    "two-stage"
  )
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), crux_mode = "global",
      synergistic_set_size = 2, url = "http://localhost:8000"
    ),
    "synergistic"
  )
})


test_that("compute_delta_u marginal mode still rejects global_status", {
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), global_status = "causal",
      url = "http://localhost:8000"
    ),
    "only valid with crux_mode"
  )
})

test_that("compute_delta_u global parses a one-row result", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"global_result":{',
    '"crux_mode":"global","target_status":"causal","feasible":true,',
    '"baseline_compatibility":0.4,"post_compatibility":0.6,',
    '"compatibility_change":0.2,"delta_u":0.2,',
    '"model_count":3,"dyad_count":6,"models_changed":1,',
    '"unknown_instances_forced":1,"dyads_improved":2,"dyads_worsened":0,',
    '"mapping_coverage":1.0},',
    '"computation_mode":"global","crux_mode":"global"',
    '}}'
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
       expect_warning(
         result <- compute_delta_u(
           mock_delta_u_dyads(), crux_mode = "global", global_status = "causal",
           url = "http://localhost:8000"
         ),
         "deprecated.*ignored.*both causal and non-causal"
       )
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_identical(result$crux_mode, "global")
      expect_identical(result$target_status, "causal")
      expect_true(result$feasible)
      expect_equal(result$compatibility_change, 0.2)
       expect_identical(result$model_count, 3L)
       expect_identical(result$models_changed, 1L)
       expect_false("global_status" %in% names(captured$body$data))
     }
  )
})

test_that("compute_delta_u parses global rankings and omits global_status", {
  captured <- NULL
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"rankings":[',
    '{"rank":1,"component_id":"C0004","type":"edge",',
    '"source":"X","target":"Y","direction":"->",',
    '"delta_u":0.30,"delta_u_causal":0.30,"delta_u_non_causal":0.10,',
    '"best_resolution":"causal","dyads_improved":3,"dyads_worsened":1,',
    '"baseline_compatibility":0.40,',
    '"post_compatibility_causal":0.70,',
    '"post_compatibility_non_causal":0.50,',
    '"compatibility_change_causal":0.30,',
    '"compatibility_change_non_causal":0.10,',
    '"models_changed_causal":4,"models_changed_non_causal":4,',
    '"instances_forced_causal":4,"instances_forced_non_causal":4,',
    '"dyads_improved_causal":3,"dyads_improved_non_causal":1,',
    '"mapping_coverage_causal":1.0,"mapping_coverage_non_causal":0.75,',
    '"feasible_causal":true,"feasible_non_causal":false,',
    '"crux_mode":"global","model_count":4,"dyad_count":12}',
    '],"crux_mode":"global","model_count":4,"dyad_count":12}}'
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
      result <- compute_delta_u(
        mock_delta_u_dyads(), crux_mode = "global", top_k = 2,
        url = "http://localhost:8000"
      )

      body <- captured$body$data
      expect_equal(body$crux_mode, "global")
      expect_identical(body$top_k, 2L)
      expect_false("global_status" %in% names(body))
      expect_equal(nrow(result), 1L)
      expect_equal(result$direction, "->")
      expect_equal(result$delta_u_causal, 0.30)
      expect_equal(result$delta_u_non_causal, 0.10)
      expect_equal(result$compatibility_change_causal, 0.30)
      expect_identical(result$dyads_improved_non_causal, 1L)
      expect_equal(result$best_resolution, "causal")
      expect_true(result$feasible_causal)
      expect_false(result$feasible_non_causal)
      expect_identical(result$model_count, 4L)
      expect_identical(result$dyad_count, 12L)
      expect_identical(result$crux_mode, "global")
    }
  )
})


test_that("Delta-U parsers preserve invalid and unmatched model ID lists", {
  expect_no_warning({
    single <- .parse_single_result(list(
      component_id = "C0004",
      invalid_models = list("M0001", "M0002"),
      unmatched_models = list(),
      invalid_models_causal = list("M0003"),
      unmatched_models_non_causal = list()
    ))
    rankings <- .parse_delta_u_rankings(list(list(
      rank = 1L,
      component_id = "C0004",
      type = "edge",
      source = "X",
      target = "Y",
      direction = "->",
      delta_u = 0.2,
      best_resolution = "causal",
      dyads_improved = 1L,
      dyads_worsened = 0L,
      invalid_models = list("M0001", "M0002"),
      unmatched_models = list(),
      invalid_models_causal = list("M0003"),
      unmatched_models_non_causal = list()
    )))
  })

  expect_type(single$invalid_models, "list")
  expect_identical(single$invalid_models[[1]], c("M0001", "M0002"))
  expect_identical(single$unmatched_models[[1]], character(0))
  expect_identical(single$invalid_models_causal[[1]], "M0003")
  expect_identical(single$unmatched_models_non_causal[[1]], character(0))

  expect_type(rankings$invalid_models, "list")
  expect_identical(rankings$invalid_models[[1]], c("M0001", "M0002"))
  expect_identical(rankings$unmatched_models[[1]], character(0))
  expect_identical(rankings$invalid_models_causal[[1]], "M0003")
  expect_identical(rankings$unmatched_models_non_causal[[1]], character(0))
})


test_that("Delta-U parsers preserve timing pruning lists, counts, and flags", {
  fields <- list(
    component_id = "C0004",
    timing_pruned_models_causal = list("M0001", "M0002"),
    timing_pruned_models_non_causal = list(),
    models_pruned_causal = 2,
    models_pruned_non_causal = 0,
    post_model_count_causal = 2,
    post_model_count_non_causal = 4,
    post_dyad_count_causal = 2,
    post_dyad_count_non_causal = 12,
    insufficient_post_models_causal = FALSE,
    insufficient_post_models_non_causal = TRUE
  )

  single <- .parse_single_result(fields)
  ranking <- .parse_delta_u_rankings(list(c(
    fields,
    list(
      rank = 1L,
      type = "edge",
      source = "X",
      target = "Y",
      direction = "->",
      delta_u = 0.2,
      best_resolution = "causal",
      dyads_improved = 1L,
      dyads_worsened = 0L
    )
  )))

  for (parsed in list(single, ranking)) {
    expect_type(parsed$timing_pruned_models_causal, "list")
    expect_identical(parsed$timing_pruned_models_causal[[1]], c("M0001", "M0002"))
    expect_type(parsed$timing_pruned_models_non_causal, "list")
    expect_identical(parsed$timing_pruned_models_non_causal[[1]], character(0))
    expect_identical(parsed$models_pruned_causal[[1]], 2L)
    expect_identical(parsed$models_pruned_non_causal[[1]], 0L)
    expect_identical(parsed$post_model_count_causal[[1]], 2L)
    expect_identical(parsed$post_model_count_non_causal[[1]], 4L)
    expect_identical(parsed$post_dyad_count_causal[[1]], 2L)
    expect_identical(parsed$post_dyad_count_non_causal[[1]], 12L)
    expect_false(parsed$insufficient_post_models_causal[[1]])
    expect_true(parsed$insufficient_post_models_non_causal[[1]])
  }
})


test_that("compute_delta_u request includes the selected causal metric", {
  captured <- NULL
  dyads <- mock_delta_u_dyads()
  dyads$mas_compatible <- c(TRUE, FALSE)
  context <- attr(dyads, "theory_context")
  context$exposure <- "X"
  context$outcome <- "Y"
  attr(dyads, "theory_context") <- context

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(paste0(
          '{"status":"success","data":{"rankings":[{"rank":1,',
          '"component_id":"C0004","type":"edge","source":"X","target":"Y",',
          '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,',
          '"dyads_worsened":0}],"component_count":4,"computation_mode":"exhaustive",',
           '"compatibility_metric":"mas_compatible","device":"cpu"}}'
        ))
      )
    },
    {
      compute_delta_u(
        dyads, top_k = 5, compatibility_metric = "mas_compatible",
        url = "http://localhost:8000"
      )
      body <- captured$body$data
       expect_equal(body$compatibility_metric, "mas_compatible")
       expect_equal(body$exposure, "X")
       expect_equal(body$outcome, "Y")
       expect_named(
        body$dyads[[1]],
        c("dyad_id", "ego_id", "alter_id", "similarity_rate", "mas_compatible")
      )
      expect_true(body$dyads[[1]]$mas_compatible)
    }
  )
})

test_that("compute_delta_u sends all available compatibility fields", {
  captured <- NULL
  dyads <- mock_delta_u_dyads()
  dyads$mas_compatible <- c(TRUE, FALSE)
  dyads$identified_compatible <- c(FALSE, TRUE)
  context <- attr(dyads, "theory_context")
  context$exposure <- "X"
  context$outcome <- "Y"
  attr(dyads, "theory_context") <- context

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(paste0(
          '{"status":"success","data":{"rankings":[{"rank":1,',
          '"component_id":"C0004","type":"edge","source":"X","target":"Y",',
          '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,',
          '"dyads_worsened":0}],"component_count":4,"computation_mode":"exhaustive",',
          '"compatibility_metric":"identified_compatible",',
           '"device":"auto"}}'
        ))
      )
    },
    {
      compute_delta_u(
        dyads, top_k = 5, compatibility_metric = "identified_compatible",
        url = "http://localhost:8000"
      )
      body <- captured$body$data
      expect_equal(body$compatibility_metric, "identified_compatible")
      expect_true(body$dyads[[2]]$identified_compatible)
      # All available metric fields are forwarded so the backend can derive
      # self-source dyad scores without recomputing causal profiles.
      expect_true("mas_compatible" %in% names(body$dyads[[2]]))
    }
  )
})

test_that("compute_delta_u supports exactly three compatibility metrics", {
  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), compatibility_metric = "full_compatible",
      url = "http://localhost:8000"
    ),
    "similarity_rate.*mas_compatible.*identified_compatible"
  )

  expect_error(
    compute_delta_u(
      mock_delta_u_dyads(), compatibility_metric = "mas_compatible",
      url = "http://localhost:8000"
    ),
    "requires exposure and outcome"
  )
})

test_that("compute_delta_u device = cuda is sent", {
  captured <- NULL

  httr2::with_mocked_responses(
    function(req) {
      captured <<- req
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(paste0(
          '{"status":"success","data":{"rankings":[{"rank":1,',
          '"component_id":"C0004","type":"edge","source":"X","target":"Y",',
          '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,',
          '"dyads_worsened":0}],"component_count":4,"computation_mode":"exhaustive",',
           '"compatibility_metric":"similarity_rate","device":"cuda"}}'
        ))
      )
    },
    {
      compute_delta_u(
        mock_delta_u_dyads(), top_k = 5, device = "cuda",
        url = "http://localhost:8000"
      )
      body <- captured$body$data
      expect_equal(body$device, "cuda")
    }
  )
})

test_that("compute_delta_u returned ranking has compatibility metric attribute", {
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"rankings":[{"rank":1,"component_id":"C0004","type":"edge","source":"X","target":"Y",',
    '"delta_u":0.25,"best_resolution":"positive","dyads_improved":2,"dyads_worsened":0}],',
    '"component_count":4,"computation_mode":"exhaustive",',
    '"compatibility_metric":"identified_compatible",',
     '"device":"cpu"}}'
  )

  dyads <- mock_delta_u_dyads()
  dyads$identified_compatible <- c(TRUE, FALSE)
  dyads$identified_ego <- c(TRUE, TRUE)
  dyads$identification_nodes_ego <- I(list(c("X", "Y"), c("X", "Y")))
  context <- attr(dyads, "theory_context")
  context$exposure <- "X"
  context$outcome <- "Y"
  attr(dyads, "theory_context") <- context

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      result <- compute_delta_u(
        dyads, top_k = 5, compatibility_metric = "identified_compatible",
        url = "http://localhost:8000"
      )
      expect_equal(attr(result, "compatibility_metric"),
                   "identified_compatible")
      expect_equal(attr(result, "device"), "cpu")
    }
  )
})


test_that("compute_delta_u preserves profile booleans and list shapes", {
  dyads <- mock_delta_u_dyads()
  dyads$identified_compatible <- c(FALSE, TRUE)
  dyads$identified_ego <- c(FALSE, TRUE)
  dyads$identified_alter <- c(TRUE, FALSE)
  dyads$identification_nodes_ego <- I(list(character(), "X"))
  dyads$identification_nodes_alter <- I(list("X", character()))

  records <- .delta_u_dyads_to_records(dyads)
  encoded <- jsonlite::toJSON(records, auto_unbox = TRUE, null = "null")

  expect_false(records[[1]]$identified_ego)
  expect_true(records[[1]]$identified_alter)
  expect_type(records[[1]]$identification_nodes_ego, "list")
  expect_match(encoded, '"identified_ego":false', fixed = TRUE)
  expect_false(grepl('"identified_ego":"FALSE"', encoded, fixed = TRUE))
})

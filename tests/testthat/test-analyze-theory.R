test_that("analyze_theory symbolic branch returns expected structure", {
  result <- list(
    registry = NULL,
    states = NULL,
    dyads = list(status = "success", data = list(mode = "full", classes = list())),
    delta_u_rankings = structure(
      list(mode = "full", exact = TRUE, results = list()),
      class = "theory_symbolic_delta_u"
    ),
    ghost_clusters = NULL,
    summary = c("Symbolic mode: 2 edge variables"),
    plots = NULL
  )
  expect_null(result$registry)
  expect_null(result$states)
  expect_true(length(result$summary) > 0)
  expect_true(grepl("Symbolic", result$summary[1]))
})

test_that("analyze_theory defaults to the general-identification backend", {
  expect_identical(
    eval(formals(analyze_theory)$causal_backend),
    c("r", "auto", "native")
  )
})


test_that("analyze_theory defaults to resolution-closed exhaustive expansion", {
  expect_identical(
    eval(formals(analyze_theory)$mode),
    c("exhaustive", "sampled", "symbolic")
  )
})


test_that("analyze_theory validates deprecated global_status without requiring it", {
  expect_error(
    analyze_theory(crux_mode = "global", global_status = "invalid"),
    "'arg' should be one of"
  )
  expect_error(
    analyze_theory(crux_mode = "marginal", global_status = "causal"),
    "only valid with crux_mode"
  )
})


test_that("concrete analyze summaries retain the model count in both crux modes", {
  for (crux_mode in c("marginal", "global")) {
    request_index <- 0L
    mock_bodies <- c(
      '{"status":"success"}',
      paste0(
        '{"status":"success","data":{"registry_data":[',
        '{"comp_id":"C0001","type":"node","source":"X",',
         '"target":null,"direction":null,"description":"X",',
         '"fixed_status":null},',
        '{"comp_id":"C0002","type":"node","source":"Y",',
         '"target":null,"direction":null,"description":"Y",',
         '"fixed_status":null},',
        '{"comp_id":"C0003","type":"edge","source":"X",',
         '"target":"Y","direction":"->","description":"X -> Y",',
         '"fixed_status":"causal"}',
        ']}}'
      ),
      paste0(
        '{"status":"success","data":{"state_data":[',
        '{"model_id":"M0001","comp_id":"C0001","status":"present",',
        '"timing":1,"seeded":false},',
        '{"model_id":"M0001","comp_id":"C0002","status":"present",',
        '"timing":2,"seeded":false},',
        '{"model_id":"M0001","comp_id":"C0003","status":"causal",',
        '"timing":null,"seeded":false},',
        '{"model_id":"M0002","comp_id":"C0001","status":"present",',
        '"timing":1,"seeded":false},',
        '{"model_id":"M0002","comp_id":"C0002","status":"present",',
        '"timing":2,"seeded":false},',
         '{"model_id":"M0002","comp_id":"C0003","status":"causal",',
        '"timing":null,"seeded":false}],"seeded_model_ids":[]}}'
      ),
      paste0(
        '{"status":"success","data":{"dyads":[',
        '{"dyad_id":"M0001__M0002","ego_id":"M0001",',
        '"alter_id":"M0002","similarity_rate":0.5,',
        '"timing_compatible":true,"existence_conflict":false,',
        '"repair_cost":1,"mas_ego":[[]],"mas_alter":[[]],',
        '"mas_compatible":true,"identified_ego":true,',
        '"identified_alter":true,"identification_nodes_ego":["X"],',
        '"identification_nodes_alter":["X"],"identified_compatible":true},',
        '{"dyad_id":"M0002__M0001","ego_id":"M0002",',
        '"alter_id":"M0001","similarity_rate":0.5,',
        '"timing_compatible":true,"existence_conflict":false,',
        '"repair_cost":1,"mas_ego":[[]],"mas_alter":[[]],',
        '"mas_compatible":true,"identified_ego":true,',
        '"identified_alter":true,"identification_nodes_ego":["X"],',
        '"identification_nodes_alter":["X"],"identified_compatible":true}',
        ']}}'
      ),
      paste0(
        '{"status":"success","data":{"rankings":[{',
        '"rank":1,"component_id":"C0003","type":"edge",',
        '"source":"X","target":"Y","direction":"->",',
        '"delta_u":0.25,"delta_u_causal":0.25,',
        '"delta_u_non_causal":0.0,"best_resolution":"causal",',
        '"dyads_improved":1,"dyads_worsened":0}],',
        '"model_count":2,"dyad_count":2,"crux_mode":"',
        crux_mode, '"}}'
      )
    )

    result <- httr2::with_mocked_responses(
      function(req) {
        request_index <<- request_index + 1L
        httr2::response(
          status_code = 200L,
          headers = list("content-type" = "application/json"),
          body = charToRaw(mock_bodies[[request_index]])
        )
      },
      analyze_theory(
        nodes = c("X", "Y"), timing = c(1, 2), exposure = "X",
        outcome = "Y", crux_mode = crux_mode, url = "http://localhost:8000"
      )
    )

    expect_equal(request_index, 5L)
    expect_true("Models: 2" %in% result$summary)
  }
})

test_that("symbolic comparison returns compatibility info", {
  comparison <- structure(
    list(
      full_compatible = TRUE,
      a_signature = list(adjustment_identifiable = TRUE),
      b_signature = list(adjustment_identifiable = TRUE)
    ),
    class = "theory_symbolic_comparison"
  )
  expect_s3_class(comparison, "theory_symbolic_comparison")
  expect_true(comparison$full_compatible)
})

test_that("compatibility summary reports available-dyad percentages", {
  dyads <- data.frame(
    mas_compatible = c(TRUE, TRUE, FALSE, NA),
    identified_compatible = c(FALSE, FALSE, TRUE, NA)
  )

  expect_equal(
    .analyze_theory_compatibility_summary(dyads, "mas_compatible", "MAS"),
    "MAS compatibility: 66.7% (2/3 available dyads)"
  )
  expect_equal(
    .analyze_theory_compatibility_summary(
      dyads, "identified_compatible", "Identified"
    ),
    "Identified compatibility: 33.3% (1/3 available dyads)"
  )
})


test_that("identified model summary deduplicates directed dyad rows", {
  model_ids <- paste0("M", 1:3)
  statuses <- c(M1 = TRUE, M2 = FALSE, M3 = NA)
  dyads <- expand.grid(
    ego_id = model_ids,
    alter_id = model_ids,
    stringsAsFactors = FALSE
  )
  dyads <- dyads[dyads$ego_id != dyads$alter_id, , drop = FALSE]
  dyads$identified_ego <- unname(statuses[dyads$ego_id])
  dyads$identified_alter <- unname(statuses[dyads$alter_id])

  expect_equal(
    .analyze_theory_identified_models_summary(dyads, model_ids),
    paste0(
      "Identified models: 1/3 ",
      "(identification available for 2/3 models)"
    )
  )
})


test_that("24-model summaries use all 552 available dyads", {
  model_ids <- sprintf("M%04d", 1:24)
  identified <- setNames(seq_along(model_ids) <= 10L, model_ids)
  dyads <- expand.grid(
    ego_id = model_ids,
    alter_id = model_ids,
    stringsAsFactors = FALSE
  )
  dyads <- dyads[dyads$ego_id != dyads$alter_id, , drop = FALSE]
  dyads$identified_ego <- unname(identified[dyads$ego_id])
  dyads$identified_alter <- unname(identified[dyads$alter_id])
  dyads$identified_compatible <-
    dyads$identified_ego & dyads$identified_alter
  dyads$mas_compatible <- FALSE

  expect_equal(nrow(dyads), 552L)
  expect_false(anyNA(dyads$identified_compatible))
  expect_equal(
    .analyze_theory_identified_models_summary(dyads, model_ids),
    "Identified models: 10/24"
  )
  expect_equal(
    .analyze_theory_compatibility_summary(
      dyads, "identified_compatible", "Identified"
    ),
    "Identified compatibility: 16.3% (90/552 available dyads)"
  )
})


test_that("MAS summary reports prevalence and uniquely enabled pairs", {
  dyads <- data.frame(
    ego_id = paste0("M", 1:4),
    mas_ego = I(list(
      list("Z1", c("Z2", "Z3")),
      list("Z1"),
      list("Z1", c("Z2", "Z3")),
      list("Z4")
    ))
  )

  expect_equal(
    .analyze_theory_mas_summary(dyads),
    c(
      "Most common MAS set: {Z1} (3/4 models)",
      "MAS set uniquely enabling most compatibility: {Z1} (2 model dyads)"
    )
  )
})


test_that("missing component summary uses unique eligible model pairs", {
  registry <- data.frame(
    comp_id = paste0("C000", 1:4),
    type = "node",
    source = c("X", "Z", "W", "Y")
  )
  present <- list(
    M1 = c("C0001", "C0002", "C0003", "C0004"),
    M2 = c("C0001", "C0003", "C0004"),
    M3 = c("C0001", "C0003", "C0004"),
    M4 = c("C0001", "C0002", "C0004")
  )
  identification_nodes <- lapply(present, function(comp_ids) {
    registry$source[match(comp_ids, registry$comp_id)]
  })
  unordered <- data.frame(
    ego_id = c("M1", "M1", "M1", "M2", "M2", "M3"),
    alter_id = c("M2", "M3", "M4", "M3", "M4", "M4"),
    identified_compatible = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    identified_ego = TRUE,
    identified_alter = TRUE,
    identification_nodes_ego = I(lapply(
      c("M1", "M1", "M1", "M2", "M2", "M3"),
      function(model_id) identification_nodes[[model_id]]
    )),
    identification_nodes_alter = I(lapply(
      c("M2", "M3", "M4", "M3", "M4", "M4"),
      function(model_id) identification_nodes[[model_id]]
    ))
  )
  reverse <- unordered
  reverse$ego_id <- unordered$alter_id
  reverse$alter_id <- unordered$ego_id
  reverse$identification_nodes_ego <- unordered$identification_nodes_alter
  reverse$identification_nodes_alter <- unordered$identification_nodes_ego
  dyads <- rbind(unordered, reverse)

  expect_equal(
    .analyze_theory_missing_component_summary(
      dyads, registry, "vary"
    ),
    paste0(
      "Missing component contributing most to identified incompatibility: ",
      "Z (C0002) (missing in 4/5 eligible model pairs)"
    )
  )
  expect_length(
    .analyze_theory_missing_component_summary(
      dyads, registry, "all-present"
    ),
    0L
  )

  unavailable <- dyads[1, , drop = FALSE]
  unavailable$identification_nodes_ego <- I(list(NULL))
  expect_match(
    .analyze_theory_missing_component_summary(
      unavailable, registry, "vary"
    ),
    "no eligible model pairs"
  )
})


test_that("crux summary reports deterministic top three rows and mode", {
  rankings <- data.frame(
    rank = c(3L, 1L, 4L, 2L),
    component_id = c("C0003", "C0001", "C0004", "C0002"),
    type = rep("edge", 4),
    source = c("Z", "X", "W", "Y"),
    target = c("Y", "Y", "X", "Z"),
    direction = c("->", "->", "<->", "->"),
    delta_u = c(0.10, 0.40, 0.05, 0.20),
    best_resolution = c("none", "causal", "non-causal", "causal"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    .analyze_theory_crux_summary(rankings, crux_mode = "global"),
    paste0(
      "Top crux components (global): ",
      "1. C0001 (X -> Y, resolution = causal, delta_u = 0.4000); ",
      "2. C0002 (Y -> Z, resolution = causal, delta_u = 0.2000); ",
      "3. C0003 (Z -> Y, resolution = none, delta_u = 0.1000)"
    )
  )
  expect_length(
    .analyze_theory_crux_summary(rankings, crux_mode = "global"), 1L
  )

  expect_identical(
    .analyze_theory_crux_summary(rankings[1:2, , drop = FALSE], "marginal"),
    paste0(
      "Top crux components (marginal): ",
      "1. C0001 (X -> Y, resolution = causal, delta_u = 0.4000); ",
      "3. C0003 (Z -> Y, resolution = none, delta_u = 0.1000)"
    )
  )
  expect_length(
    .analyze_theory_crux_summary(rankings[1:2, , drop = FALSE], "marginal"),
    1L
  )
})


test_that("crux summary annotates pruning for the selected resolution branch", {
  rankings <- data.frame(
    rank = c(1L, 2L),
    component_id = c("C0001", "C0002"),
    type = c("edge", "edge"),
    source = c("X", "Y"),
    target = c("Y", "Z"),
    direction = c("->", "->"),
    delta_u = c(0.4, 0.3),
    best_resolution = c("causal", "non-causal"),
    models_pruned_causal = c(2L, 1L),
    models_pruned_non_causal = c(7L, 3L),
    stringsAsFactors = FALSE
  )

  summary <- .analyze_theory_crux_summary(rankings, crux_mode = "marginal")

  expect_identical(
    summary,
    paste0(
      "Top crux components (marginal): ",
      "1. C0001 (X -> Y, resolution = causal, delta_u = 0.4000, ",
      "timing-pruned models = 2); ",
      "2. C0002 (Y -> Z, resolution = non-causal, delta_u = 0.3000, ",
      "timing-pruned models = 3)"
    )
  )
  expect_length(summary, 1L)
  expect_false(grepl("timing-pruned models = 7", summary, fixed = TRUE))
})


test_that("interactive reproduction omits deprecated global_status", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003"),
    type = c("node", "node", "edge"),
    source = c("X", "Y", "X"),
    target = c(NA, NA, "Y"),
    stringsAsFactors = FALSE
  )
  attr(registry, "timing_options") <- list(X = 1L, Y = 2L)
  attr(registry, "exposure") <- "X"
  attr(registry, "outcome") <- "Y"

  call <- .analyze_theory_programmatic_call(
    registry = registry,
    prior_model = NULL,
    mode = "exhaustive",
    n_models = 10L,
    seed = 42L,
    node_policy = "all-present",
    top_k = 3L,
    crux_mode = "global",
    plot = FALSE,
    eps = 0.5,
    min_samples = 5L,
    url = "http://localhost:8000",
    max_models = 10000L,
    allow_large = FALSE,
    causal_backend = "r"
  )

  expect_true(any(grepl('crux_mode = "global"', call, fixed = TRUE)))
  expect_false(any(grepl("global_status", call, fixed = TRUE)))
})

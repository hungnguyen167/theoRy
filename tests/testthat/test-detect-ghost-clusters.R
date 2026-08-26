mock_clustering_dyads <- function() {
  dyads <- data.frame(
    dyad_id = c("M0001__M0002", "M0002__M0001"),
    ego_id = c("M0001", "M0002"),
    alter_id = c("M0002", "M0001"),
    similarity_rate = c(0.8, 0.8),
    timing_compatible = c(TRUE, TRUE),
    existence_conflict = c(FALSE, FALSE),
    repair_cost = c(0L, 0L),
    stringsAsFactors = FALSE
  )
  attr(dyads, "theory_context") <- list(
    registry_data = list(
      list(comp_id = "C0001", type = "node", source = "X",
           target = NULL, direction = NULL, description = "X"),
      list(comp_id = "C0002", type = "node", source = "Y",
           target = NULL, direction = NULL, description = "Y"),
      list(comp_id = "C0003", type = "edge", source = "X",
           target = "Y", direction = "->", description = "X -> Y",
           fixed_status = "causal")
    ),
    state_data = list(
      list(model_id = "M0001", comp_id = "C0001", status = "causal", timing = 1L),
      list(model_id = "M0001", comp_id = "C0002", status = "causal", timing = 2L),
      list(model_id = "M0001", comp_id = "C0003", status = "causal"),
      list(model_id = "M0002", comp_id = "C0001", status = "causal", timing = 1L),
      list(model_id = "M0002", comp_id = "C0002", status = "causal", timing = 2L),
      list(model_id = "M0002", comp_id = "C0003", status = "causal")
    ),
    model_ids = c("M0001", "M0002"),
    exposure = "X",
    outcome = "Y"
  )
  dyads
}


test_that("detect_ghost_clusters with prior returns ghost clusters", {
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"cluster_assignments":[',
    '{"model_id":"M0001","cluster_id":"Cluster_01"},',
    '{"model_id":"M0002","cluster_id":"Cluster_01"}',
    '],',
    '"cluster_summaries":[',
    '{"cluster_id":"Cluster_01","model_count":2,"internal_compatibility":0.85}',
    '],',
    '"ghost_clusters":[',
    '{"cluster_id":"Cluster_01","model_count":2,"internal_compatibility":0.85,',
    '"prior_compatibility":0.2,"prior_distance":0.8,"label":"ghost",',
    '"representative_models":["M0001","M0002"]}',
    '],',
    '"embedding_2d":{',
    '"model_ids":["M0001","M0002"],',
    '"x":[0.1,0.2],',
    '"y":[0.3,0.4]',
    '},',
    '"model_count":2,',
    '"cluster_count":1,',
    '"noise_count":0',
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
      result <- detect_ghost_clusters(mock_clustering_dyads(),
                                       prior_model = "M0001",
                                       url = "http://localhost:8000")

      expect_type(result, "list")
      expect_named(result, c("cluster_assignments", "cluster_summaries",
                              "ghost_clusters", "embedding_2d"))

      expect_s3_class(result$cluster_assignments, "data.frame")
      expect_equal(nrow(result$cluster_assignments), 2)
      expect_named(result$cluster_assignments, c("model_id", "cluster_id"))

      expect_s3_class(result$cluster_summaries, "data.frame")
      expect_equal(nrow(result$cluster_summaries), 1)
      expect_equal(result$cluster_summaries$cluster_id[1], "Cluster_01")
      expect_equal(result$cluster_summaries$internal_compatibility[1], 0.85)

      expect_s3_class(result$ghost_clusters, "data.frame")
      expect_equal(nrow(result$ghost_clusters), 1)
      expect_equal(result$ghost_clusters$label[1], "ghost")
      expect_equal(result$ghost_clusters$prior_compatibility[1], 0.2)

      expect_s3_class(result$embedding_2d, "data.frame")
      expect_equal(nrow(result$embedding_2d), 2)
      expect_named(result$embedding_2d, c("model_id", "x", "y"))
    }
  )
})


test_that("detect_ghost_clusters without prior returns empty ghost_clusters", {
  mock_body <- paste0(
    '{"status":"success","data":{',
    '"cluster_assignments":[',
    '{"model_id":"M0001","cluster_id":"Cluster_01"},',
    '{"model_id":"M0002","cluster_id":"Cluster_01"}',
    '],',
    '"cluster_summaries":[',
    '{"cluster_id":"Cluster_01","model_count":2,"internal_compatibility":0.8}',
    '],',
    '"ghost_clusters":[],',
    '"embedding_2d":{',
    '"model_ids":["M0001","M0002"],',
    '"x":[0.1,0.2],',
    '"y":[0.3,0.4]',
    '},',
    '"model_count":2,',
    '"cluster_count":1,',
    '"noise_count":0',
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
      result <- detect_ghost_clusters(mock_clustering_dyads(),
                                       url = "http://localhost:8000")

      expect_type(result, "list")
      expect_s3_class(result$ghost_clusters, "data.frame")
      expect_equal(nrow(result$ghost_clusters), 0)
      expect_named(result$ghost_clusters,
                   c("cluster_id", "model_count", "internal_compatibility",
                     "prior_compatibility", "prior_distance", "label",
                     "representative_models"))
    }
  )
})


test_that("detect_ghost_clusters no dyads error is mapped", {
  mock_body <- '{"status":"error","code":"NO_DYADS","message":"Run dyad-matrix computation first"}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 400L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      expect_error(
        detect_ghost_clusters(mock_clustering_dyads(),
                               url = "http://localhost:8000"),
        "No dyad records available"
      )
    }
  )
})


test_that("detect_ghost_clusters invalid eps fails before HTTP", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), eps = -0.1),
    "eps must be positive"
  )
})


test_that("detect_ghost_clusters invalid min_samples fails before HTTP", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), min_samples = 1),
    "min_samples must be at least 2"
  )
})


test_that("detect_ghost_clusters invalid umap_components fails before HTTP", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), umap_components = 5),
    "umap_components must be 2 or 3"
  )
})


test_that("detect_ghost_clusters backend error is mapped", {
  mock_body <- '{"status":"error","code":"CLUSTERING_ERROR","message":"Internal error"}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 500L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      expect_error(
        detect_ghost_clusters(mock_clustering_dyads(),
                               url = "http://localhost:8000"),
        "Backend error \\[CLUSTERING_ERROR\\]: Internal error"
      )
    }
  )
})


test_that("detect_ghost_clusters model not found is mapped", {
  mock_body <- '{"status":"error","code":"MODEL_NOT_FOUND","message":"Model INVALID not found"}'

  httr2::with_mocked_responses(
    function(req) {
      httr2::response(
        status_code = 422L,
        headers = list("content-type" = "application/json"),
        body = charToRaw(mock_body)
      )
    },
    {
      expect_error(
        detect_ghost_clusters(mock_clustering_dyads(),
                               prior_model = "INVALID",
                               url = "http://localhost:8000"),
        "Prior model INVALID not found"
      )
    }
  )
})


test_that("detect_ghost_clusters empty ghost data frame has correct schema", {
  result <- list(
    cluster_assignments = data.frame(
      model_id = character(0),
      cluster_id = character(0),
      stringsAsFactors = FALSE
    ),
    cluster_summaries = data.frame(
      cluster_id = character(0),
      model_count = integer(0),
      internal_compatibility = numeric(0),
      stringsAsFactors = FALSE
    ),
    ghost_clusters = data.frame(
      cluster_id = character(0),
      model_count = integer(0),
      internal_compatibility = numeric(0),
      prior_compatibility = numeric(0),
      prior_distance = numeric(0),
      label = character(0),
      representative_models = I(list()),
      stringsAsFactors = FALSE
    ),
    embedding_2d = data.frame(
      model_id = character(0),
      x = numeric(0),
      y = numeric(0),
      stringsAsFactors = FALSE
    )
  )

  expect_s3_class(result$ghost_clusters, "data.frame")
  expect_equal(nrow(result$ghost_clusters), 0)
  expect_named(result$ghost_clusters,
               c("cluster_id", "model_count", "internal_compatibility",
                 "prior_compatibility", "prior_distance", "label",
                 "representative_models"))
})


test_that("detect_ghost_clusters missing theory_context fails", {
  dyads <- data.frame(
    dyad_id = "M0001__M0002",
    ego_id = "M0001",
    alter_id = "M0002",
    similarity_rate = 0.8,
    stringsAsFactors = FALSE
  )

  expect_error(
    detect_ghost_clusters(dyads),
    "missing theory_context"
  )
})


test_that("detect_ghost_clusters invalid internal_threshold fails before HTTP", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), internal_threshold = 1.5),
    "internal_threshold must be between 0 and 1"
  )
})


test_that("detect_ghost_clusters invalid prior_threshold fails before HTTP", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), prior_threshold = -0.1),
    "prior_threshold must be between 0 and 1"
  )
})


test_that("detect_ghost_clusters accepts exactly three score fields", {
  expect_error(
    detect_ghost_clusters(mock_clustering_dyads(), score_field = ""),
    paste0(
      "score_field must be one of: similarity_rate, mas_compatible, ",
      "identified_compatible"
    )
  )
})

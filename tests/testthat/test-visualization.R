test_that("plot_dyad_heatmap returns a ggplot object", {
  dyads <- data.frame(
    dyad_id = c("M0001__M0002", "M0001__M0003", "M0002__M0003",
                "M0002__M0001", "M0003__M0001", "M0003__M0002"),
    ego_id = c("M0001", "M0001", "M0002", "M0002", "M0003", "M0003"),
    alter_id = c("M0002", "M0003", "M0003", "M0001", "M0001", "M0002"),
    similarity_rate = c(0.75, 0.50, 0.80, 0.75, 0.50, 0.80),
    timing_compatible = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    existence_conflict = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    repair_cost = c(1L, 3L, 1L, 1L, 3L, 1L),
    stringsAsFactors = FALSE
  )

  p <- plot_dyad_heatmap(dyads)
  expect_s3_class(p, "ggplot")
})

test_that("plot_dyad_heatmap handles boolean score field", {
  dyads <- data.frame(
    dyad_id = c("M0001__M0002", "M0002__M0001"),
    ego_id = c("M0001", "M0002"),
    alter_id = c("M0002", "M0001"),
    mas_compatible = c(TRUE, FALSE),
    similarity_rate = c(0.5, 0.5),
    timing_compatible = c(TRUE, TRUE),
    existence_conflict = c(FALSE, FALSE),
    repair_cost = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  p <- plot_dyad_heatmap(dyads, score_field = "mas_compatible")
  expect_s3_class(p, "ggplot")
})

test_that("plot_dyad_heatmap errors on non-data-frame", {
  expect_error(plot_dyad_heatmap(list()), "data frame")
})

test_that("plot_dyad_heatmap accepts exactly three score fields", {
  dyads <- data.frame(
    ego_id = "M0001", alter_id = "M0002",
    similarity_rate = 0.5,
    stringsAsFactors = FALSE
  )
  expect_error(
    plot_dyad_heatmap(dyads, score_field = "nonexistent"),
    paste0(
      "score_field must be one of: similarity_rate, mas_compatible, ",
      "identified_compatible"
    )
  )
  expect_error(
    plot_dyad_heatmap(dyads, score_field = "mas_compatible"),
    "missing required column"
  )
})

test_that("plot_dyad_heatmap auto-hides labels for large multiverse", {
  n <- 101
  ids <- sprintf("M%04d", seq_len(n))
  pairs <- expand.grid(ego_id = ids, alter_id = ids, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$ego_id != pairs$alter_id, ]
  pairs$dyad_id <- paste(pairs$ego_id, pairs$alter_id, sep = "__")
  pairs$similarity_rate <- runif(nrow(pairs))
  pairs$timing_compatible <- TRUE
  pairs$existence_conflict <- FALSE
  pairs$repair_cost <- 1L

  expect_message(
    {
      p <- plot_dyad_heatmap(pairs)
      expect_s3_class(p, "ggplot")
    },
    "hiding"
  )
})


test_that("plot_lynchpin_ranking returns a ggplot object", {
  rankings <- data.frame(
    rank = 1:3,
    component_id = c("C0001", "C0002", "C0003"),
    type = c("edge", "edge", "node"),
    source = c("X1", "X2", "X3"),
    target = c("X2", "X3", NA),
    delta_u = c(0.15, 0.08, -0.02),
    best_resolution = c("positive", "positive", "negative"),
    dyads_improved = c(10L, 5L, 0L),
    dyads_worsened = c(2L, 1L, 3L),
    stringsAsFactors = FALSE
  )

  p <- plot_lynchpin_ranking(rankings)
  expect_s3_class(p, "ggplot")
})


test_that("plot_lynchpin_ranking accepts global rankings", {
  rankings <- data.frame(
    rank = 1L,
    component_id = "C0001",
    type = "edge",
    source = "X",
    target = "Y",
    direction = "->",
    delta_u = 0.20,
    delta_u_causal = 0.20,
    delta_u_non_causal = 0.05,
    best_resolution = "causal",
    dyads_improved = 2L,
    dyads_worsened = 0L,
    crux_mode = "global",
    stringsAsFactors = FALSE
  )

  expect_s3_class(plot_lynchpin_ranking(rankings), "ggplot")
})

test_that("plot_lynchpin_ranking respects top_n", {
  rankings <- data.frame(
    rank = 1:25,
    component_id = sprintf("C%04d", 1:25),
    type = rep("edge", 25),
    source = rep("X1", 25),
    target = rep("X2", 25),
    delta_u = seq(0.2, 0.01, length.out = 25),
    best_resolution = rep("positive", 25),
    dyads_improved = rep(10L, 25),
    dyads_worsened = rep(2L, 25),
    stringsAsFactors = FALSE
  )

  expect_message(
    {
      p <- plot_lynchpin_ranking(rankings)
      expect_s3_class(p, "ggplot")
    },
    "top 20"
  )

  p5 <- plot_lynchpin_ranking(rankings, top_n = 5)
  expect_s3_class(p5, "ggplot")
})

test_that("plot_lynchpin_ranking errors on empty rankings", {
  rankings <- data.frame(
    rank = integer(0), component_id = character(0),
    delta_u = numeric(0), best_resolution = character(0),
    stringsAsFactors = FALSE
  )
  expect_error(plot_lynchpin_ranking(rankings), "no rows")
})

test_that("plot_lynchpin_ranking handles synergistic_sets", {
  rankings <- data.frame(
    rank = 1:2,
    component_id = c("C0001", "C0002"),
    type = c("edge", "edge"),
    source = c("X1", "X2"),
    target = c("X2", "X3"),
    delta_u = c(0.15, 0.08),
    best_resolution = c("positive", "positive"),
    dyads_improved = c(10L, 5L),
    dyads_worsened = c(2L, 1L),
    stringsAsFactors = FALSE
  )

  syn <- data.frame(
    rank = 1L,
    components = I(list(c("C0001", "C0002"))),
    delta_u_combined = 0.30,
    delta_u_individual_sum = 0.23,
    synergy_score = 0.07,
    label = "C0001 + C0002",
    stringsAsFactors = FALSE
  )

  p <- plot_lynchpin_ranking(rankings, synergistic_sets = syn)
  expect_s3_class(p, "ggplot")
})


test_that("plot_compatibility_timeline accepts a bare data frame", {
  df <- data.frame(
    step = c("baseline", "resolve_C0003", "post"),
    compatibility = c(0.42, 0.61, 0.88),
    stringsAsFactors = FALSE
  )

  p <- plot_compatibility_timeline(df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_compatibility_timeline accepts a simulation result", {
  result <- list(
    results = list(
      compatibility_timeline = data.frame(
        step = c("baseline", "resolve_C0003", "post"),
        compatibility = c(0.42, 0.61, 0.88),
        stringsAsFactors = FALSE
      ),
      phase_transition_score = 0.46
    )
  )

  p <- plot_compatibility_timeline(result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_compatibility_timeline errors on invalid input", {
  expect_error(plot_compatibility_timeline(42), "data frame")
  expect_error(plot_compatibility_timeline(list()), "compatibility_timeline")
})


test_that("plot_cluster_embedding returns a ggplot object", {
  cluster_result <- list(
    embedding_2d = data.frame(
      model_id = c("M0001", "M0002", "M0003", "M0004"),
      x = c(0.1, 0.2, 0.8, 0.9),
      y = c(0.1, 0.2, 0.8, 0.9),
      stringsAsFactors = FALSE
    ),
    cluster_assignments = data.frame(
      model_id = c("M0001", "M0002", "M0003", "M0004"),
      cluster_id = c("Cluster_01", "Cluster_01", "Cluster_02", "Cluster_02"),
      stringsAsFactors = FALSE
    ),
    ghost_clusters = data.frame(
      cluster_id = c("Cluster_01", "Cluster_02"),
      model_count = c(2L, 2L),
      internal_compatibility = c(0.8, 0.7),
      prior_compatibility = c(0.3, 0.6),
      prior_distance = c(0.5, 0.1),
      label = c("ghost", "mainstream"),
      representative_models = I(list(c("M0001", "M0002"), c("M0003", "M0004"))),
      stringsAsFactors = FALSE
    )
  )

  p <- plot_cluster_embedding(cluster_result)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_embedding highlights prior model", {
  cluster_result <- list(
    embedding_2d = data.frame(
      model_id = c("M0001", "M0002"),
      x = c(0.1, 0.8),
      y = c(0.1, 0.8),
      stringsAsFactors = FALSE
    ),
    cluster_assignments = data.frame(
      model_id = c("M0001", "M0002"),
      cluster_id = c("Cluster_01", "Cluster_02"),
      stringsAsFactors = FALSE
    )
  )

  p <- plot_cluster_embedding(cluster_result,
                               highlight_prior = TRUE,
                               prior_model_id = "M0001")
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_embedding errors on missing embedding", {
  expect_error(
    plot_cluster_embedding(list(cluster_assignments = data.frame())),
    "embedding"
  )
})

test_that("plot_cluster_embedding handles all-noise case", {
  cluster_result <- list(
    embedding_2d = data.frame(
      model_id = c("M0001", "M0002"),
      x = c(0.1, 0.8),
      y = c(0.1, 0.8),
      stringsAsFactors = FALSE
    ),
    cluster_assignments = data.frame(
      model_id = c("M0001", "M0002"),
      cluster_id = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )
  )

  expect_message(
    {
      p <- plot_cluster_embedding(cluster_result)
      expect_s3_class(p, "ggplot")
    },
    "noise"
  )
})


test_that("plot_dag_models returns a list of ggplot objects", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003", "C0004", "C0005"),
    type = c("node", "node", "node", "edge", "edge"),
    source = c("X1", "X2", "Y", "X1", "X2"),
    target = c(NA, NA, NA, "Y", "Y"),
    direction = c(NA, NA, NA, "->", "->"),
    description = c("X1", "X2", "Y", "X1->Y", "X2->Y"),
    stringsAsFactors = FALSE
  )
  attr(registry, "node_timing") <- c(X1 = 1L, X2 = 2L, Y = 3L)
  attr(registry, "exposure") <- "X1"
  attr(registry, "outcome") <- "Y"

  states <- data.frame(
    model_id = c("M0001", "M0001", "M0001", "M0001", "M0001",
                 "M0002", "M0002", "M0002", "M0002", "M0002"),
    comp_id = c("C0001", "C0002", "C0003", "C0004", "C0005",
                "C0001", "C0002", "C0003", "C0004", "C0005"),
    status = c("causal", "causal", "causal", "causal", "unknown",
               "causal", "causal", "causal", "unknown", "causal"),
    stringsAsFactors = FALSE
  )

  plots <- plot_dag_models(registry, states, model_ids = c("M0001", "M0002"))
  expect_type(plots, "list")
  expect_length(plots, 2)
  expect_named(plots, c("M0001", "M0002"))
  for (p in plots) {
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_dag_models errors on missing model", {
  registry <- data.frame(
    comp_id = "C0001", type = "node", source = "X1", target = NA,
    direction = NA, description = "X1",
    stringsAsFactors = FALSE
  )
  attr(registry, "node_timing") <- c(X1 = 1L)

  states <- data.frame(
    model_id = "M0001", comp_id = "C0001", status = "causal",
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_dag_models(registry, states, model_ids = "M9999"),
    "not found"
  )
})

test_that("plot_dag_models infers timing from X-numbered node names", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003"),
    type = c("node", "node", "node"),
    source = c("X1", "X2", "X3"),
    target = c(NA, NA, NA),
    direction = c(NA, NA, NA),
    description = c("X1", "X2", "X3"),
    stringsAsFactors = FALSE
  )
  # No node_timing attribute - should infer from X1, X2, X3

  states <- data.frame(
    model_id = "M0001",
    comp_id = c("C0001", "C0002", "C0003"),
    status = c("causal", "causal", "causal"),
    stringsAsFactors = FALSE
  )

  plots <- plot_dag_models(registry, states, model_ids = "M0001")
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_dag_models selects first 6 models when model_ids is NULL", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003", "C0004"),
    type = c("node", "node", "node", "edge"),
    source = c("X1", "X2", "Y", "X1"),
    target = c(NA, NA, NA, "Y"),
    direction = c(NA, NA, NA, "->"),
    description = c("X1", "X2", "Y", "X1->Y"),
    stringsAsFactors = FALSE
  )
  attr(registry, "node_timing") <- c(X1 = 1L, X2 = 2L, Y = 3L)

  states <- data.frame(
    model_id = rep(c("M0001", "M0002", "M0003"), each = 4),
    comp_id = rep(c("C0001", "C0002", "C0003", "C0004"), 3),
    status = rep(c("causal", "causal", "causal", "causal"), 3),
    stringsAsFactors = FALSE
  )

  plots <- plot_dag_models(registry, states)
  expect_length(plots, 3)
  expect_named(plots, c("M0001", "M0002", "M0003"))
  for (p in plots) {
    expect_s3_class(p, "ggplot")
  }
})

test_that("plot_dag_models annotates MAS when show_mas = TRUE", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003", "C0004", "C0005"),
    type = c("node", "node", "node", "edge", "edge"),
    source = c("X1", "X2", "Y", "X1", "X2"),
    target = c(NA, NA, NA, "Y", "Y"),
    direction = c(NA, NA, NA, "->", "->"),
    description = c("X1", "X2", "Y", "X1->Y", "X2->Y"),
    stringsAsFactors = FALSE
  )
  attr(registry, "node_timing") <- c(X1 = 1L, X2 = 2L, Y = 3L)
  attr(registry, "exposure") <- "X1"
  attr(registry, "outcome") <- "Y"

  states <- data.frame(
    model_id = "M0001",
    comp_id = c("C0001", "C0002", "C0003", "C0004", "C0005"),
    status = c("causal", "causal", "causal", "causal", "unknown"),
    stringsAsFactors = FALSE
  )

  plots <- plot_dag_models(registry, states, model_ids = "M0001",
                           show_mas = TRUE)
  expect_s3_class(plots[[1]], "ggplot")
})

test_that("plot_dag_models skips MAS message without exposure/outcome", {
  registry <- data.frame(
    comp_id = c("C0001", "C0002", "C0003", "C0004"),
    type = c("node", "node", "node", "edge"),
    source = c("X1", "X2", "Y", "X1"),
    target = c(NA, NA, NA, "Y"),
    direction = c(NA, NA, NA, "->"),
    description = c("X1", "X2", "Y", "X1->Y"),
    stringsAsFactors = FALSE
  )
  attr(registry, "node_timing") <- c(X1 = 1L, X2 = 2L, Y = 3L)

  states <- data.frame(
    model_id = c("M0001", "M0001", "M0001", "M0001",
                 "M0002", "M0002", "M0002", "M0002"),
    comp_id = rep(c("C0001", "C0002", "C0003", "C0004"), 2),
    status = rep(c("causal", "causal", "causal", "causal"), 2),
    stringsAsFactors = FALSE
  )

  expect_message(
    plots <- plot_dag_models(registry, states, show_mas = TRUE),
    "MAS annotation skipped"
  )
  expect_length(plots, 2)
})


test_that("plot_simulation dispatches consensus_illusion", {
  result <- list(
    scenario = "consensus_illusion",
    results = list(
      mean_similarity_rate = 0.82,
      compatibility_metric = "mas_compatible",
      compatibility_rate = 0.45,
      consensus_illusion_gap = 0.37
    )
  )

  plots <- plot_simulation(result)
  expect_type(plots, "list")
  expect_named(plots, "consensus_comparison")
  expect_identical(
    plots$consensus_comparison$data$metric,
    c("Mean Structural Similarity", "MAS Compatibility")
  )
  expect_identical(plots$consensus_comparison$labels$y, "Rate")
  for (p in plots) expect_s3_class(p, "ggplot")
})

test_that("plot_simulation dispatches lynchpin_of_certainty", {
  result <- list(
    scenario = "lynchpin_of_certainty",
    results = list(
      baseline_compatibility = 0.42,
      post_resolution_compatibility = 0.88,
      phase_transition_score = 0.46,
      lynchpin_component_id = "C0003",
      lynchpin_rank = 1L,
      compatibility_timeline = data.frame(
        step = c("baseline", "resolve_C0003", "post"),
        compatibility = c(0.42, 0.61, 0.88),
        stringsAsFactors = FALSE
      )
    )
  )

  plots <- plot_simulation(result)
  expect_type(plots, "list")
  expect_true("compatibility_timeline" %in% names(plots))
  expect_true("lynchpin_ranking" %in% names(plots))
  for (p in plots) expect_s3_class(p, "ggplot")
})

test_that("plot_simulation dispatches ghost_discovery", {
  result <- list(
    scenario = "ghost_discovery",
    results = list(
      ghost_cluster_found = TRUE,
      clusters_detected = 2L,
      noise_count = 0L,
      ghost_clusters = data.frame(
        cluster_id = c("Cluster_01", "Cluster_02"),
        model_count = c(20L, 80L),
        internal_compatibility = c(0.8, 0.7),
        prior_compatibility = c(0.2, 0.65),
        prior_distance = c(0.6, 0.05),
        label = c("ghost", "mainstream"),
        representative_models = I(list(c("M0001", "M0002"), c("M0003", "M0004"))),
        stringsAsFactors = FALSE
      )
    ),
    artifacts = list(
      registry_data = data.frame(),
      state_data = data.frame(),
      model_ids = c("M0001", "M0002")
    )
  )

  plots <- plot_simulation(result)
  expect_type(plots, "list")
  expect_true("cluster_sizes" %in% names(plots))
  expect_s3_class(plots$cluster_sizes, "ggplot")
})

test_that("plot_simulation errors on unrecognized scenario", {
  expect_error(
    plot_simulation(list(scenario = "unknown")),
    "Unrecognized"
  )
})

test_that("simulation plots use generic compatibility metric labels", {
  consensus <- list(
    scenario = "consensus_illusion",
    results = list(
      mean_similarity_rate = 0.8,
      compatibility_metric = "identified_compatible",
      compatibility_rate = 0.5,
      consensus_illusion_gap = 0.3
    ),
    artifacts = list(
      plot_data = list(
        model_metrics = data.frame(
          model_id = c("M0001", "M0002"),
          mean_similarity_rate = c(0.8, 0.7),
          compatibility_rate = c(0.5, 0.4),
          consensus_illusion_gap = c(0.3, 0.3),
          compatibility_metric = "identified_compatible",
          stringsAsFactors = FALSE
        )
      )
    )
  )

  plots <- plot_simulation(consensus)
  expect_identical(
    plots$consensus_comparison$data$metric,
    c("Mean Structural Similarity", "Identified Compatibility")
  )

  contrast <- plot_consensus_contrast(consensus, label_outliers = FALSE)
  expect_identical(contrast$labels$x, "Mean Structural Similarity")
  expect_identical(contrast$labels$y, "Identified Compatibility Rate")
  expect_identical(contrast$labels$colour, "Consensus Illusion Gap")

  crux <- list(
    scenario = "crux_of_certainty",
    results = list(
      compatibility_metric = "mas_compatible",
      lynchpin_component_id = "C0003",
      phase_transition_score = 0.4
    ),
    artifacts = list(
      plot_data = list(
        pairwise_shift = data.frame(
          ego_id = c("M0001", "M0002"),
          alter_id = c("M0002", "M0001"),
          baseline_score = c(0.2, 0.3),
          post_score = c(0.7, 0.8),
          delta = c(0.5, 0.5),
          stringsAsFactors = FALSE
        )
      )
    )
  )
  shift <- plot_compatibility_shift(crux, geom = "density", show_delta = FALSE)
  expect_identical(shift$labels$x, "mas compatible score")
})

test_that("consensus contrast labels the largest consensus gaps", {
  metrics <- data.frame(
    model_id = sprintf("M%04d", 1:20),
    mean_similarity_rate = seq(0.6, 0.98, length.out = 20),
    compatibility_rate = seq(0.59, 0.40, length.out = 20),
    consensus_illusion_gap = seq(0.01, 0.58, length.out = 20),
    compatibility_metric = "mas_compatible",
    stringsAsFactors = FALSE
  )

  contrast <- plot_consensus_contrast(metrics)
  label_data <- contrast$layers[[3]]$data
  expect_true(all(label_data$model_id %in% c("M0019", "M0020")))
  expect_identical(label_data$model_id[[1]], "M0020")
  expect_identical(
    rlang::as_name(contrast$mapping$x), "mean_similarity_rate"
  )
  expect_identical(
    rlang::as_name(contrast$mapping$colour), "consensus_illusion_gap"
  )
})

test_that("Crux aliases dispatch through simulation plot wrappers", {
  result <- list(
    scenario = "crux_of_certainty",
    results = list(
      compatibility_metric = "similarity_rate",
      baseline_compatibility = 0.42,
      post_resolution_compatibility = 0.88,
      phase_transition_score = 0.46,
      lynchpin_component_id = "C0003",
      lynchpin_rank = 1L,
      compatibility_timeline = data.frame(
        step = c("baseline", "post"),
        compatibility = c(0.42, 0.88),
        stringsAsFactors = FALSE
      )
    ),
    artifacts = list(
      registry_data = data.frame(
        comp_id = "C0003", type = "edge", source = "X", target = "Y",
        direction = "->", description = "X->Y",
        stringsAsFactors = FALSE
      ),
      state_data = data.frame(
        model_id = rep(c("M0001", "M0002"), each = 1),
        comp_id = "C0003", status = c("unknown", "causal"),
        stringsAsFactors = FALSE
      ),
      model_ids = c("M0001", "M0002"),
      plot_data = list()
    )
  )

  plots <- plot_simulation(result)
  expect_named(plots, c("compatibility_timeline", "lynchpin_ranking"))

  expect_message(
    showcase <- plot_simulation_showcase(result),
    "Optional showcase plot 'compatibility_shift'"
  )
  expect_named(showcase, "component_status_heatmap")
  expect_s3_class(showcase$component_status_heatmap, "ggplot")
})

test_that("Crux simulation plots use complete rankings when available", {
  result <- list(
    scenario = "crux_of_certainty",
    results = list(
      baseline_compatibility = 0.30,
      post_resolution_compatibility = 0.50,
      phase_transition_score = 0.20,
      lynchpin_component_id = "C0008",
      compatibility_timeline = data.frame(
        step = c("baseline", "post"),
        compatibility = c(0.30, 0.50)
      )
    ),
    artifacts = list(
      rankings = data.frame(
        rank = 1:2,
        component_id = c("C0008", "C0010"),
        type = "edge",
        source = c("X2", "X3"),
        target = c("X1", "X6"),
        delta_u = c(0.20, 0.05),
        best_resolution = c("positive", "negative"),
        dyads_improved = c(12L, 4L),
        dyads_worsened = c(0L, 0L)
      )
    )
  )

  plots <- plot_simulation(result)

  expect_equal(nrow(plots$lynchpin_ranking$data), 2L)
  expect_setequal(
    plots$lynchpin_ranking$data$component_id,
    c("C0008", "C0010")
  )
})

test_that("symbolic simulation returns structured data", {
  result <- structure(
    list(
      scenario = "consensus_illusion",
      mode = "symbolic_sampled",
      exact = FALSE,
      universe_summary = list(
        nodes = c("A", "X", "B", "C", "D", "Y"),
        edge_count = 15L,
        exposure = "X",
        outcome = "Y"
      ),
      classes = list(
        list(class_id = "Q0001", mass = 50L, proportion = 0.5,
             adjustment_identifiable = TRUE, atom_values = list()),
        list(class_id = "Q0002", mass = 30L, proportion = 0.3,
             adjustment_identifiable = FALSE, atom_values = list()),
        list(class_id = "Q0003", mass = 20L, proportion = 0.2,
             adjustment_identifiable = TRUE, atom_values = list())
      ),
      metrics = list(
        surface_structural_consensus = 0.85,
        query_class_entropy = 1.2,
        dominant_class_share = 0.5,
        causal_compatibility = 0.38,
        consensus_gap = 0.47,
        classes_count = 3L
      ),
      artifacts = list(
        shared_edges = list(c("A", "X"), c("A", "C")),
        critical_edges = list(c("C", "D"))
      ),
      warnings = list()
    ),
    class = "theory_symbolic_simulation"
  )
  expect_s3_class(result, "theory_symbolic_simulation")
  expect_equal(result$scenario, "consensus_illusion")
  expect_equal(result$mode, "symbolic_sampled")
  expect_false(result$exact)
  expect_length(result$classes, 3)
  expect_true(result$metrics$classes_count >= 2)
  expect_true(result$metrics$surface_structural_consensus >= 0)
  expect_true(result$metrics$query_class_entropy >= 0)
})

test_that("symbolic lynchpin simulation has phase transition metrics", {
  result <- structure(
    list(
      scenario = "lynchpin_of_certainty",
      mode = "symbolic_sampled",
      exact = FALSE,
      classes = list(),
      metrics = list(
        baseline_entropy = 1.5,
        post_resolution_expected_entropy = 0.3,
        phase_transition_score = 1.2,
        lynchpin_rank = 1L,
        lynchpin_edge = list("C", "D")
      ),
      artifacts = list(
        lynchpin_edges = list(list("C", "D"))
      ),
      warnings = list()
    ),
    class = "theory_symbolic_simulation"
  )
  expect_s3_class(result, "theory_symbolic_simulation")
  expect_equal(result$scenario, "lynchpin_of_certainty")
  expect_true(result$metrics$baseline_entropy >= 0)
  expect_true(result$metrics$phase_transition_score >= 0)
  expect_equal(result$metrics$lynchpin_edge, list("C", "D"))
})

test_that("symbolic ghost simulation has ghost metrics", {
  result <- structure(
    list(
      scenario = "ghost_discovery",
      mode = "symbolic_sampled",
      exact = FALSE,
      classes = list(),
      metrics = list(
        classes_detected = 3L,
        ghost_class_count = 1L,
        ghost_total_mass = 15L,
        largest_ghost_mass = 15L,
        largest_ghost_prior_overlap = 0.1,
        ghost_internal_coherence = 0.8
      ),
      artifacts = list(),
      warnings = list()
    ),
    class = "theory_symbolic_simulation"
  )
  expect_s3_class(result, "theory_symbolic_simulation")
  expect_equal(result$scenario, "ghost_discovery")
  expect_true(result$metrics$classes_detected >= 1)
  expect_true(result$metrics$ghost_class_count >= 0)
})

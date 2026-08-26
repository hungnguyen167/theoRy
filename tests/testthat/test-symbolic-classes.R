test_that("symbolic query classes returns structured data", {
  result <- structure(
    list(
      mode = "sampled",
      exact = FALSE,
      edge_variable_count = 3L,
      candidate_adjustment_set_count = 2L,
      signature_atom_count = 5L,
      total_mass = 100L,
      classes = list(
        list(
          class_id = "Q0001",
          mass = 60L,
          proportion = 0.6,
          adjustment_identifiable = TRUE,
          empty_adjustment_valid = TRUE,
          atom_values = list(
            adjustment_identifiable = TRUE,
            empty_adjustment_valid = TRUE
          )
        ),
        list(
          class_id = "Q0002",
          mass = 40L,
          proportion = 0.4,
          adjustment_identifiable = FALSE,
          empty_adjustment_valid = FALSE,
          atom_values = list(
            adjustment_identifiable = FALSE,
            empty_adjustment_valid = FALSE
          )
        )
      ),
      warnings = list()
    ),
    class = "theory_symbolic_classes"
  )
  expect_s3_class(result, "theory_symbolic_classes")
  expect_equal(result$mode, "sampled")
  expect_false(result$exact)
  expect_length(result$classes, 2)
  expect_equal(result$classes[[1]]$class_id, "Q0001")
  expect_true(result$classes[[1]]$adjustment_identifiable)
  expect_equal(sum(result$classes[[1]]$mass, result$classes[[2]]$mass), result$total_mass)
})

test_that("symbolic query classes handles empty result", {
  result <- structure(
    list(
      mode = "sampled",
      exact = FALSE,
      edge_variable_count = 0L,
      candidate_adjustment_set_count = 0L,
      signature_atom_count = 0L,
      total_mass = 0L,
      classes = list(),
      warnings = list("No samples produced")
    ),
    class = "theory_symbolic_classes"
  )
  expect_s3_class(result, "theory_symbolic_classes")
  expect_length(result$classes, 0)
  expect_length(result$warnings, 1)
})

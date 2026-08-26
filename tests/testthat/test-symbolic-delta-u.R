test_that("compute_symbolic_delta_u returns structured data", {
  result <- structure(
    list(mode = "full", exact = TRUE, results = list(
      list(component_id = "S0001", type = "edge", source = "X", target = "Y",
           delta_u = 0.5, best_resolution = "positive")
    )),
    class = "theory_symbolic_delta_u"
  )
  expect_s3_class(result, "theory_symbolic_delta_u")
  expect_equal(result$mode, "full")
  expect_true(length(result$results) > 0)
  expect_equal(result$results[[1]]$source, "X")
})

test_that("symbolic delta-u handles empty results", {
  result <- structure(
    list(mode = "full", exact = TRUE, results = list()),
    class = "theory_symbolic_delta_u"
  )
  expect_s3_class(result, "theory_symbolic_delta_u")
  expect_length(result$results, 0)
})

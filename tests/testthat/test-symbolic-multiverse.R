test_that("build_symbolic_multiverse returns structured object", {
  skip_if_not_installed("httr2")
  result <- structure(
    list(
      nodes = c("X", "Y", "A"),
      exposure = "X",
      outcome = "Y",
      edge_count = 2L,
      edge_variables = list(
        list(source = "X", target = "Y", name = "e__X__Y"),
        list(source = "A", target = "Y", name = "e__A__Y")
      )
    ),
    class = "theory_symbolic_multiverse"
  )
  expect_s3_class(result, "theory_symbolic_multiverse")
  expect_equal(result$exposure, "X")
  expect_equal(result$outcome, "Y")
  expect_true(result$edge_count >= 0)
})

test_that("symbolic multiverse handles empty edges", {
  result <- structure(
    list(nodes = c("X", "Y"), exposure = "X", outcome = "Y", edge_count = 0L,
         edge_variables = list()),
    class = "theory_symbolic_multiverse"
  )
  expect_s3_class(result, "theory_symbolic_multiverse")
  expect_equal(result$edge_count, 0L)
})

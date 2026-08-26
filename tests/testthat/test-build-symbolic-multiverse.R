test_that("build_symbolic_multiverse rejects non-positive timing before HTTP", {
  expect_error(
    build_symbolic_multiverse(
      timing = c(X = 0, Y = 2), exposure = "X", outcome = "Y"
    ),
    "timing.*values >= 1"
  )

  expect_error(
    build_symbolic_multiverse(
      nodes = data.frame(
        name = c("X", "Y"), timing = c(1L, -1L),
        stringsAsFactors = FALSE
      ),
      exposure = "X", outcome = "Y"
    ),
    "nodes timing.*values >= 1"
  )

  expect_error(
    build_symbolic_multiverse(
      nodes = list(
        list(name = "X", timing = 1L),
        list(name = "Y", timing = 0L)
      ),
      exposure = "X", outcome = "Y"
    ),
    "nodes.*timing.*values >= 1"
  )
})

test_that("build_symbolic_multiverse rejects non-finite, fractional, and overflow timing", {
  bad_values <- list(NaN, Inf, -Inf, 1.5, 2147483648)
  for (bad in bad_values) {
    expect_error(
      build_symbolic_multiverse(
        timing = c(X = bad, Y = 2), exposure = "X", outcome = "Y"
      ),
      "timing.*values >= 1"
    )
  }
})

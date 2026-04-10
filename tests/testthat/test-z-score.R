testthat::test_that("z_score uses the correct single-pair formula", {
  expected <- abs(100 - 120) / sqrt(10^2 + 10^2)

  testthat::expect_equal(z_score(c(100, 10), c(120, 10)), expected)
})

testthat::test_that("z_score vectorizes across two-column inputs", {
  x <- matrix(c(100, 10,
                200, 20),
              ncol = 2,
              byrow = TRUE)
  y <- data.frame(estimate = c(120, 150), moe = c(10, 30))

  expected <- c(
    abs(100 - 120) / sqrt(10^2 + 10^2),
    abs(200 - 150) / sqrt(20^2 + 30^2)
  )

  testthat::expect_equal(z_score(x, y), expected)
})

testthat::test_that("z_score accepts data.table row subsets", {
  rs <- data.table::data.table(
    HINCP_median = c(100, 120, 150),
    HINCP_median_moe = c(10, 10, 30)
  )

  compare_a <- rs[c(1, 2), c("HINCP_median", "HINCP_median_moe")]
  compare_b <- rs[c(2, 3), c("HINCP_median", "HINCP_median_moe")]

  expected <- c(
    abs(100 - 120) / sqrt(10^2 + 10^2),
    abs(120 - 150) / sqrt(10^2 + 30^2)
  )

  testthat::expect_equal(z_score(compare_a, compare_b), expected)
})

testthat::test_that("z_score validates input shape", {
  testthat::expect_error(
    z_score(c(1, 2, 3), c(4, 5)),
    "length 2 or a 2-column matrix/data.frame"
  )

  testthat::expect_error(
    z_score(matrix(1:6, ncol = 3), c(4, 5)),
    "length 2 or a 2-column matrix/data.frame"
  )

  testthat::expect_error(
    z_score(matrix(c(1, 2,
                     3, 4),
                   ncol = 2,
                   byrow = TRUE), c(5, 6)),
    "same number of estimate/MOE pairs"
  )
})
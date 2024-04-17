source("../scripts/packages_and_seeds.r")
library(testthat)
testthat::local_edition(3)

test_that("Distance metric works for various vectors", {
  expect_equal(d(c(1, 0), c(0, 1)), sqrt(2), tolerance = 0.01)
  expect_equal(d(c(2, 0), c(0, 2)), sqrt(2), tolerance = 0.01)
  expect_equal(d(c(1, 0), c(-1, 0)), 2, tolerance = 0.01)
  expect_equal(d(c(2, 0), c(-2, 0)), 2, tolerance = 0.01)
})
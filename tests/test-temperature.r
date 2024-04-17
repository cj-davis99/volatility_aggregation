source("../scripts/packages_and_seeds.r")
library(testthat)
testthat::local_edition(3)

test_that("Make sure temperature data returns a numeric vector", {
  expect_false(any(is.na(temperature_data(winter_sd = 7.5, summer_sd = 5))))
  expect_false(any(is.na(temperature_data(winter_sd = 10, summer_sd = 3))))
  expect_false(any(is.na(temperature_data(winter_sd = 8, summer_sd = 5))))
  expect_false(any(is.na(temperature_data(winter_sd = 3, summer_sd = 8))))
  expect_false(any(is.na(temperature_data(winter_sd = 5, summer_sd = 5))))
})
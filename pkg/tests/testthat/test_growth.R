library(regts)
library(testthat)
context("growth_ts")


# create some example timeseries
a <- regts(rnorm(20), start = "2018Q2")
ts_labels(a) <- "Timeseries a"

mts <- regts(matrix(rnorm(30), ncol = 3), start = "2018Q1", names = c("a", "b", "c"))
ts_labels(mts) <- c("Timeseries a", "Timeseries b", "Timeseries c")

test_that("univariate timeseries", {

  p <- get_period_range(a)
  result <- (a - lag(a, -1))/ abs(lag(a, -1))
  resultp <- result[p]
  expect_identical(growth_ts(a, keep_range = FALSE) , result)
  expect_identical(growth_ts(a), resultp)

  result <- (a - lag(a, -4))/ abs(lag(a, -4))
  resultp <- result[p]
  expect_identical(growth_ts(a, lag = 4, keep_range = FALSE) , result)
  expect_identical(growth_ts(a, lag = 4), resultp)

  expect_identical(ts_labels(result), ts_labels(a))
  expect_identical(ts_labels(resultp), ts_labels(a))
})

test_that("multivariate timeseries", {

  p <- get_period_range(mts)
  result <- (mts - lag(mts, -1))/ abs(lag(mts, -1))
  resultp <- result[p]
  expect_identical(growth_ts(mts, keep_range = FALSE) , result)
  expect_identical(growth_ts(mts), resultp)

  result <- (mts - lag(mts, -4))/ abs(lag(mts, -4))
  resultp <- result[p]
  expect_identical(growth_ts(mts, lag = 4, keep_range = FALSE) , result)
  expect_identical(growth_ts(mts, lag = 4), resultp)

  expect_identical(ts_labels(result), ts_labels(mts))
  expect_identical(ts_labels(resultp), ts_labels(mts))
})

test_that("errors", {
  msg <- "Timeseries must have more observations than size of lag"
  expect_error(growth_ts(a, lag = 20), msg)
})

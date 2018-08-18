library(regts)
library(testthat)

rm(list = ls())

context("lag_ts")

# prepare input data
labels <- c("Var a", "Var b")
qts <- regts(matrix(data = rep(1:8), nc = 2), start = "2018Q4",
             names = c("a", "b"), labels = labels)
qp <- get_period_range(qts)

mts <- regts(matrix(data = rep(1:8), nc = 2), start = "2018m4",
             names = c("a", "b"), labels = labels)
mp <- get_period_range(mts)

test_that("lag_ts", {
  result1 <- lag_ts(qts)
  expect_identical(result1, lag(qts, -1)[qp])
  expect_identical(unname(ts_labels(result1)), labels)
  expect_identical(lag_ts(qts, k = 2, keep_range = FALSE), lag(qts, -2))
  expect_identical(lag_ts(qts[, 1], k = 2), lag(qts[, 1], -2)[qp])

  expected_result <- lag(qts, -10)[qp]
  # expected_result is logical, therefore convert to integer
  expected_result[] <- as.integer(expected_result)
  expect_identical(lag_ts(qts, k = 10), expected_result)

  expect_identical(lag_ts(qts, k = 10, keep_range = FALSE), lag(qts, -10))

  qts2 <- qts["/2019Q1"]
  qp2 <- get_period_range(qts2)

  expected_result <- lag(qts2, -2)[qp2]
  # expected_result is logical, therefore convert to integer
  expected_result[] <- as.integer(expected_result)
  expect_identical(lag_ts(qts2, k = 2), expected_result)

  expect_identical(lag_ts(qts2, k = 2, keep_range = FALSE), lag(qts2, -2))
})

test_that("lead_ts", {
  result1 <- lead_ts(mts)
  expect_identical(result1, lag(mts, 1)[mp])
  expect_identical(unname(ts_labels(result1)), labels)
  expect_identical(lead_ts(mts, k = 2, keep_range = FALSE), lag(mts, 2))
  expect_identical(lead_ts(mts[, 1], k = 2), lag(mts[, 1], 2)[mp])
})

test_that("diff_ts", {
  result1 <- diff_ts(qts)
  expect_identical(result1, diff(qts, 1)[qp])
  expect_identical(unname(ts_labels(result1)), labels)
  expect_identical(diff_ts(qts, lag = 2, keep_range = FALSE),
                   diff(qts, lag = 2))
  expect_identical(diff_ts(qts, differences = 2), diff(qts, differences = 2)[qp])
  expect_identical(diff_ts(qts[, 1], differences = 2), diff(qts[, 1],
                                                            differences = 2)[qp])

  expect_error(diff_ts(qts["2018q4"]), "Timeseries x has too few observations")
  expect_error(diff_ts(qts["/2019q1", 1], differences = 2), "Timeseries x has too few observations")
})


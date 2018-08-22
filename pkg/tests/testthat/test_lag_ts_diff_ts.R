library(regts)
library(testthat)

rm(list = ls())

context("lag_ts, lead_ts and diff_ts")

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
  expect_identical(lag_ts(qts, n = 2, keep_range = FALSE), lag(qts, -2))
  expect_identical(lag_ts(qts[, 1], n = 2), lag(qts[, 1], -2)[qp])

  expect_identical(lag_ts(qts, n = 10), lag(qts, -10)[qp])

  expect_identical(lag_ts(qts, n = 10, keep_range = FALSE), lag(qts, -10))

  qts2 <- qts["/2019Q1"]
  qp2 <- get_period_range(qts2)

  expect_identical(lag_ts(qts2, n = 2),  lag(qts2, -2)[qp2])

  expect_identical(lag_ts(qts2, n = 2, keep_range = FALSE), lag(qts2, -2))


  expect_error(lag_ts(qts, n = -1, keep_range = FALSE),
               "Argument n should be positive")
})

test_that("lead_ts", {
  result1 <- lead_ts(mts)
  expect_identical(result1, lag(mts, 1)[mp])
  expect_identical(unname(ts_labels(result1)), labels)
  expect_identical(lead_ts(mts, n = 2, keep_range = FALSE), lag(mts, 2))
  expect_identical(lead_ts(mts[, 1], n = 2), lag(mts[, 1], 2)[mp])
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
  expect_error(diff_ts(qts["/2019q1", 1], differences = 2),
               "Timeseries x has too few observations")
})


test_that("vector argument", {
  v <- 1:3
  expect_identical(lag_ts(v), regts(c(NA, 1:2), period = "1/3"))
  expect_identical(lag_ts(v, n = 2, keep_range = FALSE),
                   regts(v, period = "3/5"))
  expect_identical(lead_ts(v), regts(c(2:3, NA), period = "1/3"))
  # regts does not yet support negative periods
  expect_identical(lead_ts(v, n = 2, keep_range = FALSE),
                   as.regts(ts(v, start = -1)))

  expect_identical(diff_ts(v, lag = 2), regts(c(NA, NA, 2L), period = "1/3"))

  # function diff creates a name e1 here, I have no idea why
  expected_result <- regts(0L, period = "3")
  names(expected_result) <- "e1"
  expect_identical(diff_ts(v, differences =  2, keep_range = FALSE),
                   expected_result)

  mat <- matrix(c("a", "b", "c", "d"), nrow = 2)
  expect_identical(lag_ts(mat), regts(matrix(c(NA, "a", NA, "c"), ncol = 2),
                                      period = "1/2"))
  expect_identical(lag_ts(mat, n = 2, keep_range = FALSE),
                   regts(mat, period = "3/4"))
  expect_identical(lead_ts(mat), regts(matrix(c("b", NA, "d", NA), ncol = 2),
                                              period = "1/2"))
  # regts does not yet support negative periods
  expected_result <- as.regts(ts(mat, start = -1))
  colnames(expected_result) <- NULL
  expect_identical(lead_ts(mat, n = 2, keep_range = FALSE),
                   expected_result)

  expect_error(diff_ts(mat), "non-numeric argument to binary operator")

  mat2 <- matrix(as.numeric(1:4), nrow = 2)
  expect_identical(diff_ts(mat2), regts(matrix(c(NA, 1, NA, 1), ncol = 2),
                                        period = "1/2"))
  expect_identical(diff_ts(mat2, keep_range = FALSE),
                   regts(matrix(1, ncol = 2), period = "2"))
})


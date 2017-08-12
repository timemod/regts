library(regts)
library(testthat)

context("conversion functions for timeseries")

set.seed(123)

test_that("rel2index univariate timeseries", {
  ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
  ts1 <- 100 * ts1 / ts1[1]
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel)
  expect_equal(ts1, ts1_index)

  ts1_index2 <- rel2index(ts1_rel, base_period = "2010Q4")
  expected <- 100 * ts1 / as.numeric(ts1["2010Q4"])
  expect_equal(ts1_index2, expected)
})

test_that("rel2index multivariate timeseries", {
  ts1 <- regts(matrix(abs(rnorm(20)), ncol = 2), start = "2010Q2",
               names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
  ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel, scale = 1)
  expect_equal(ts1, ts1_index)

  ts1_index2 <- rel2index(ts1_rel, base_period = "2011Q3", scale = 1)
  expected <- ts1
  i <- period("2011Q3") - period("2010Q2") + 1
  expected[] <- apply(expected, MARGIN = 2, FUN = function(x) {x /x[i]})
  expect_equal(ts1_index2, expected)
})

test_that("pct2index", {
  ts1 <- regts(matrix(abs(rnorm(20)), ncol = 2), start = "2010Q2",
               names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
  ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
  ts1_rel <- 100 * diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- pct2index(ts1_rel, scale = 1)
  expect_equal(ts1, ts1_index)
})

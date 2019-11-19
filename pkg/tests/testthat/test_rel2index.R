library(regts)
library(testthat)

context("conversion functions for timeseries")

set.seed(123)

data <- c(-0.56047565, -0.23017749,  1.55870831,  0.07050839,  0.12928774,
           1.71506499,  0.46091621, -1.26506123, -0.68685285, -0.44566197,
           1.22408180,  0.35981383,  0.40077145,  0.11068272, -0.55584113,
           1.78691314,  0.49785048, -1.96661716,  0.70135590, -0.47279141)

test_that("rel2index univariate timeseries", {
  ts1 <- regts(data[1:10], start = "2010Q2")
  ts1 <- 100 * ts1 / ts1[1]
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel, keep_range = FALSE)
  expect_equal(ts1, ts1_index)

  ts1_index_2 <- rel2index(ts1_rel["2010q3"], keep_range = TRUE)
  expect_equal(ts1["2010q3"], ts1_index_2)

  expect_error(rel2index(ts1_rel, base = "2010Q4"),
               "Cumulated timeseries has negative value at base period 2010Q4.")

  ts1_index2 <- rel2index(ts1_rel, "2010q3")
  expected <- (100 * ts1 / as.numeric(ts1["2010Q3"]))[get_period_range(ts1_rel)]
  expect_equal(ts1_index2, expected)
})

test_that("rel2index multivariate timeseries", {
  ts1 <- regts(matrix(data, ncol = 2), start = "2010Q2",
               names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
  ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel, scale = 1, keep_range = TRUE)
  p <- get_period_range(ts1_rel)
  expect_equal(ts1[p], ts1_index[p])

  ts1_index_2 <- rel2index(ts1_rel["2010q3"], keep_range = TRUE, scale = 1)
  expect_equal(ts1["2010q3"], ts1_index_2)

  expect_error(rel2index(ts1_rel, base = "2011Q3", scale = 1),
               "Cumulated timeseries has negative value at base period 2011Q3 for columns: a.")
  expect_error(rel2index(ts1_rel, base = "2011Q2", scale = 1),
               "Cumulated timeseries has negative value at base period 2011Q2 for columns: a, b.")

  ts1_index2 <- rel2index(ts1_rel, base = "2012Q2", scale = 1,
                          keep_range = FALSE)
  expected <- ts1
  i <- period("2012Q2") - period("2010Q2") + 1
  expected[] <- apply(expected, MARGIN = 2, FUN = function(x) {x /x[i]})
  expect_equal(ts1_index2, expected)

  # empty timeseries
  ts1_rel_empty <- ts1_rel[ , character(0)]
  ts1_empty <- ts1[ , character(0)]
  expect_equal(rel2index(ts1_rel_empty, keep_range = FALSE), ts1_empty)
  expect_equal(rel2index(ts1_rel_empty, keep_range = TRUE), ts1_empty[p],
               check.attributes = FALSE)
})

test_that("pct2index", {
  ts1 <- regts(matrix(data, ncol = 2), start = "2010Q2",
               names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
  ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
  ts1_rel <- 100 * diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- pct2index(ts1_rel, scale = 1)
  p <- get_period_range(ts1_rel)
  expect_equal(ts1[p], ts1_index[p])

  ts1_index2 <- pct2index(ts1_rel, scale = 1, keep_range = FALSE)
  expect_equal(ts1, ts1_index2)
})

test_that("NA values", {
  ts1 <- regts(c(1L, 2L, 3L, NA, 5L, 6L), start = "2010Q2")
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel, scale = 1, keep_range = FALSE)
  expected_result <- ts1
  expected_result["2011Q2/"] <- NA
  expect_equal(ts1_index, expected_result)
})

test_that("Inf values", {
  ts1 <- regts(c(1L, 2L, 3L, Inf, 5L, 6L), start = "2010M2")
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  ts1_index <- rel2index(ts1_rel, scale = 1)
  expected_result <- ts1[get_period_range(ts1_rel)]
  expected_result["2010m6/"] <- NaN
  expect_equal(ts1_index, expected_result)
})

test_that("errors", {
  ts1 <- regts(c(1, 2, 3), start = "2010Q2")
  msg <- "The base period should lie between 2010Q1 and 2010Q4."
  expect_error(rel2index(ts1, base = "2018Q3"), msg)
  msg <- paste("The base period 2018M03 has a different frequency than the",
               "timeseries \\(4\\).")
  expect_error(rel2index(ts1, base = "2018M3"), msg)
})

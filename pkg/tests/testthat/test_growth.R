library(regts)
library(testthat)


# create some example timeseries
a <- regts(rnorm(20), start = "2018Q2")
ts_labels(a) <- "Timeseries a"

mts <- regts(matrix(rnorm(30), ncol = 3), start = "2018Q1",
             names = c("a", "b", "c"))
ts_labels(mts) <- c("Timeseries a", "Timeseries b", "Timeseries c")

test_that("univariate timeseries", {

  p <- get_period_range(a)
  result <- (a - lag(a, -1)) / lag(a, -1)
  resultp <- result[p]
  expect_identical(growth(a, keep_range = FALSE), result)
  expect_identical(growth(a), resultp)

  result <- (a - lag(a, -4)) / lag(a, -4)
  resultp <- result[p]
  expect_identical(growth(a, n = 4, keep_range = FALSE), result)
  expect_identical(growth(a, n = 4), resultp)

  expect_identical(ts_labels(result), ts_labels(a))
  expect_identical(ts_labels(resultp), ts_labels(a))
})

test_that("multivariate timeseries", {

  p <- get_period_range(mts)
  result <- (mts - lag(mts, -1)) / lag(mts, -1)
  resultp <- result[p]
  expect_identical(growth(mts, keep_range = FALSE), result)
  expect_identical(growth(mts), resultp)

  result <- (mts - lag(mts, -4)) / lag(mts, -4)
  resultp <- result[p]
  expect_identical(growth(mts, n = 4, keep_range = FALSE), result)
  expect_identical(growth(mts, n = 4), resultp)

  expect_identical(ts_labels(result), ts_labels(mts))
  expect_identical(ts_labels(resultp), ts_labels(mts))
})

test_that("errors", {
  msg <- "Timeseries must have at least 21 observations"
  expect_error(growth(a, n = 20), msg)
})

test_that("non-timeseries", {

  # vector
  v <- 1:4
  v_gr <- growth(v)
  v_gr_expected <- regts(c(NA,  1, 1 / 2, 1 / 3))
  expect_equal(v_gr, v_gr_expected)

  m <- cbind(v, w = 2 * v)
  m_gr <- growth(m)
  m_gr_expected <- cbind(v = v_gr, w = v_gr)
  expect_equal(m_gr, m_gr_expected)

  df <- as.data.frame(m)
  rownames(df) <- c("2020Q1", "2020Q2", "2020Q3", "2020Q4")
  df_gr <- growth(df)
  df_gr_expected <- m_gr_expected
  expect_equal(df_gr, m_gr_expected)
})

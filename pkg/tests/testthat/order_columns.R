library(regts)
library(testthat)
library(tibble)
library(data.table)
library(conflicted)

mts <- regts(matrix(rnorm(30), ncol = 3), start = "2018Q1",
             names = c("b", "a", "c"))
ts_labels(mts) <- c("Timeseries b", "Timeseries a", "Timeseries c")

test_that("timeseries", {
  expect_equal(order_columns(mts), mts[, c("a", "b", "c")])
  expect_equal(order_columns(mts[, "a", drop = FALSE]),
               mts[, "a", drop = FALSE])
})

test_that("data frame", {
  df <- as.data.frame(mts)
  expect_equal(order_columns(df), df[, c("a", "b", "c")])
  expect_equal(order_columns(df[, "a", drop = FALSE]), df[, "a", drop = FALSE])

  dft <- as_tibble(df)
  expect_equal(order_columns(dft), dft[, c("a", "b", "c")])
  expect_equal(order_columns(dft[, "a"]), dft[, "a"])

  dfdt <- as.data.table(df)
  expect_equal(order_columns(dfdt), dfdt[, c("a", "b", "c")])
  expect_equal(order_columns(dfdt[, "a"]), dfdt[, "a"])
})

test_that("matrix", {
  m <- as_matrix(mts)
  expect_equal(order_columns(df), df[, c("a", "b", "c")])
  expect_equal(order_columns(df[, "c", drop = FALSE]), df[, "c", drop = FALSE])
})


test_that("errors", {

  emsg <- "Argument 'x' does not have column names"

  # univariate timeseries
  expect_error(order_columns(mts$a), emsg)

  # multivariate timeseries without column name
  mts_err <- mts
  colnames(mts_err) <- NULL

  expect_error(order_columns(mts_err), emsg)
})

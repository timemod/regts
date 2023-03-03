library(regts)
library(testthat)

rm(list = ls())


a_ts <- regts(1:3, start = "2018Q1")
a_df <-data.frame(a_ts = 1:3, row.names = c("2018Q1", "2018Q2", "2018Q3"))
a_mat <- as.matrix(a_df)

test_that("univariate timeseries without labels", {
  expect_identical(as_matrix(a_ts), a_mat)
  expect_identical(as_matrix(a_ts, rowwise = TRUE), t(a_mat))
})

test_that("univariate timeseries with labels", {
  # as_matrix ignores labels

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- "Var a"

  a_mat_l <- a_mat
  colnames(a_mat_l) <- "a_ts_l"

  expect_identical(as_matrix(a_ts_l), a_mat_l)
  expect_identical(as_matrix(a_ts_l, rowwise = TRUE), t(a_mat_l))

})

test_that("multivariate timeseries with labels", {

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- "Var a"

  b_ts_l <- 2 * a_ts
  ts_labels(b_ts_l) <- "Var b"

  multi_ts <- cbind(a_ts = a_ts_l, b_ts = b_ts_l)

  multi_mat <- cbind(a_mat, 2 * a_mat)
  colnames(multi_mat)[2] <- "b_ts"

  expect_identical(as_matrix(multi_ts), multi_mat)
  expect_identical(as_matrix(multi_ts, rowwise = TRUE), t(multi_mat))
})


test_that("single period", {

  a <- a_ts["2018Q1"]
  expected_result <- a_mat[1, , drop = FALSE]
  colnames(expected_result) <- "a"
  expect_identical(as_matrix(a), expected_result)

  ab_ts  <- cbind(a, b = 2 * a)
  ts_labels(ab_ts) <- c("Var a", "Var b")

  expected_result <- as.matrix(data.frame(a = 1, b = 2,
                                          stringsAsFactors = FALSE))
  rownames(expected_result) <- "2018Q1"

  expect_identical(as_matrix(ab_ts), expected_result)
})

library(regts)
library(testthat)

test_that("select_range works for regts", {
  ts1 <- regts(1:10, start = "2010Q1")
  range <- "2010Q2/2010Q3"
  res <- select_range(ts1, range)
  expect_s3_class(res, "regts")
  expect_equal(as.numeric(res), 2:3)
  expect_equal(start_period(res), as.period("2010Q2"))

  range <- 2011
  res <- select_range(ts1, range)
  expect_equal(res, ts1["2011"])
})

test_that("select_range works for ts", {
  ts1 <- ts(1:10, start = c(2010, 1), frequency = 4)
  range <- "2010Q2/2010Q3"
  res <- select_range(ts1, range)
  expect_s3_class(res, "regts")
  expect_equal(as.numeric(res), 2:3)
  expect_equal(start_period(res), as.period("2010Q2"))
})

test_that("select_range works with piping", {
  ts1 <- regts(1:10, start = "2010Q1")
  res <- ts1 |> select_range("2010Q2/2010Q3")
  expect_equal(as.numeric(res), 2:3)
})

test_that("select_range works for multivariate regts", {
  ts_m <- regts(matrix(1:20, ncol = 2), start = "2010Q1", names = c("a", "b"))
  res <- ts_m |> select_range("2010Q2/2010Q3")
  expect_s3_class(res, "regts")
  expect_equal(ncol(res), 2)
  expect_equal(as.numeric(res[, "a"]), 2:3)
  expect_equal(as.numeric(res[, "b"]), 12:13)

  res <- ts_m |> select_range("2011/")
  expect_s3_class(res, "regts")
  expect_equal(ncol(res), 2)
  expect_equal(as.numeric(res[, "a"]), 5:10)
  expect_equal(as.numeric(res[, "b"]), 15:20)
})

test_that("select_range errors for non-ts input", {
  expect_error(
    select_range(1:10, "2010Q1/2010Q2"),
    "Argument 'x' must be a 'ts' or 'regts' object"
  )
})

test_that("select_range errors for invalid range", {
  ts1 <- regts(1:10, start = "2010Q1")
  # Invalid string
  expect_error(select_range(ts1, "invalid_range"))
  # Numeric vector of length > 1 that cannot be a single period
  expect_error(select_range(ts1, c(2010, 2011)))
})

test_that("select_range errors for non-ts input", {
  expect_error(
    select_range(1:10, "xxx"),
    "Argument 'x' must be a 'ts' or 'regts' object"
  )
})

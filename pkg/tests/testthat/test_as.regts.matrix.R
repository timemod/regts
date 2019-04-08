library(regts)
library(testthat)

rm(list = ls())

context("as.regts.matrix")

ref_ts <- regts(matrix(1:6, ncol = 2), start = "2018q1",
                    names = c("a", "b"))

test_that("as.regts.matrix univariate", {

  m <- matrix(1:3, ncol = 1, dimnames = list(c("2018q1", "2018q2", "2018q3"),
                                             c("a")))
  regts1 <- as.regts(m)

  expect_equal(regts1, ref_ts[ , "a", drop = FALSE])

  expect_error(as.regts(m[-2, , drop = FALSE]),
               "Missing periods found \\(2018Q2\\). Set parameter strict to FALSE!")

  regts2 <- as.regts(m[-2, , drop = FALSE], strict = FALSE)
  expected_result <- ref_ts[ , "a", drop = FALSE]
  expected_result[2, ] <- NA
  expect_equal(regts2, expected_result)
})

test_that("no column names", {

  m <- matrix(1:6, ncol = 2, dimnames = list(NULL,
                                             c("a", "b")))

  regts1 <- as.regts(m)
  expect_equal(regts1, regts(m, period = "1/3"))
})

test_that("different frequencies and fun", {

  fun <- function(x) {period(paste0(2018, x))}
  m <- matrix(1:2, ncol = 1, dimnames = list(c("q1", "m2"), c("a")))
  expect_error(as.regts(m, fun = fun),
               "The row names contain periods with different frquencies")
})


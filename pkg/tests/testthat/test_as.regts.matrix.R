library(regts)
library(testthat)

rm(list = ls())


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

test_that("no row names", {

  m <- matrix(1:6, ncol = 2, dimnames = list(NULL,
                                             c("a", "b")))

  regts1 <- as.regts(m)
  expect_equal(regts1, regts(m, period = "1/3"))
})

test_that("fun", {

  fun_a <- function(x) {period(paste0(2018, x))}
  fun_b <- function(x) {as.list(period(paste0(2018, x)))}
  fun_c <- function(x) {period("2018q1")}
  fun_d <- function(x) {c(TRUE, FALSE)}

  m1 <- matrix(1:2, ncol = 1, dimnames = list(c("q1", "q2"), c("a")))
  regts1 <- as.regts(m1, fun = fun_a)
  expect_identical(regts1, regts(matrix(1:2, ncol = 1), names = "a",
                                 start = "2018q1"))

  regts2 <- as.regts(m1, fun = fun_b)
  expect_identical(regts1, regts2)

  expect_error(as.regts(m1, fun = fun_c),
               paste("Function 'fun' should return an object with the same",
                     "length as\nthe number of rows in the data.frame or matrix."))

  expect_error(as.regts(m1, fun = fun_d),
               "Function 'fun' should return a period vector.")


  m2 <- matrix(1:2, ncol = 1, dimnames = list(c("q1", "m2"), c("a")))
  expect_error(as.regts(m2, fun = fun_a),
               "The row names contain periods with different frequencies.")
})


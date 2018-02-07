library(regts)
library(testthat)

rm(list = ls())

context("$ operator for regts")

test_that("$.regts", {

  data <- matrix(1: 12, ncol = 4)
  colnames(data) <- c("b b", "a", "c", "a")
  regts1 <- regts(data, start = "2010Q4")
  expect_identical(regts1$a, regts1[, 2])
  expect_identical(regts1$"b b", regts1[, 1])
  expect_null(regts1$z)
  expect_identical(regts1[, "a", drop = FALSE]$a, regts1[, 2])

  # errors
  rts_no_names <- regts1
  colnames(rts_no_names) <- NULL
  msg <- "\\$ operator not possible for regts without column names"
  expect_error(rts_no_names$a, msg)

  msg <- "\\$ operator not possible for vector timeseries"
  expect_error(regts1[, "a"]$a, msg)
})

test_that("$<-.regts", {
  data <- matrix(1: 12, ncol = 4)
  colnames(data) <- c("b b", "a", "c", "a")
  regts1 <- regts(data, start = "2010Q4")
  regts1$a <- 2
  regts1$"b b" <- 2 * regts1$c
  regts1$z <- c(100, 1000, 10000)

  p <- get_period_range(regts1)
  expect_identical(regts1[, "a"], regts(2, period = p))
  expect_identical(regts1[, "b b"], regts(c(14, 16, 18), period = p))
  expect_identical(regts1[, "z"], regts(c(100, 1000, 10000), period = p))

  regts2 <- regts1
  regts2$a <- regts(1:3, start = "2011Q2")
  expect_identical(regts2[, "a"], regts(c(1,2,3), start = "2010Q4"))

  regts2 <- regts1
  ts_labels(regts2) <- paste("Variable", 1:5)
  regts2$a[1:2] <- regts(3:4, start = "2018M2")
  expect_identical(regts2[, "a"], regts(c(3,4,2), period = p,
                                        labels = "Variable 2"))

  regts2 <- regts1
  regts2$a <- NULL
  regts2$"b b" <- NULL
  expect_identical(colnames(regts2), c("c", "a", "z"))

  regts2 <- regts1[ , c("a", "c")]
  regts2$z <- lag(2 * regts2$c, -1)
  regts2$c <- lag(regts2$c)
  data <- matrix(c(2, 2, 2, 7, 8, 9, 14, 16, 18), ncol = 3)
  expected_result <- regts(data, period = "2010Q4/2011Q2", names = c("a", "c", "z"))
  expect_identical(regts2, expected_result)

  # errors
  rts_no_names <- regts1
  colnames(rts_no_names) <- NULL
  msg <- "\\$ operator not possible for regts without column names"
  expect_error(rts_no_names$a <- 2, msg)

  msg <- "\\$ operator not possible for vector timeseries"
  expect_error(regts1[, "a"]$a <- 8, msg)


})








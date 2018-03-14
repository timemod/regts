library(regts)
library(testthat)

rm(list = ls())

context("index_ts")

a <- regts(1:18, start = "2018Q1")
b <- 2 * a
ab <- cbind(a, b)

test_that("default arguments", {
  expect_identical(index_ts(a), 100 * a)
  expected_result  <- 100 * ab
  expected_result$b <- expected_result$a
  expect_identical(index_ts(ab), expected_result)
})

test_that("base and index_value", {
  bp <- "2021Q1"
  expect_equal(index_ts(a, base = bp), (100 / 13)* a)
  expected_result  <- (100 / 13) * ab
  expected_result$b <- expected_result$a
  expect_equal(index_ts(ab, bp), expected_result)

  bp <- "2020"
  mean_2020 <- mean(a[bp])
  expect_equal(index_ts(a, base = bp), (100 / mean_2020) * a)
  expected_result  <- (100 / mean_2020) * ab
  expected_result$b <- expected_result$a
  expect_equal(index_ts(ab, bp), expected_result)

  bp <- period_range("2019Q2", "2020Q1")
  mean_bp <- mean(a[bp])
  expect_equal(index_ts(a, base = bp,
                        index_value = 1), (1 / mean_bp) * a)
  expected_result  <- (1 / mean_bp) * ab
  expected_result$b <- expected_result$a
  expect_equal(index_ts(ab, bp, index_value = 1), expected_result)
})

test_that("errors", {
  msg <- "Base period \\(2017Q1\\) not within timeseries period \\(2018Q1/2022Q2\\)"
  expect_error(index_ts(a, base  = "2017Q1"), msg)
  msg <-  paste0("Base period \\(2017M01\\) should not have a higher frequency",
                 " than the input timeseries \\(4\\)")
  expect_error(index_ts(a, base  = "2017M1"), msg)
})


test_that("NA values", {
  a_NA <- a
  a_NA["2019Q3"] <- NA
  index_ts(a_NA,  base = "2019Q3/2019Q4")
  ab_NA <- ab
  expect_identical(index_ts(a), 100 * a)
  expected_result  <- 100 * ab
  expected_result$b <- expected_result$a
  expect_identical(index_ts(ab), expected_result)
})


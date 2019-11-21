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

test_that("base and scale", {
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
                        scale = 1), (1 / mean_bp) * a)
  expected_result  <- (1 / mean_bp) * ab
  expected_result$b <- expected_result$a
  expect_equal(index_ts(ab, bp, scale = 1), expected_result)
})

test_that("errors", {
  msg <- "Base period \\(2017Q1\\) not within timeseries period \\(2018Q1/2022Q2\\)"
  expect_error(index_ts(a, base  = "2017Q1"), msg)
  msg <-  paste0("Base period \\(2017M01\\) should not have a higher frequency",
                 " than the input timeseries \\(4\\)")
  expect_error(index_ts(a, base  = "2017M1"), msg)
  expect_error(index_ts(a, scale = -10),
               "Argument scale must be a positive number.")
  expect_error(index_ts(a, scale = NaN),
               "Argument scale must be a positive number.")
})

test_that("NA values", {

  a_NA <- a
  a_NA["2019Q3"] <- NA
  warning <- "NA values in base period 2019Q3/2019Q4"
  expect_warning(a_NA_i <- index_ts(a_NA,  base = "2019Q3/2019Q4"), warning)
  expect_equal(a_NA_i, NA * a)

  warning <- "NA values in base period 2019Q3"
  expect_warning(a_NA_i <- index_ts(a_NA,  base = "2019Q3"), warning)
  expect_equal(a_NA_i, NA * a)

  ab_NA <- ab
  ab_NA["2019Q3", "a"] <- NA
  warning <- paste("NA values in base period 2019Q3/2019Q4 for columns: a.")
  expect_warning(ab_NA_i <- index_ts(ab_NA,  base = "2019Q3/2019Q4"), warning)

  expected_result  <-100 * ab_NA / 15
  expected_result$a <- NA
  expect_equal(ab_NA_i, expected_result)

  ab_NA <- ab
  ab_NA["2019Q3", "a"] <- NA
  warning <- "NA values in base period 2019Q3"
  expect_warning(ab_NA_i <- index_ts(ab_NA,  base = "2019Q3"), warning)

  expected_result  <-100 * ab_NA / 14
  expected_result$a <- NA
  expect_equal(ab_NA_i, expected_result)

  abc_NA <- ab
  abc_NA$c <- 2
  abc_NA["2019Q3", "a"] <- NA
  abc_NA["2019q4", "c"] <- NA
  warning <- paste("NA values in base period 2019Q3/2019Q4 for columns: a, c.")
  expect_warning(abc_NA_i <- index_ts(abc_NA,  base = "2019Q3/2019Q4"), warning)

  expected_result  <- 100 * abc_NA / 15
  expected_result$a <- NA
  expected_result$c <- NA
  expect_equal(abc_NA_i, expected_result)

  big_NA <- ab
  big_NA[ , paste0("x_", 1:1000)] <- 2
  big_NA["2019Q3", "a"] <- NA
  big_NA["2019q4", 2:1002] <- NA
  warning <- "NA values in base period 2019Q3/2019Q4 for columns: a, b"
  expect_warning(big_NA_i <- index_ts(big_NA,  base = "2019Q3/2019Q4"), warning)

  expected_result  <- 100 * big_NA / 15
  expected_result$a <- NA
  expected_result[ , 2:1002] <- NA
  expect_equal(big_NA_i, expected_result)

  # now test a timeseries without column names
  no_colnames <- big_NA
  colnames(no_colnames) <- NULL
  warning <- paste("NA values in base period",
                   "2019Q3/2019Q4 for columns: 1, 2, 3, 4")

  expect_warning(no_colnames_i <- index_ts(no_colnames,
                                           base = "2019Q3/2019Q4"), warning)

  expected_result  <- 100 * no_colnames / 15
  expected_result[ , 1] <- NA
  expected_result[ , 2:1002] <- NA
  expect_equal(no_colnames_i, expected_result)
})

test_that("negative value at base period", {

  a2 <- a
  a2["2018q2"] <- -3
  expect_equal(index_ts(a2), a2 * 100)

  expect_error(index_ts(a2, base = "2018q2"),
               "Negative \\(average\\) value at base period 2018Q2.")
  expect_error(index_ts(a2, base = "2018q1/2018q2"),
               "Negative \\(average\\) value at base period 2018Q1/2018Q2.")

  ab2 <- ab
  ab2["2018q1", 1] <- -10

  expected_result  <- 50 * ab2
  expected_result$b <- expected_result$b / 2

  expect_equal(index_ts(ab2, base = "2018q2"), expected_result)

  expect_error(index_ts(ab2),
               "Negative \\(average\\) value at base period 2018Q1 for columns: a.")

  ab2["2018q1", 2] <- -5
  expect_error(index_ts(ab2,  base = "2018q1/2018q2"),
               "Negative \\(average\\) value at base period 2018Q1/2018Q2 for columns: a, b.")


  ab2["2018q1", 1] <- -2
  expect_warning(
    expect_error(index_ts(ab2,  base = "2018q1/2018q2"),
               "Negative \\(average\\) value at base period 2018Q1/2018Q2 for columns: b."),
    "Zero \\(average\\) value at base period 2018Q1/2018Q2 for columns: a.")
})

test_that("zero value at base period", {

  a2 <- a
  a2["2018q1"] <- 0

  expect_warning(result <- index_ts(a2),
                 "Zero \\(average\\) value at base period 2018Q1.")
  expect_equal(result, a2 *Inf)

  ab2 <- ab
  ab2["2018q2", 1] <- 0

  expect_warning(
    result <- index_ts(ab2, base = "2018q2"),
     "Zero \\(average\\) value at base period 2018Q2 for columns: a.")

  expected_result  <- 25 * ab2
  expected_result$a <- expected_result$a * Inf
  expect_equal(result, expected_result)

  ab2["2018q2", ] <- c(-1, -2)

  expect_warning(
    result <- index_ts(ab2, base = "2018q1/2018q2"),
    "Zero \\(average\\) value at base period 2018Q1/2018Q2 for columns: a, b")
  expect_equal(result, ab2 * Inf)
})


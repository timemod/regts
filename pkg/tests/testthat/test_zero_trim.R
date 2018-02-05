library(testthat)
library(regts)

rm(list = ls())

context("zero_trim")

test_that("univariate timeseries", {
  ts1 <- regts(c(0,1,3,0,4,8), start = "2000")
  ts2 <- regts(c(0,1,3,0,4,8,0), start = "2000")
  ts3 <- regts(rep(0,4), start = "2000")

  expect_identical(zero_trim(ts1), regts(c(1,3,0,4,8), start = "2001"))
  expect_identical(ts1, zero_trim(ts2, method = "last"))
  expect_identical(zero_trim(ts1, method = "first"), zero_trim(ts2))
  expect_identical(zero_trim(ts2), zero_trim(ts2, method = "both"))
  expect_null(zero_trim(ts3))
  expect_null(zero_trim(ts3, method = "last"))
  expect_null(zero_trim(ts3, method = "first"))
})

test_that("multivariate timeseries (1)", {
  data0 <- matrix(rep(0, 15), ncol = 3)
  rts0 <- regts(data0, start = "2017Q1", names = c("a", "b", "c"))

  data <- matrix(1:6, ncol = 3)
  prd <- period_range("2017Q2/2017Q3")
  rts <- regts(data, period = prd, names = c("a", "b", "c"))

  rts0[prd] <- rts
  rts1 <- rts0["2017Q1/2017Q3"]
  rts2 <- rts0["2017Q2/2018Q1"]

  # remove leading/trailing/all 0s
  expect_identical(zero_trim(rts0, method = "both"), rts * 1)
  expect_identical(zero_trim(rts0, method = "last"), rts1)
  expect_identical(zero_trim(rts0, method = "first"), rts2)

  expect_identical(zero_trim(rts, method = "last"), zero_trim(rts))


  rts3 <- regts(matrix(0, ncol = 2), names = c("a", "b"),
                start = "2018M1")
  expect_null(zero_trim(rts3))
  expect_null(zero_trim(rts3, method = "last"))
  expect_null(zero_trim(rts3, method = "first"))
})


test_that("multivariate timeseries (2)", {
  data0 <- matrix(rep(0, 15), ncol = 3)
  rts0 <- regts(data0, start = "2017Q1", names = c("a", "b", "c"))

  data <- matrix(1:6, ncol = 3)
  prd <- period_range("2017Q2/2017Q3")
  rts <- regts(data, period = prd, names = c("a", "b", "c"))

  rts0["2017Q2/2017Q3", "a"] <- rts[, "a"]
  rts0["2017Q3/2017Q4", "b"] <- rts[, "b"]
  rts0["2017Q4/2017Q4", "c"] <- rts[1, "c"]

  expect_identical(zero_trim(rts0, method = "both"), rts0["2017Q2/2017Q4"])
  expect_identical(zero_trim(rts0, method = "first"), rts0["2017Q2/2018Q1"])
  expect_identical(zero_trim(rts0, method = "last"), rts0["2017Q1/2017Q4"])
})

test_that("multivariate timeseries (3)", {
  data0 <- matrix(rep(0, 21), ncol = 3)
  rts0 <- regts(data0, start = "2016Q4", names = c("a", "b", "c"))

  data <- matrix(1:6, ncol = 3)
  prd <- period_range("2017Q2/2017Q3")
  rts <- regts(data, period = prd, names = c("a", "b", "c"))

  rts0["2017Q2", "a"] <- rts[1, "a"]
  rts0["2017Q4", "a"] <- rts[2, "a"]
  rts0["2017Q1", "b"] <- rts[1, "b"]
  rts0["2018Q1", "b"] <- rts[1, "b"]
  rts0["2017Q3", "c"] <- rts[1, "c"]

  expect_identical(zero_trim(rts0), rts0["2017Q1/2018Q1"])
})

test_that("preserve labels", {

  ts <- regts(c(0,1,3,0,4,8), start = "2000", labels = "tijdreeks")
  expect_identical(ts_labels(ts), ts_labels(zero_trim(ts)))

  data0 <- matrix(rep(0, 12), ncol = 3)
  rts0 <- regts(data0, start = "2017Q1", names = c("a", "b", "c"),
                labels = c("label_a", "label_b", "label_c"))

  data <- matrix(c(1,2,0,3,4,0,0,6,0), ncol = 3)
  prd <- period_range("2017Q1/2017Q3")
  rts <- regts(data, period = prd, names = c("a", "b", "c"),
               labels = c("label_a", "label_b", "label_c"))

  rts1 <- zero_trim(rts)
  expect_identical(ts_labels(rts), ts_labels(rts1))

  rts2 <- zero_trim(rts, method = "last")
  expect_identical(ts_labels(rts), ts_labels(rts2))
})

test_that("univariate timeseries with NAs", {

  ts1 <- regts(c(0, 1, 3, 0 , 4, NA), start = "2018Q1")
  ts2 <- regts(c(0, 0, NA, 0, NA, 0), start = "2018Q1")

  expect_identical(zero_trim(ts1), regts(c(1, 3, 0, 4, NA), start = "2018Q2"))
  expect_identical(zero_trim(ts2), regts(c(NA, 0, NA), start = "2018Q3"))
})

test_that("multivariate timeseries with NAs", {
  data0 <- matrix(rep(0, 15), ncol = 3)
  rts0 <- regts(data0, start = "2017Q1", names = c("a", "b", "c"))

  data <- matrix(1:6, ncol = 3)
  prd <- period_range("2017Q2/2017Q3")
  rts <- regts(data, period = prd, names = c("a", "b", "c"))
  rts[1, 2] <- NA
  rts[ , 3] <- NA

  rts0[prd] <- rts
  rts1 <- rts0["2017Q1/2017Q3"]
  rts2 <- rts0["2017Q2/2018Q1"]

  # remove leading/trailing/all 0s
  expect_identical(zero_trim(rts0, method = "both"), rts * 1)
  expect_identical(zero_trim(rts0, method = "last"), rts1)
  expect_identical(zero_trim(rts0, method = "first"), rts2)

  expect_identical(zero_trim(rts, method = "last"), zero_trim(rts))

  rts3 <- rts0
  rts3[!is.na(rts3)] <- 0
  expect_identical(zero_trim(rts3), rts3["2017Q2/2017Q3", ])
})


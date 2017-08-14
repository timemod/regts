library(testthat)
library(regts)

context("zero_trim")

test_that("univariate timeseries", {
  ts1 <- regts(c(0,1,3,0,4,8), start = "2000")
  ts2 <- regts(c(0,1,3,0,4,8,0), start = "2000")
  expect_identical(ts1, zero_trim(ts2, method = "last"))
  expect_identical(zero_trim(ts1, method = "first"), zero_trim(ts2))
  expect_identical(zero_trim(ts2), zero_trim(ts2, method = "both"))
})


test_that("multivariate timeseries", {
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

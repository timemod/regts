library(regts)
library(testthat)

context("conversion functions for timeseries")

set.seed(123)
test_that("rel2index", {
  ts1 <- regts(matrix(abs(rnorm(20)), ncol = 2), start = "2010Q2",
               names = c("a", "b"))
  print(ts1)
  ts1_rel <- diff(ts1) / abs(lag(ts1, -1))
  print(ts1_rel)
  ts1_index <- rel2index(ts1_rel)
  cat("ts1_index\n")
  print(ts1_index)
  print(ts1_index/ts1)
})

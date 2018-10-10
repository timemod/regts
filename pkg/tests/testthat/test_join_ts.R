library(regts)
library(testthat)

rm(list = ls())

context("join_ts")

# prepare input data
data <- c(1, 1:5, 5:4, 2, 2)
ts1 <- regts(data, start = "2017")
ts2 <- regts(data, start = "2010")

mts1 <- regts(matrix((data), nc = 1), start = "2017", names = "a")
mts2 <- regts(matrix((data), nc = 1), start = "2010", names = "a")
mts3 <- regts(matrix((data), nc = 1), start = "2019", names = "m")
xts1 <- regts(matrix(data = rep(data, 3), nc = 3), start = "2017",
             names = c("a", "b", "c"))
xts2 <- regts(matrix(data = rep(data, 3), nc = 3), start = "2010",
             names = c("a", "b", "c"))
xts3 <- regts(matrix(data = rep(data, 3), nc = 3), start = "2000",
              names = c("a", "b", "c"))

data2 <- c( data[1:7]*0.5, data)
tsjoined <- regts(data2, start = "2010")
mtsjoined <- regts(matrix(data2, nc = 1), start = "2010", names = "a")
xtsjoined <- regts(matrix(rep(data2, 3), nc = 3), start = "2010",
                   names = c("a", "b", "c"))


test_that("univariate example", {
  tsres <- join_ts(ts1, ts2)
  expect_equal(tsres, tsjoined)
})

test_that("multivariate 1 column example", {
  mtsres <- join_ts(mts1, mts2)
  expect_equal(mtsres, mtsjoined)
})

test_that("multivariate 3 column example", {
  xtsres <- join_ts(xts1, xts2)
  expect_equal(xtsres, xtsjoined)
})

test_that("join & add non common columns", {
  xtsres <- join_ts(xts1, mts2)
  mxts <- cbind(mtsjoined, xts1[, c("b", "c")])
  expect_equal(xtsres, mxts)

  xtsres <- join_ts(mts1, xts2)
  mxts <- cbind(mtsjoined, xts2[, c("b", "c")])
  expect_equal(xtsres, mxts)
})



test_that("no common columns, warnings ignored", {
res <- suppressWarnings(join_ts(mts3, xts1))
expect_equal(res, mts3)

res <- suppressWarnings(join_ts(xts2, mts3))
expect_equal(res, xts2)
})

# Introduce NA's in overlapping period   and elsewhere ...
xts1["2017/2018", "a"] <- NA
xts1["2017", "b"] <- NA
xts2["2019", "b"] <- NA
xts2["2018/2019", "c"] <- NA

xtsjoined_NA <- xtsjoined
xtsjoined_NA["2010/2016", "a"] <- xtsjoined["2010/2016", "a"]*2
xtsjoined_NA["2017/2018", "a"] <- c(4.0, 2.0)
xtsjoined_NA["2017", "b"] <- 2.0
xtsjoined_NA["2010/2016", "c"] <- xtsjoined["2010/2016", "c"]/2

test_that("NA's in common period", {
  xtsres <- join_ts(xts1, xts2)
  expect_equal(xtsres, xtsjoined_NA)

  # and elsewhere ..
  xts1["2023/2026", "c"] <- NA
  xtsjoined_NA["2023/2026", "c"] <- NA
  xtsres <- join_ts(xts1, xts2)
  expect_equal(xtsres, xtsjoined_NA)

})



test_that("errors", {

  expect_error(join_ts("bla", xts2), "Argument x1 \\(\"bla\"\\) is not a timeseries")

  expect_error(join_ts(xts2, xts1), "Timeseries are in wrong order!")

  expect_error(join_ts(xts1, xts3), "Timeseries have no overlap!")

  expect_error(join_ts(ts1, xts2),
              "Both timeseries must be vectors or both should have column names")

  expect_warning(join_ts(mts3, xts1),
  "No common names in two timeseries, first timeseries is returned!")

  #NA values in overlap
  xts1["2017/2019", "a"] <- NA
  expect_error(join_ts(xts1, xts2),
  "Timeseries a has no valid values in overlapping period!")



 })


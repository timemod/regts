library(regts)
library(testthat)

rm(list = ls())

context("join_ts")

# prepare input data
data <- c(1, 1:5, 5:4, 2, 2)
ts_new <- regts(data, start = "2017")
ts_old <- regts(data, start = "2010")

mts_new <- regts(matrix((data), nc = 1), start = "2017", names = "a")
mts_old <- regts(matrix((data), nc = 1), start = "2010", names = "a")
mts3 <- regts(matrix((data), nc = 1), start = "2019", names = "m")
xts_new <- regts(matrix(data = rep(data, 3), nc = 3), start = "2017",
             names = c("a", "b", "c"))
xts_old <- regts(matrix(data = rep(data, 3), nc = 3), start = "2010",
             names = c("a", "b", "c"))
xts3 <- regts(matrix(data = rep(data, 3), nc = 3), start = "2000",
              names = c("a", "b", "c"))

data2 <- c( data[1:7]*0.5, data)
tsjoined <- regts(data2, start = "2010")
mtsjoined <- regts(matrix(data2, nc = 1), start = "2010", names = "a")
xtsjoined <- regts(matrix(rep(data2, 3), nc = 3), start = "2010",
                   names = c("a", "b", "c"))


test_that("univariate example", {
  tsres <- join_ts(ts_old, ts_new)
  expect_equal(tsres, tsjoined)
})

test_that("multivariate 1 column example", {
  mtsres <- join_ts(mts_old, mts_new)
  expect_equal(mtsres, mtsjoined)
})

test_that("multivariate 3 column example", {
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoined)
})

test_that("join & add non common columns", {
  xtsres <- join_ts(xts_old, mts_new)
  mxts <- cbind(mtsjoined, xts_old[, c("b", "c")])
  expect_equal(xtsres, mxts)

  xtsres <- join_ts(mts_old, xts_new)
  mxts <- cbind(mtsjoined, xts_new[, c("b", "c")])
  expect_equal(xtsres, mxts)
})

test_that("no common columns, warnings ignored", {
res <- suppressWarnings(join_ts(mts3, xts_new))
expect_equal(res, xts_new)

res <- suppressWarnings(join_ts(xts_old, mts3))
expect_equal(res, mts3)
})

# Introduce NA's in overlapping period   and elsewhere ...
xts_new["2017/2018", "a"] <- NA
xts_new["2017", "b"] <- NA
xts_old["2019", "b"] <- NA
xts_old["2018/2019", "c"] <- NA

xtsjoined_NA <- xtsjoined
xtsjoined_NA["2010/2016", "a"] <- xtsjoined["2010/2016", "a"]*2
xtsjoined_NA["2017/2018", "a"] <- c(4.0, 2.0)
xtsjoined_NA["2017", "b"] <- 2.0
xtsjoined_NA["2010/2016", "c"] <- xtsjoined["2010/2016", "c"]/2

test_that("NA's in common period", {
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoined_NA)

  # and elsewhere ..
  xts_new["2023/2026", "c"] <- NA
  xtsjoined_NA["2023/2026", "c"] <- NA
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoined_NA)

})

test_that("errors", {

  expect_error(join_ts("bla", xts_old), "Argument old \\(\"bla\"\\) is not a timeseries")

  expect_error(join_ts(xts_new, xts_old),
               "Timeseries are in wrong order, old series should start before new!")

  expect_error(join_ts(xts3, xts_new), "Timeseries have no overlap!")

  expect_error(join_ts(ts_new, xts_old),
              "A combination of a vector and a multivariate \\(reg\\)ts is not possible")

  expect_warning(join_ts(mts3, xts_new),
  "No common names in two timeseries, new timeseries is returned!")

  #NA values in overlap
  xts_old["2017/2019", "a"] <- NA
  expect_error(join_ts(xts_old, xts_new),
  "In old and new series, combination of timeseries a has no valid values in overlapping period!")

})


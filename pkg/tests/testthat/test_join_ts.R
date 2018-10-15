library(regts)
library(testthat)

rm(list = ls())

context("join_ts")

# prepare input data
data <- c(1, 1:5, 5:4, 2, 2)
ts_new <- regts(data, start = "2017")
ts_old <- regts(data, start = "2010")
ts_NA  <- regts(rep(NA, 10), start = "2014")

test_that("identical timeseries", {
  tsres <- join_ts(ts_old, ts_old)
  expect_equal(tsres, ts_old)
})

# construct result
p_new <- get_period_range(ts_new)
overlap <- range_intersect(get_period_range(ts_old), get_period_range(ts_new))
factor <- mean(ts_new[overlap])/mean(ts_old[overlap])
result <- factor*ts_old
result[p_new] <- ts_new[p_new]

test_that("univariate example with constructed result", {
  tsres <- join_ts(ts_old, ts_new)
  expect_equal(tsres, result)
})


data2 <- c( data[1:7]*0.5, data)
tsjoin <- regts(data2, start = "2010")

test_that("univariate example", {
  tsres <- join_ts(ts_old, ts_new)
  expect_equal(tsres, tsjoin)
})

mts_new <- regts(matrix((data), nc = 1), start = "2017", names = "a")
mts_old <- regts(matrix((data), nc = 1), start = "2010", names = "a")
mts3    <- regts(matrix((data), nc = 1), start = "2019", names = "m")
mtsjoin <- regts(matrix(data2, nc = 1), start = "2010", names = "a")

test_that("multivariate 1 column example", {
  mtsres <- join_ts(mts_old, mts_new)
  expect_equal(mtsres, mtsjoin)
})


xts_new <- regts(matrix(data = rep(data, 3), nc = 3), start = "2017",
                 names = c("a", "b", "c"))
xts_old <- regts(matrix(data = rep(data, 3), nc = 3), start = "2010",
                 names = c("a", "b", "c"))
xts3    <- regts(matrix(data = rep(data, 3), nc = 3), start = "2000",
                 names = c("a", "b", "c"))
xtsjoin <- regts(matrix(rep(data2, 3), nc = 3), start = "2010",
                 names = c("a", "b", "c"))

test_that("multivariate 3 column example", {
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoin)
})

test_that("join & add non common columns", {
  xtsres <- join_ts(xts_old, mts_new)
  mxts <- cbind(mtsjoin, xts_old[, c("b", "c")])
  expect_equal(xtsres, mxts)

  xtsres <- join_ts(mts_old, xts_new)
  mxts <- cbind(mtsjoin, xts_new[, c("b", "c")])
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

xtsjoin_NA <- xtsjoin
xtsjoin_NA["2010/2016", "a"] <- xtsjoin["2010/2016", "a"]*2
xtsjoin_NA["2017/2018", "a"] <- c(4.0, 2.0)
xtsjoin_NA["2017", "b"] <- 2.0
xtsjoin_NA["2010/2016", "c"] <- xtsjoin["2010/2016", "c"]/2

test_that("NA's in common period", {
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoin_NA)

  # and elsewhere ..
  xts_new["2023/2026", "c"] <- NA
  xtsjoin_NA["2023/2026", "c"] <- NA
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(xtsres, xtsjoin_NA)

})

test_that("labels", {
  ts_labels(xts_old) <- c("a_old", "b_old", "c_old")
  ts_labels(xts_new) <- c("a_new", "b_new", "c_new")
  xtsres <- join_ts(xts_old, xts_new)
  expect_equal(ts_labels(xtsres), ts_labels(xts_new))

  # and with non common columnns
  xts4 <- regts(matrix(data = rep(data, 3), nc = 3), start = "2017",
                   names = c("d", "b", "e"), labels = c("d_new", "", "e_new"))
  xtsres <- join_ts(xts_old, xts4)
  expect_equal(ts_labels(xtsres),
               c(a = "a_old", b = "b_old", c = "c_old", d = "d_new", e = "e_new"))

})

test_that("errors", {

  expect_error(join_ts("bla", xts_old), "Argument old \\(\"bla\"\\) is not a timeseries")

  expect_error(join_ts(xts_new, xts_old),
               "Timeseries are in wrong order, old series should start before new series!")

  expect_error(join_ts(xts3, xts_new), "Timeseries have no overlap!")

  expect_error(join_ts(ts_new, xts_old),
              "Combinations of timeseries with and without column names are not possible")

  expect_warning(join_ts(mts3, xts_new),
  "No common names in two timeseries, new timeseries is returned!")

  #NA values in overlap
  xts_old["2017/2019", "a"] <- NA
  expect_error(join_ts(xts_old, xts_new),
  "No overlapping period in combination of timeseries a \\(when NA values are taken into account\\)")

  # NA timeseries
  expect_error(join_ts(ts_old, ts_NA),
               "No overlapping period! \\(when NA values are taken into account\\)")
  expect_error(join_ts(ts_NA, ts_new),
               "No overlapping period! \\(when NA values are taken into account\\)")


})


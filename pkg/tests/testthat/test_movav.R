library(regts)
library(testthat)
context("movav")

rm(list = ls())

set.seed(12345)

# movav_back calculates the backwards moving average for a univariate
# timeseries independently from regts::movav
movav_back <- function(xts, max_lag) {

  lag_ts_list <- lapply(-max_lag:-1, FUN = function(x) {lag(xts, x)})
  names(lag_ts_list) <- paste0("lag", -max_lag:-1)
  lag_ts <- do.call(cbind, lag_ts_list)
  data <- cbind(xts, lag_ts)
  data <- na_trim(data, method = "last", is_na = "any")
  movav <- rowMeans(data)

  retval <- xts
  retval[] <- movav
  return(retval)
}

movav_back_multi <- function(xts, max_lag) {
  l <- as.list(xts)
  res <- lapply(l, movav_back, max_lag)
  return(do.call(cbind, res))
}


# create some example timeseries including NA values and strange
# values
a <- regts(rnorm(20), start = "2018Q2")
a["2020Q3"] <- NA
a["2019Q2"] <- 1e40

b <- regts(rnorm(20), start = "2018Q2")
b["2018Q2"] <- NA
b["2019Q2"] <- Inf

c <- regts(as.integer(1:20), start = "2018Q2")
c["2021Q2"] <- NA_integer_

# create labelled timeseries
a_lbls <- a
b_lbls <- b
c_lbls <- c
ts_labels(a_lbls) <- "Timeseries a"
ts_labels(b_lbls) <- "Timeseries b"
ts_labels(c_lbls) <- "Timeseries c"

p <- get_period_range(a)

test_that("univariate timeseries", {
  expect_identical(movav(a), a)
  expect_equal(movav(a, max_lag = 2), movav_back(a, max_lag = 2))

  res <- movav(b_lbls, max_lag = 3)
  expect_equal(res, movav_back(b_lbls, max_lag = 3))
  expect_identical(ts_labels(res), "Timeseries b")
  expect_equal(movav(c, max_lag = 3, max_lead = 1),
               lag(movav_back(c, max_lag = 4), 1)[p])
  expect_equal(movav(c, max_lag = 2, keep_range = FALSE),
               na_trim(movav_back(c, max_lag = 2)))
  expect_equal(movav(c, max_lag = 3, max_lead = 1, keep_range = FALSE),
               na_trim(lag(movav_back(c, max_lag = 4), 1)[p]))
})

test_that("multivariate timeseries", {
  multi <- cbind(a, b, c)
  result <- movav(multi, max_lag = 1, max_lead = 1)
  expected_result <- lag(movav_back_multi(multi, max_lag = 2), 1)[p]
  expect_equal(result, expected_result)

  multi_lbl <- cbind(a_lbls, b_lbls, c_lbls)
  result <- movav(multi_lbl, max_lag = 1, max_lead = 2, keep_range = FALSE)
  expected_result <- na_trim(lag(movav_back_multi(multi_lbl, max_lag = 3), 2))
  expect_equal(result, expected_result)
})

test_that("simpel annual timeseries", {
  xts <- regts(1:10, start = "2018")
  p <- get_period_range(xts)
  xts_movav <- regts(c(NA, NA, 2:9), start = "2018")
  expect_equal(movav(xts, max_lag = 2), xts_movav)
  expect_equal(movav(xts, max_lag = 2, keep_range = FALSE), na_trim(xts_movav))
  expect_equal(movav(xts, max_lag = 1, max_lead = 1), lag(xts_movav, 1)[p])
  expect_equal(movav(xts, max_lag = 1, max_lead = 1, keep_range = FALSE),
               na_trim(lag(xts_movav, 1), method = "first"))
})


test_that("errors", {
  x <- regts("hello", period =  p)
  msg <- "Not compatible with requested type: \\[type=character; target=double\\]."
  expect_error(movav(x), msg)
  msg <- "Argument max_lag and max_lead should be >= 0"
  expect_error(movav(a, max_lag = -2), msg)
})

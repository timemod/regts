library(regts)
library(testthat)
context("movav")

rm(list = ls())

set.seed(12345)

# movav_back calculates the backwards moving average for a univariate
# timeseries independently from regts::movavc
movav_back <- function(xts, order) {
  max_lag <- order - 1
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

movav_back_multi <- function(xts, order) {
  l <- as.list(xts)
  res <- lapply(l, movav_back, order)
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
  expect_identical(movavc(a, order = 1), a)
  expect_identical(movavb(a, order = 1), a)
  expect_equal(movavb(a, order = 3), movav_back(a, order = 3))

  res <- movavb(b_lbls, order = 4)
  expect_equal(res, movav_back(b_lbls, order = 4))
  expect_identical(ts_labels(res), "Timeseries b")
  expect_equal(movavc(c, order = 3),
               lag(movav_back(c, order = 3), 1)[p])
  expect_equal(movavb(c, order = 3, keep_range = FALSE),
               na_trim(movav_back(c, order = 3)))
  expect_equal(movavc(c, order = 5, keep_range = FALSE),
               na_trim(lag(movav_back(c, order = 5), 2)[p]))
})

test_that("multivariate timeseries", {
  multi <- cbind(a, b, c)
  result <- movavc(multi, order = 3)
  expected_result <- lag(movav_back_multi(multi, order = 3), 1)[p]
  expect_equal(result, expected_result)

  multi_lbl <- cbind(a_lbls, b_lbls, c_lbls)
  result <- movavb(multi_lbl, order = 3, keep_range = FALSE)
  expected_result <- na_trim(movav_back_multi(multi_lbl, order = 3))
  expect_equal(result, expected_result)

  multi_lbl <- cbind(a_lbls, b_lbls, c_lbls)
  result <- movavb(multi_lbl, order = 2, keep_range = FALSE)
  expected_result <- na_trim(movav_back_multi(multi_lbl, order = 2))
  expect_equal(result, expected_result)
})


test_that("simpel annual timeseries", {
  xts <- regts(1:10, start = "2018")
  p <- get_period_range(xts)
  xts_movav <- regts(c(NA, NA, 2:9), start = "2018")
  expect_equal(movavb(xts, order = 3), xts_movav)
  expect_equal(movavb(xts, order = 3, keep_range = FALSE), na_trim(xts_movav))
  expect_equal(movavc(xts, order = 3), lag(xts_movav, 1)[p])
  expect_equal(movavc(xts, order = 3, keep_range = FALSE),
               na_trim(lag(xts_movav, 1), method = "first"))
})


test_that("errors", {
  x <- regts("hello", period =  p)
  msg <- "Not compatible with requested type: \\[type=character; target=double\\]."
  expect_error(movavb(x, order = 2), msg)
  msg <- "movavc not yet supported for even orders."
  expect_error(movavc(a, order = 2), msg)
})

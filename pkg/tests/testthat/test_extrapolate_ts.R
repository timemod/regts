library(regts)
library(testthat)

rm(list = ls())

# create example timeseries
x <- regts(matrix(rep(1, 4), ncol = 1), names = "a",
           start = "2019q1")
x["2019q3/2019q4"] <- 2:3
x$b <- rev(x$a)
x$c <- x$a
x["2019Q4", "c"] <- NA
x$d <- NA

range_x <- get_period_range(x)

test_that("level_constant", {
  result1 <- extrapolate_ts(x, to = "2020Q2")
  result2 <- extrapolate_ts(x, to = "2020Q2", from = "2019q2")
  expect_known_value(result1, file = "expected_output/extrapolate_level_constant1.rds")
  expect_known_value(result2, file = "expected_output/extrapolate_level_constant2.rds")

  expect_equal(result1[range_x], x)
  range <- period_range("2019q1/2019q2")
  expect_equal(result2[range], x[range])
  expect_equal(get_period_range(result1), period_range("2019Q1/2020q2"))
  expect_equal(get_period_range(result2), period_range("2019Q1/2020q2"))

  expect_equal(extrapolate_ts(x$a, to = "2020Q2"), result1$a)
  expect_equal(extrapolate_ts(x$b, to = "2020Q2", from = "2019q2"), result2$b)
  expect_equal(extrapolate_ts(x$d, to = "2020Q2", from = "2019q2"), result2$d)
})

test_that("diff_constant", {
  result1 <- extrapolate_ts(x, to = "2020Q2", method = "diff_constant")
  result2 <- extrapolate_ts(x, to = "2020Q2", from = "2019q2", method = "diff_constant")
  expect_known_value(result1, file = "expected_output/extrapolate_diff_constant1.rds")
  expect_known_value(result2, file = "expected_output/extrapolate_diff_constant2.rds")

  expect_equal(result1[range_x], x)
  range <- period_range("2019q1/2019q2")
  expect_equal(result2[range], x[range])
  expect_equal(get_period_range(result1), period_range("2019Q1/2020q2"))
  expect_equal(get_period_range(result2), period_range("2019Q1/2020q2"))

  expect_equal(extrapolate_ts(x$a, to = "2020Q2", method = "diff_constant"),
               result1$a)
  expect_equal(extrapolate_ts(x$b, to = "2020Q2", from = "2019q2",
                              method = "diff_constant"), result2$b)
  expect_equal(extrapolate_ts(x$d, to = "2020Q2", from = "2019q2",
                              method = "diff_constant"), result2$d)
})

test_that("weird", {
  result <- extrapolate_ts(x, from = "2011q2", to = "2013q1")
  expect_equal(result, x["2011q2/2013q1"])
})

test_that("errors", {
  expect_error(extrapolate_ts(x, to = "2019q2"),
               "'to' should be larger than the end period of 'x'",
               fixed = TRUE)
  expect_error(extrapolate_ts(x, to = "2020m3"),
               "'to' has a different frequency than timeseries 'x'",
               fixed = TRUE)
  expect_error(extrapolate_ts(x, to = "2020q1", from = "2019m3"),
               "'from' has a different frequency than timeseries 'x'",
               fixed = TRUE)
})

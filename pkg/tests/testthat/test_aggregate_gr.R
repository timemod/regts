library(regts)
library(testthat)

rm(list = ls())

context("aggregate_gr")

# Test aggregation of growth timeseries

set.seed(12345)

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First aggregate, then calculate difference.
agg_diff_1 <- function(x, nfrequency = 1) {
  x_y <- aggregate(x, nfrequency)
  return(diff(x_y))
}

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First calculate difference, then calculate aggregate.
agg_diff_2 <- function(x, method, nfrequency = 1) {
  x_diff <- diff(x)
  if (method == "difmean") {
    x_diff <- x_diff * (frequency(x) / nfrequency)
  }
  return(aggregate_gr(x_diff, method, nfrequency))
}

# Convert timeseries x to a relative difference timeseries with lower
# frequency nfrequency. First aggregate, then calculate difference.
agg_reldiff_1 <- function(x, nfrequency = 1) {
  x_nfreq  <- aggregate(x, nfrequency)
  ret <- growth(x_nfreq, keep_range = FALSE)
  colnames(ret) <- colnames(x)
  return (ret)
}

# Convert timeseries x to a relative difference timeseries with lower
# frequency nfrequency. First calculate difference, then calculate aggregate.
agg_reldiff_2 <- function(x, method, nfrequency = 1) {
  x_gr <- growth(x, keep_range = FALSE)
  colnames(x_gr) <- colnames(x)
  if (method == "pct") {
    x_gr <- 100 * x_gr
  }
  return(aggregate_gr(x_gr, method, nfrequency))
}

test_that("difmean and difsum, quarterly to year, single timeseries", {
  p         <- period_range("2008Q2", "2013Q3")
  ts_q      <- regts(rnorm(nperiod(p)), start = start_period(p))
  ref <- agg_diff_1(ts_q) # the correct result
  expect_equal(agg_diff_2(ts_q, method = "difmean"), ref)
  expect_equal(agg_diff_2(ts_q["2008Q4/"], method = "difsum"), ref)
  expect_equal(agg_diff_2(ts_q["2009Q1/"], method = "difmean"), ref)
})

test_that("pct and rel, quarterly to year, single timeseries", {
  p         <- period_range("2008Q2", "2013Q3")
  ts_q      <- regts(abs(rnorm(nperiod(p))), start = start_period(p))
  ref <- agg_reldiff_1(ts_q) # the correct result
  expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref)
  expect_equal(agg_reldiff_2(ts_q["2009Q1/"], method = "pct"), ref * 100)
})

test_that("difmean and rel, monthly to quarterly, two timeseries", {
  p <- period_range("2010M11", "2011M11")
  ts_m <- regts(matrix(abs(rnorm(nperiod(p) * 2)), ncol = 2),
                start = start_period(p), names = c("a", "b"),
                labels = c("ts a", "ts b"))
  ref_abs <- agg_diff_1(ts_m, nfrequency = 4)
  expect_equal(agg_diff_2(ts_m, method = "difmean", nfrequency = 4), ref_abs)
  ref_rel <- agg_reldiff_1(ts_m, nfrequency = 4)
  expect_equal(agg_reldiff_2(ts_m["2011M1/"], method = "rel", nfrequency = 4),
               ref_rel)
})

test_that("difsum/difmean/rel, quarterly to year, single timeseries with NA values", {
  p         <- period_range("2009Q1", "2015Q4")
  ts_q      <- regts(abs(rnorm(nperiod(p))), start = start_period(p))
  ts_q["2009Q1"] <- NA
  ts_q["2012Q2"] <- NA
  ts_q["2015Q4"] <- NA
  ref_abs <- agg_diff_1(ts_q) # the correct result
  expect_equal(agg_diff_2(ts_q, method = "difmean"), ref_abs)
  ref_rel <- agg_reldiff_1(ts_q) # the correct result
  expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref_rel)
})

test_that("difmean/difsum/reldif, timeseries with Inf, -Inf or NaN values", {
  p  <- period_range("2009Q1", "2015Q4")
  ts_q <- regts(abs(rnorm(nperiod(p))), start = start_period(p))
  ts_q["2009Q1"] <- 1/0
  ts_q["2012Q2"] <- 0/0
  ts_q["2015Q4"] <- -1/0
  ref_abs <- agg_diff_1(ts_q) # the correct result
  expect_equal(agg_diff_2(ts_q, method = "difmean"), ref_abs)
  ref_rel <- agg_reldiff_1(ts_q) # the correct result
  ref_rel['2015'] <- NaN  # the pct and dif1 methods  cannot distinguish
  ts_q["2015Q4"] <- 1/0
  #Inf, Inf and NaN
  expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref_rel)
})

test_that("errors", {
  msg <- "Argument x is not a timeseries"
  expect_error(aggregate_gr("aap"), msg)

  msg <- "Argument x should be a numeric timeseries"
  ts_t <- regts(c("a", "b", "c"), start = "2017Q1")
  expect_error(aggregate_gr(ts_t), msg)

  msg <- "Method name 'dif1s' is obsolete, use 'difmean' instead"
  p  <- period_range("2009Q1", "2015Q4")
  ts_q <- regts(abs(rnorm(nperiod(p))), start = start_period(p))
  expect_warning(aggregate_gr(ts_q, method = "dif1s"), msg)

})

test_that("rel and pct for negative timeseries", {
  t1 <- regts(c(1, -3, 4, -2, -2, -1, 0.1, 1, 2, 3, -2, -3), start = "2010q1")
  r1 <- growth(t1)
  expect_error(aggregate_gr(r1, method = "rel"),
               "Relative growth smaller than -1 for one or more periods.")

  r2 <- 100 * cbind(x = r1, y = 2 * r1)
  expect_error(aggregate_gr(r2, method = "pct"),
               "Relative growth smaller than -1 for one or more periods for columns: x, y.")

  r3 <- r2
  r3$x <- ifelse(r3$x < -100, -100, r3$x)

  expect_error(aggregate_gr(r3, method = "pct"),
               "Relative growth smaller than -1 for one or more periods for columns: y.")

})

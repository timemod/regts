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

#
# alternative implementations of the pct and rel methods
#
aggregate_rel <- function(x, nfrequency = 1) {
  x_i <- rel2index(na_trim(x), keep_range = FALSE)[get_period_range(x)]
  x_i_agg <- aggregate(x_i)
  return(growth(x_i_agg, keep_range = FALSE))
}
aggregate_pct <- function(x, nfrequency = 1) {
  return(100 * aggregate_rel(x / 100, nfrequency = nfrequency))
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
  agg1 <- aggregate_gr(r1, method = "rel")
  expect_equal(agg1, aggregate_rel(r1))

  r2 <- 100 * cbind(x = r1, y = 2 * r1)
  agg2 <- aggregate_gr(r2, method = "pct")
  expect_equal(agg2$x, aggregate_pct(r2$x))
  expect_equal(agg2$y, aggregate_pct(r2$y))

  r3 <- r2
  r3$x <- ifelse(r3$x < -100, -100, r3$x)
  agg3 <- aggregate_gr(r3, method = "pct")
  expect_equal(agg3$x, aggregate_pct(r3$x))
  expect_equal(agg3$y, aggregate_pct(r3$y))
})


test_that("rel and pct and NA values", {

  r0 <- regts(c(1:32), start = "2010q1")

  r1 <- r0
  r1["2012q3"] <- NA

  agg1 <- aggregate_gr(r1, method = "rel")
  expect_equal(agg1["2011"], aggregate_rel(r1)["2011"])
  expect_equal(agg1["2012/2013"], regts(NA_real_, period = "2012/2013"))
  expect_equal(agg1["2014/"], aggregate_rel(r1["2013/"]))

  r2 <- 100 * cbind(x = r0, y = 2 * r0)
  r2$x["2012q1"] <- NA
  r2$y["2014q4"] <- NA
  r2$x["2013Q3"] <- -200
  agg2 <- aggregate_gr(r2, method = "pct")

  expect_equal(agg2$x["2011"], aggregate_pct(r2$x)["2011"])
  expect_equal(agg2$x["2012"], regts(NA_real_, period = "2012"))
  expect_equal(agg2$x["2013/"], aggregate_pct(r2$x["2012/"]))

  expect_equal(agg2$y["2011/2013"], na_trim(aggregate_pct(r2$y)))
  expect_equal(agg2$y["2014/2015"], regts(NA_real_, period = "2014/2015"))
  expect_equal(agg2$y["2016/"], aggregate_pct(r2$y["2015/"]))
})


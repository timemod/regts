library(regts)
context("aggregate_gr")

# Test aggregation of growth timeseries

set.seed(12345)

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First aggregate, then calculate difference.
agg_diff_1 <- function(x, nfrequency = 1) {
    x_y <- aggregate(x, nfrequency)
    return (diff(x_y))
}

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First calculate difference, then calculate aggregate.
agg_diff_2 <- function(x, method, nfrequency = 1) {
    x_diff <- diff(x)
    if (method == "dif1s") {
        x_diff <- x_diff * (frequency(x) / nfrequency)
    }
    return (aggregate_gr(x_diff, method, nfrequency))
}

# Convert timeseries x to a relative difference timeseries with lower
# frequency nfrequency. First aggregate, then calculate difference.
agg_reldiff_1 <- function(x, nfrequency = 1) {
    x_nfreq  <- aggregate(x, nfrequency)
    ret <- diff(x_nfreq) / lag(x_nfreq, -1)
    colnames(ret) <- colnames(x)
    return (ret)
}

# Convert timeseries x to a relative difference timeseries with lower
# frequency nfrequency. First calculate difference, then calculate aggregate.
agg_reldiff_2 <- function(x, method, nfrequency = 1) {
    x_diff <- diff(x) / lag(x, - 1)
    colnames(x_diff) <- colnames(x)
    if (method == "pct") {
        x_diff <- 100 * x_diff
    }
    return (aggregate_gr(x_diff, method, nfrequency))
}

test_that("dif1s and dif1, quarterly to year, single timeseries", {
    p         <- period_range("2008Q2", "2013Q3")
    ts_q      <- regts(rnorm(nperiod(p)), start = start_period(p))
    ref <- agg_diff_1(ts_q) # the correct result
    expect_equal(agg_diff_2(ts_q, method = "dif1s"), ref);
    expect_equal(agg_diff_2(ts_q["2008Q4/"], method = "dif1"), ref);
    expect_equal(agg_diff_2(ts_q["2009Q1/"], method = "dif1s"), ref);
})

test_that("pct and rel, quarterly to year, single timeseries", {
    p         <- period_range("2008Q2", "2013Q3")
    ts_q      <- regts(rnorm(nperiod(p)), start = start_period(p))
    ref <- agg_reldiff_1(ts_q) # the correct result
    expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref);
    expect_equal(agg_reldiff_2(ts_q["2009Q1/"], method = "pct"), ref * 100);
})

test_that("dif1s and rel, monthly to quarterly, two timeseries", {
    p <- period_range("2010M11", "2011M11")
    ts_m <- regts(matrix(rnorm(nperiod(p) * 2), ncol = 2),
                 start = start_period(p), names = c("a", "b"),
                 labels = c("ts a", "ts b"))
    ref_abs <- agg_diff_1(ts_m, nfrequency = 4)
    expect_equal(agg_diff_2(ts_m, method = "dif1s", nfrequency = 4), ref_abs);
    ref_rel <- agg_reldiff_1(ts_m, nfrequency = 4)
    expect_equal(agg_reldiff_2(ts_m["2011M1/"], method = "rel", nfrequency = 4),
                 ref_rel);
})

test_that("dif1s and dif1, quarterly to year, single timeseries with NA values", {
    p         <- period_range("2009Q1", "2015Q4")
    ts_q      <- regts(rnorm(nperiod(p)), start = start_period(p))
    ts_q["2009Q1"] <- NA
    ts_q["2012Q2"] <- NA
    ts_q["2015Q4"] <- NA
    ref_abs <- agg_diff_1(ts_q) # the correct result
    expect_equal(agg_diff_2(ts_q, method = "dif1s"), ref_abs);
    ref_rel <- agg_reldiff_1(ts_q) # the correct result
    expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref_rel);
})

test_that("dif1s and dif1, quarterly to year, single timeseries with Inf, -Inf or
          NaN values", {
    p         <- period_range("2009Q1", "2015Q4")
    ts_q      <- regts(rnorm(nperiod(p)), start = start_period(p))
    ts_q["2009Q1"] <- 1/0
    ts_q["2012Q2"] <- 0/0
    ts_q["2015Q4"] <- -1/0
    ref_abs <- agg_diff_1(ts_q) # the correct result
    expect_equal(agg_diff_2(ts_q, method = "dif1s"), ref_abs);
    ref_rel <- agg_reldiff_1(ts_q) # the correct result
    ref_rel['2015'] <- NaN  # the pct and dif1 methods  cannot distinguish
                              #Inf, Inf and NaN
    expect_equal(agg_reldiff_2(ts_q, method = "rel"), ref_rel);
})

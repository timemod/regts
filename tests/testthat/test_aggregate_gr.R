library(regts)
context("aggregate_gr")

# Test aggregation of growth timeseries

set.seed(12345)

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First aggregate, then calculate difference.
agg_diff1 <- function(x, nfrequency = 1) {
    x_y <- aggregate(x, nfrequency)
    return (diff(x_y))
}

# Convert timeseries x to a first difference timeseries with lower
# frequency nfrequency. First calculate difference, then calculate aggregate.
agg_diff2 <- function(x, method, nfrequency = 1) {
    x_diff <- (frequency(x) / nfrequency) * diff(x)
    return (aggregate_gr(x_diff, method, nfrequency))
}

test_that("quarterly to year, single timeseries", {
    p         <- regperiod_range("2008Q3", "2013Q3")
    ts_q      <- regts(rnorm(lensub(p)), start = start_period(p))
    ref <- agg_diff1(ts_q) # the correct result
    x1 <- agg_diff2(ts_q, method = "cgr")
    expect_equal(agg_diff2(ts_q, method = "cgr"), ref);
    expect_equal(agg_diff2(ts_q["2009Q1/"], method = "cgr"), ref);
})

# test_that("monthly to quarterly, two timeseries", {
#     p <- regperiod_range("2010M2", "2011M11")
#     tsm <- regts(rnorm(lensub(p) * 2), start = start_period(p))
#     ts_q1 <- aggregate(tsm, nfrequency = 4)
#     ts_q2 <- aggregate(tsm["2010M3/"], nfrequency = 4)
#     ts_q3 <- aggregate(tsm["2010M4/"], nfrequency = 4)
#     expect_equal(ts_q1, ts_q2)
#     expect_equal(ts_q1, ts_q3)
# })

library(regts)
library(testthat)

context("conversion functions for timeseries")

rm(list = ls())

set.seed(123)

data <- c(-0.56047565, -0.23017749,  1.55870831,  0.07050839,  0.12928774,
           1.71506499,  0.46091621, -1.26506123, -0.68685285, -0.44566197,
           1.22408180,  0.35981383,  0.40077145,  0.11068272, -0.55584113,
           1.78691314,  0.49785048, -1.96661716,  0.70135590, -0.47279141)

test_that("rel2index univariate timeseries", {
  ts1 <- regts(data[1:10], start = "2010Q2")
  ts1 <- 100 * ts1 / ts1[1]
  ts1_rel <- diff(ts1) / lag(ts1, -1)
  ts1_index <- rel2index(ts1_rel, keep_range = FALSE)
  expect_equal(ts1, ts1_index)

  ts1_index_2 <- rel2index(ts1_rel["2010q3"], keep_range = TRUE)
  expect_equal(ts1["2010q3"], ts1_index_2)

  expect_warning(ts1_index2 <- rel2index(ts1_rel, base = "2010Q4"),
                 "Negative \\(average\\) value at base period 2010Q4.")
  expected <- (100 * ts1 / abs(as.numeric(ts1["2010Q4"])))[get_period_range(ts1_rel)]
  expect_equal(ts1_index2, expected)

  ts1_index3 <- rel2index(ts1_rel, "2010q3")
  expected <- (100 * ts1 / as.numeric(ts1["2010Q3"]))[get_period_range(ts1_rel)]
  expect_equal(ts1_index3, expected)

  # NA values at beginning
  ts2_rel <- ts1_rel
  ts2_rel["2009q4"] <- NA
  expect_equal(rel2index(ts2_rel), ts2_rel * NA)

  expect_warning(
    expect_equal(rel2index(ts2_rel, base = "2009q4"), ts2_rel * NA),
    "NA values in base period 2009Q4"
  )

  expect_warning(ts2_index2 <- rel2index(ts2_rel, base = "2010Q4"),
                 "Negative \\(average\\) value at base period 2010Q4.")
  expected <- (100 * ts1 / abs(as.numeric(ts1["2010Q4"])))[get_period_range(ts2_rel)]
  expect_equal(ts2_index2, expected)

  expect_silent(ts2_index3 <- rel2index(ts2_rel, "2010q3"))
  expected <- (100 * ts1 / as.numeric(ts1["2010Q3"]))[get_period_range(ts2_rel)]
  expect_equal(ts2_index3, expected)

  ts3_rel <- ts1_rel
  ts3_rel <- ts3_rel * NA
  ts3_rel[1] <- 2
  expect_equal(rel2index(ts3_rel), ts3_rel * (300/ 2))

  expect_equal(rel2index(ts3_rel, base = "2010q3"), ts3_rel * (100/ 2))

  ts4_rel <- ts1_rel
  ts4_rel["2009q3"] <- NA
  ts4_rel["2009q4"] <- 2
  expected <- (100 * ts1 / as.numeric(ts1["2010Q3"]))[get_period_range(ts4_rel)]
  expect_equal(rel2index(ts4_rel, "2010q3"), expected)
})

test_that("rel2index multivariate timeseries", {
    ts1 <- regts(matrix(data, ncol = 2), start = "2010Q2",
                 names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
    ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
    ts1_rel <- diff(ts1) / lag(ts1, -1)
    ts1_index <- rel2index(ts1_rel, scale = 1, keep_range = TRUE)
    p <- get_period_range(ts1_rel)
    expect_equal(ts1[p], ts1_index[p])

    ts1_index_2 <- rel2index(ts1_rel["2010q3"], keep_range = TRUE, scale = 1)
    expect_equal(ts1["2010q3"], ts1_index_2)

    expect_warning(ts1_index_3 <- rel2index(ts1_rel, base = "2011Q3", scale = 1),
                 "Negative \\(average\\) value at base period 2011Q3 for columns: a.")
    prd <- get_period_range(ts1_rel)
    expected_result <- ts1 * rep(1 / abs(as.numeric(ts1["2011q3"])),
                                 each = nrow(ts1))
    expected_result <- expected_result[prd]
    expect_equal(ts1_index_3,  expected_result)

    expect_warning(ts1_index_4 <- rel2index(ts1_rel, base = "2011Q2", scale = 1),
                 "Negative \\(average\\) value at base period 2011Q2 for columns: a, b.")
    expected_result <- ts1 * rep(1 / abs(as.numeric(ts1["2011q2"])),
                                 each = nrow(ts1))
    expected_result <- expected_result[prd]
    expect_equal(ts1_index_4,  expected_result)

    ts1_index5 <- rel2index(ts1_rel, base = "2012Q2", scale = 1,
                            keep_range = FALSE)
    expected <- ts1
    i <- period("2012Q2") - period("2010Q2") + 1
    expected[] <- apply(expected, MARGIN = 2, FUN = function(x) {x /x[i]})
    expect_equal(ts1_index5, expected)

  # empty timeseries
  ts1_rel_empty <- ts1_rel[ , character(0)]
  ts1_empty <- ts1[ , character(0)]
  expect_equal(rel2index(ts1_rel_empty, keep_range = FALSE), ts1_empty)
  expect_equal(rel2index(ts1_rel_empty, keep_range = TRUE), ts1_empty[p],
               check.attributes = FALSE)

  ts2_rel <- ts1_rel
  ts2_rel["2010q3", "a"] <- NA

  expected <- ts1[get_period_range(ts2_rel)]
  expected$a <- NA
  expected$b <- 100 * expected$b
  expect_equal(rel2index(ts2_rel), expected)

  expected <- index_ts(ts1, base = "2012q2")
  expected["2010q2", "a"] <- NA

  expect_equal(rel2index(ts2_rel, base = "2012q2", keep_range = FALSE),
               expected)
})

test_that("pct2index", {
  ts1 <- regts(matrix(data, ncol = 2), start = "2010Q2",
               names = c("a", "b"), labels = paste("Timeseries", c("a", "b")))
  ts1[] <- apply(ts1, MARGIN = 2, FUN = function(x) {x /x[1]})
  ts1_rel <- 100 * diff(ts1) / lag(ts1, -1)
  ts1_index <- pct2index(ts1_rel, scale = 1)
  p <- get_period_range(ts1_rel)
  expect_equal(ts1[p], ts1_index[p])

  ts1_index2 <- pct2index(ts1_rel, scale = 1, keep_range = FALSE)
  expect_equal(ts1, ts1_index2)
})

test_that("NA values", {
  ts1 <- regts(c(1L, 2L, 3L, NA, 5L, 6L), start = "2010Q2")
  ts1_rel <- diff(ts1) / lag(ts1, -1)
  ts1_index <- rel2index(ts1_rel, scale = 1, keep_range = FALSE)
  expected_result <- ts1
  expected_result["2011Q2/"] <- NA
  expect_equal(ts1_index, expected_result)
})

test_that("Inf values", {
  ts1 <- regts(c(1L, 2L, 3L, Inf, 5L, 6L), start = "2010M2")
  ts1_rel <- diff(ts1) / lag(ts1, -1)
  ts1_index <- rel2index(ts1_rel, scale = 1)
  expected_result <- ts1[get_period_range(ts1_rel)]
  expected_result["2010m6/"] <- NaN
  expect_equal(ts1_index, expected_result)
})

test_that("errors", {
  ts1 <- regts(c(1, 2, 3), start = "2010Q2")
  msg <- "Base period \\(2018Q3\\) not within timeseries period \\(2010Q1/2010Q4\\)"
  expect_error(rel2index(ts1, base = "2018Q3"), msg)
  msg <- paste("Base period \\(2018M03\\) should not have a higher frequency",
               "than the input timeseries \\(4\\)")
  expect_error(rel2index(ts1, base = "2018M3"), msg)

  ts2 <- lag_ts(ts1, keep_range = FALSE)

  expect_error(rel2index(ts2, base = "2010"),
               paste("Base period \\(2010Q1/2010Q4\\) not within timeseries",
                      "period \\(2010Q2/2011Q1\\)"))

  msg <- "Argument 'scale' should be a numeric vector of length 1"
  expect_error(rel2index(ts1, scale = "xxx"), msg)
  expect_error(rel2index(ts1, scale = 1:2), msg)

})

test_that("Base period is period range, univariate series", {
  t1 <- regts(abs(rnorm(20)), start = "2010q1", labels = "series")
  i1 <- index_ts(t1, base = "2010")
  r1 <- growth(i1, keep_range = FALSE)
  i2 <- rel2index(r1, base = "2010", keep_range = FALSE)
  expect_identical(ts_labels(i2), "series")
  expect_equal(i1, i2)
  i3 <- rel2index(r1, base = "2010q1/2010q4", keep_range = FALSE)
  expect_equal(i1, i3)
})

test_that("Base period is period range, multivariate series", {
  t1a <- regts(abs(rnorm(20)), start = "2010q2", labels = "series1")
  t1b <- 2 * t1a
  ts_labels(t1b) <- "series2"
  t1 <- cbind(t1a, t1b)
  i1 <- index_ts(t1, base = "2010Q2/2010q3")
  r1 <- growth(i1, keep_range = FALSE)
  i2 <- rel2index(r1, base = "2010q2/2010q3", keep_range = FALSE)
  expect_identical(ts_labels(i2), c(t1a = "series1", t1b = "series2"))
  expect_equal(i1, i2)
  i3 <- rel2index(r1, base = "2010q2/2010q3", keep_range = TRUE)
  expect_equal(i1["2010q3/"], i3)
})

test_that("negative timeseries (1)", {
  i1 <- regts(c(1, -3, 4, -2, -2, -1, 0, 1), start = "2010q2")
  r1 <- growth(i1, keep_range = FALSE)
  i2 <- rel2index(r1, scale  = 1, keep_range = FALSE)

  expected_result <- i1
  expected_result["2012q1"] <- NaN
  expect_equal(i2,  expected_result)

  msg <- "Zero \\(average\\) value at base period 2010Q4/2011Q2."
  expect_warning(i3 <- rel2index(r1, base = "2010q4/2011q2", keep_range = FALSE),
                 msg)
  expect_warning(i4 <- index_ts(i2, base = "2010q4/2011q2"))

  expect_equal(i3, i4)

  r3 <- cbind(x = r1, y = 2 * r1)
  r3["2012q1", "x"] <- 2

  i5 <- rel2index(r3, scale = 1, keep_range = FALSE)

  x_expected <- i1
  x_expected["2012q1"] <- 0
  expect_equal(i5$x, x_expected)

  i5_y_gr <- growth(i5$y, keep_range = FALSE)

  expected_result <- r3$y
  expected_result["2011q4/2012q1"] <- NaN
  expect_equal(i5_y_gr, expected_result)

  msg <-"Zero \\(average\\) value at base period 2011Q4 for columns: x, y."
  expect_warning(
   i6 <- rel2index(r3, scale = 1, keep_range = FALSE, base = "2011q4"),
   "Zero \\(average\\) value at base period 2011Q4 for columns: x, y.")
  expect_warning(i7 <- index_ts(i5, base = "2011q4"), msg)
  expect_equal(i6, i7)

  expect_warning(i8 <- rel2index(r3, scale = 100, keep_range = FALSE, base = "2011q1"),
               "Negative \\(average\\) value at base period 2011Q1 for columns: x, y.")
  expect_warning(i9 <- index_ts(i5, base = "2011q1"))
  expect_equal(i8, i9)

  expect_warning(i10  <-rel2index(r3, scale = 100, keep_range = FALSE, base = "2011q3"),
               "Negative \\(average\\) value at base period 2011Q3 for columns: x.")
  expect_warning(i11 <- index_ts(i5, base = "2011q3"))
  expect_equal(i10, i11)

  expect_warning(
    i12 <- rel2index(r3[ , "y"], scale = 100, base = "2012q1"),
    "NA values in base period 2012Q1")
  expect_equal(i12, regts(NaN, period = "2010q3/2012q1"))
})

test_that("negative timeseries (2)", {

  # create a timeseries that starts with a positive value
  t1 <- regts(c(1, rnorm(10)), start = "2019q1")
  t2 <- pct2index(100 * growth(t1, keep_range = FALSE), keep_range = FALSE,
                  scale = 1)
  expect_equal(t1, t2)

  t3 <- cbind(x = t1, y = 1 / t1)
  ts_labels(t3) <- c(x = "var x ", y = "var y")
  t4 <- pct2index(100 * growth(t3, keep_range = FALSE), keep_range = FALSE,
                  scale = 1)
  expect_equal(t3, t4)
})

test_that("example extrapolation of timeseries with growth series", {
  z_r <- regts(abs(rnorm(10, mean = 1)) + 1e-3, start = "2018m1")
  per <- get_period_range(z_r)
  z <- regts(10, start = "2017m12")
  per0 <- period("2017m12")
  z[per] <- pct2index(z_r, scale = z[per0])

  z_ref <- z[per0]
  z_ref[per]  <- c(z[per0]) * cumprod(1 + z_r / 100)
  expect_equal(z, z_ref)
})

library(testthat)
library(regts)

context("regts")

rm(list = ls())

test_that("constructor regts for univariate timeseries", {

  regts1 <- regts(1:10, start = "2010Q4")
  ts1 <- ts(1:10, start = c(2010,4), frequency = 4)
  expect_identical(regts1, as.regts(ts1))
  expect_identical(as.ts(regts1), ts1)
  expect_identical(class(regts1), c("regts", "ts"))
  expect_identical(is.regts(regts1), TRUE)
  expect_identical(is.regts(ts1), FALSE)
  expect_identical(is.ts(regts1), TRUE)

  r1 <- period_range("2010Q1", "2012Q2")
  regts1 <- regts(matrix(1:10, ncol = 1), period = r1, names = "a")
  ts1 <- ts(matrix(1:10, ncol = 1), start = c(2010,1), frequency = 4, names = "a")
  expect_identical(regts1, as.regts(ts1))
  expect_identical(as.ts(regts1), ts1)

  regts1 <- regts(1:10, start = "2010", end = "2012")
  expect_identical(regts1, as.regts(ts(1:10, start = 2010, end = 2012, frequency = 1)))

  expect_error(regts(1:10, start = "2010-1"),
               "Frequency of period 2010-1 unknown. Specify argument frequency.")
  expect_error(regts(1:10, start = "2010-1", period = period_range("2010Q1")),
               "Arguments 'start' and 'period' exclude each other.")

  regts1 <- regts(1:10, start = "2010-1", frequency = 4)
  ts1 <- ts(1:10, start = c(2010, 1), frequency = 4, names = NULL)
  expect_identical(as.regts(regts1), as.regts(ts1))

  m <- matrix(1:10, ncol = 1)
  regts1 <- regts(m, start = "2010-1", frequency = 3, names = "a")
  ts1 <- ts(m, start = c(2010, 1), frequency = 3, names = "a")
  expect_identical(as.regts(regts1), as.regts(ts1))


  expect_error(regts(numeric(0), start = "2010q1"),
               "`regts` object must have one or more observations")
  expect_error(regts(matrix(nrow = 0, ncol = 2), start = "2010q1"),
               "`regts` object must have one or more observations")
})

test_that("constructor regts for multivariate time series", {
  data <- matrix(1: 9, ncol = 3)
  colnames(data) <- c("a", "b", "c")
  regts1 <- regts(data, start = "2010Q4")
  ts1 <- ts(data, start = c(2010,4), frequency = 4)
  expect_identical(regts1, as.regts(ts1))
  expect_identical(class(regts1), c("regts", "mts", "ts", "matrix"))
  expect_identical(is.regts(regts1), TRUE)
  expect_identical(is.regts(ts1), FALSE)
  expect_identical(is.ts(regts1), TRUE)
  expect_identical(is.mts(regts1), TRUE)
  expect_identical(is.matrix(regts1), TRUE)

  regts1 <- regts(data, start = "2010M1", end = "2012m3")
  ts1 <- ts(data, start = c(2010,1), end = c(2012,3), frequency = 12)
  expect_identical(regts1, as.regts(ts1))
})

test_that("get_period_range / get_periods", {

  range1 <- period_range("2010q1/2011q4")
  regts1 <- regts(1, period = range1)
  expect_identical(get_period_range(regts1), range1)

  # test get_periods
  expected_result <- start_period(range1) + (0:7)
  expect_identical(get_periods(regts1), expected_result)
  expect_identical(get_periods(range1), expected_result)

  regts2 <- regts(c("aap", "noot", "mies"), start = "2010M1", end = "2011M4")
  expect_identical(get_period_range(regts2),
                   period_range("2010M1", "2011M4"))
})

test_that("arguments: start, end & period", {
  regts1 <- regts(1:5, start = "2010Q1", end = "2011Q1")
  regts2 <- regts(1:5, start = "2010Q1")
  regts3 <- regts(1:5, end = "2011Q1")
  regts4 <- regts(1:5, period = "2010Q1/2011Q1")
  regts5 <- regts(1:5, period = "2010Q1/")
  regts6 <- regts(1:5, period = "/2011Q1")
  expect_identical(regts1, regts2)
  expect_identical(regts1, regts3)
  expect_identical(regts1, regts4)
  expect_identical(regts1, regts5)
  expect_identical(regts1, regts6)

  expect_error(regts(1:5, start = period(c("2010Q1", "2010q2"))),
               "Argument 'start' must be of length 1")
  expect_error(regts(1:5, end = period(c("2010Q1", "2010q2"))),
               "Argument 'end' must be of length 1")
})

test_that("period selection in univariate timeseries", {
  ts1 <- ts(1:8, start = c(2010, 1), end = c(2011, 4), frequency = 4,
            names = NULL)
  regts1 <- as.regts(ts1)

  regts2 <- regts1["2010Q2/2011Q3"]
  expect_is(regts2, "regts")
  expect_identical(regts2,
                   as.regts(window(ts1, start = c(2010,2), end = c(2011,3))))

  regts2 <- regts1["2008/2012"]
  expect_is(regts2, "regts")
  expect_identical(regts2,
                   as.regts(window(ts1, start = c(2008,1), end = c(2012,4),
                                   extend = TRUE)))

  regts2 <- regts1["2008/"]
  expect_is(regts2, "regts")
  expect_identical(regts2,
                   as.regts(window(ts1, start = c(2008,1), extend = TRUE)))

  regts2 <- regts1["/2011Q2"]
  expect_is(regts2, "regts")
  expect_identical(regts2,
                   as.regts(window(ts1, end = c(2011,2), extend = TRUE)))

  expect_identical(regts1[3], ts1[3])
  expect_identical(regts1[3:10], ts1[3:10])

  msg <- "The start period \\(2010Q1\\) is after the end period \\(2011Q4\\)."
  expect_error(regts1["2018Q3/"], msg)
  expect_error(regts1["2018Q3/"] <- 2, msg)
  msg <- "The start period \\(2010Q1\\) is after the end period \\(200Q3\\)."
  expect_error(regts1["/200Q3"] <- 2, msg)

  # period selection outside range
  expect_identical(regts1["2018Q1"], regts(NA_integer_, period = "2018q1"))

  expect_error(regts1[period(c("2010q2", "2010q3"))],
               "'x' must be a period of length 1")

  expect_error(regts1[c("2010q2", "2010q3")],
               "x should be a single character string")
})

test_that("period / column selection in multivariate timeseries", {
  data <- matrix(1: 9, ncol = 3)
  ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b", "c"))
  regts1 <- as.regts(ts1)

  regts2 <- regts1[, 'b']
  expect_is(regts2, "regts")
  expect_identical(regts2, as.regts(ts1[, 'b']))

  regts3 <- regts1[, 'b', drop = FALSE]
  expect_identical(regts3, as.regts(ts1[, 'b', drop = FALSE]))

  expect_identical(regts1[, c("b", "a")], as.regts(ts1[, c("b", "a")]))

  expect_identical(regts1['2011Q1', "c"], as.regts(
    window(ts1[, "c"], start = c(2011,1), end = c(2011,1))))

  expect_identical(regts1['2011Q1', c("b", "a")], as.regts(
    window(ts1, start = c(2011,1), end = c(2011,1)))[, c("b", "a"),
                                                     drop = FALSE])

  expect_identical(regts1['2011'], as.regts(
    window(ts1, start = c(2011,1), end = c(2011,4), extend = TRUE)))

  expect_identical(regts1['2011Q1', c("a", "b")], as.regts(
    window(ts1, start = c(2011,1), end = c(2011,1), extend = TRUE)
    [, c("a", "b"), drop = FALSE]))
})


test_that("period selection at the lhs of a univariate timeseries", {

  ts1 <- ts(1:8, start = c(2010, 1), end = c(2011, 4), frequency = 4)
  regts1 <- as.regts(ts1)

  # without period extension
  regts2 <- regts1
  regts2['2010Q2'] <- 2
  regts2['2011Q3/'] <- 2 * regts1['2011Q3/']
  ts2 <- ts1
  window(ts2, start = c(2010, 2), end = c(2010,2)) <- 2
  window(ts2, start = c(2011, 3)) <- 2 * window(ts1, start = c(2011, 3))
  expect_identical(regts2, as.regts(ts2))

  # with period extension
  regts2 <- regts1
  regts2['/2013Q2'] <- 9
  regts2['2008Q2/2009Q2'] <- regts1['2010Q2/2011Q2']
  ts2 <- ts1
  suppressWarnings({
    window(ts2, end = c(2013,2), extend = TRUE) <- 9
    window(ts2, start = c(2008, 2), end = c(2009, 2), extend = TRUE) <-
      window(ts1, start = c(2010, 2), end = c(2011,2))
  })
  expect_equivalent(regts2, as.regts(ts2))

  # with period extension and period_range objects
  regts2 <- regts1
  range1 <- period_range(NULL, "2013Q2")
  regts2[range1] <- 9
  range2 <- period_range('2008Q2/2009Q2')
  range3 <- period_range('2010Q2/2011Q2')
  regts2[range2] <- regts1[range3]
  ts2 <- ts1
  suppressWarnings({
    window(ts2, end = c(2013,2), extend = TRUE) <- 9
    window(ts2, start = c(2008, 2), end = c(2009, 2), extend = TRUE) <-
      window(ts1, start = c(2010, 2), end = c(2011,2))
  })
  expect_equivalent(regts2, as.regts(ts2))
})


test_that("period and column selection at the lhs of an assignment", {

  data <- matrix(1: 10, ncol = 2)
  ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b"))
  regts1 <- as.regts(ts1)

  regts2 <- regts1
  regts2['2011Q1/'] <- 2 * regts1['2011Q1/']
  regts2['/2011Q1', 'a'] <- c(10, 20)

  ts2 <- ts1
  window(ts2, start = c(2011, 1)) <- 2 * window(ts1, start = c(2011,1))
  window(ts2, end = c(2011, 1))[, 'a'] <- c(10, 20)

  expect_identical(regts2, as.regts(ts2))

  regts2 <- regts1
  regts2[, 'x'] <- 1
  regts2['2010Q1'] <- 2 * regts2['2011Q1']
  regts2['2012Q3/2012Q4', 'xx'] <- 2 * regts2['2011Q2/2011Q3', 'x']
  regts2['2012Q3/',  c('a_2', 'b_2')] <- regts1['2011Q2/2011Q3',
                                                c('a', 'b')]
  regts2['2010Q3/',  c('a', 'a_3')] <- 2 * regts1['2011Q2/2011Q3',
                                                  c('a', 'a')]
  expect_known_value(regts2, "expected_output/period_and_column.rds")
})

test_that("colnames are preserved in miscellaneous timeseries functions", {
  x <- regts(matrix(1:10, nc = 1), start = "2010Q4", names = "a")
  x_sin <- sin(x)
  x_lag <- lag(x)
  x_diff <- diff(x)
  x_agg <- aggregate(x)
  x_agg_gr <- aggregate_gr(x, method = "dif1s")
  x_windows <- window(x, start = c(2011, 4))
  expect_identical(colnames(x), colnames(x_lag))
  expect_identical(colnames(x), colnames(x_sin))
  expect_identical(colnames(x), colnames(x_diff))
  expect_identical(colnames(x), colnames(x_agg))
  expect_identical(colnames(x), colnames(x_agg_gr))
  expect_identical(colnames(x), colnames(x_windows))
})

test_that("several tests for character timeseries", {
  data <- matrix(paste0("text", as.character(1: 10)), ncol = 2)
  regts1 <- regts(data, start = "2010Q4", names = c("a", "b"))
  ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b"))
  expect_identical(regts1, as.regts(ts1))
  expect_identical(as.ts(regts1), ts1)
  expect_identical(regts1["2011Q2/2011Q3"],
                   as.regts(window(ts1, start = c(2011, 2), end = c(2011, 3))))

  regts2 <- regts1
  regts2["2010Q4"] <- "aap"
  ts2 <- ts1;
  window(ts2, start = c(2010, 4), end = c(2010, 4)) <- "aap";
  expect_identical(regts2, as.regts(ts2))
})

test_that("column selection in a timeseries with 1 row", {
  # selecting more than one column in a timeseries with 1 row has an odd
  # result for class ts if drop = TRUE. This behaviour is corrected for
  # in regts (see the implementation of "[.regts")

  regts1 <- regts(matrix(1:3, nc = 3), start = "2010Q2", names = c("a", "b", "c"))
  ts1 <- as.ts(regts1)

  # select two columns
  ref <- regts(matrix(2:3, nc = 2), start = "2010Q2", names = c("b", "c"))
  expect_identical(regts1[, c("b", "c")], ref)
  expect_identical(regts1[, c("b", "c")],
                   as.regts(ts1[, c("b", "c"), drop = FALSE]))

  # select one column (regts1[, "c"] is a named vector)
  ref <- regts(3, "2010Q2")
  names(ref) <- "c"
  expect_equal(regts1[, "c"], ref)
  expect_identical(regts1[, "c"], as.regts(ts1[, "c"]))

  # select all columns
  expect_identical(regts1[, ], regts1)
  expect_identical(regts1[, ], as.regts(ts1[, , drop = FALSE]))
})

test_that("colnames for regts that is not a matrix", {
  regts1 <- regts(1, start = "2010Q2", end = "2010Q3")
  expect_null(colnames(regts1))
  expect_identical(regts1, regts(rep(1, 2), start = "2010Q2"))

  expect_warning(regts1 <- regts(1, start = "2010Q2", end = "2010Q3",
                                 names = "var a"),
                 "Argument names is ignored if data is a vector")
  expect_null(colnames(regts1))
})

test_that("multivariate ts without colnames", {
  regts1 <- regts(matrix(1:3, nc = 3), "2010Q2")
  regts2 <- regts(matrix(1:3, nc = 3), "2010Q2", names = NULL)
  expect_null(colnames(regts1))
  expect_null(colnames(regts2))
  expect_identical(regts1, regts2)
})

test_that("start and end/prd_range, univariate", {
  expect_identical(regts(1:10), regts(1:10, start = "1", end = "10"))
  expect_identical(regts(1:10), regts(1:10, start = 1, end = 10))
  expect_identical(regts(1:10), regts(1:10, start = 1))
  expect_identical(regts(1:10), regts(1:10, end = 10))
  expect_identical(regts(1:10), regts(1:10, period = period_range("1","10")))
  expect_identical(regts(rep(1, 10)), regts(1, start = 1, end = 10))
  expect_identical(regts(rep(1:2, 5)), regts(1:2, start = 1, end = 10))
  expect_identical(regts(rep(1:2, 5)), regts(1:2, period = period_range("1","10")))
})


test_that("start and end/prd_range, multivariate", {
  data <- matrix(1:10, ncol = 2)
  expect_identical(regts(data), regts(data, start = "1", end = "5"))
  expect_identical(regts(data[1:2, ]), regts(data, start = 1, end = 2))
  expect_identical(regts(data[1:2, ]), regts(data, period = period_range("1","2")))
  expect_identical(regts(data[1, , drop = FALSE]),
                   regts(data, start = 1, end = 1))
  expect_identical(regts(data[1, , drop = FALSE], start = 1, end = 2),
                   regts(rbind(data[1, , drop=FALSE], data[1, , drop = FALSE]),
                         period = period_range("1","2")))

})

test_that("start and end/prd_range, multivariate (1 column)", {
  data <- matrix(1:5, ncol = 1)
  expect_identical(regts(data), regts(data, start = "1", end = "5"))
  expect_identical(regts(data), regts(data, period = period_range("1","5")))
  expect_identical(regts(data[1:2, , drop = FALSE]), regts(data, start = 1, end = 2))
  expect_identical(regts(data[1, , drop = FALSE]), regts(data, start = 1, end = 1))
  expect_identical(regts(data[1, , drop = FALSE]), regts(data, period = period_range("1","1")))
  expect_identical(regts(data[1, , drop = FALSE], start = 1, end = 2),
                   regts(rbind(data[1, , drop=FALSE], data[1, , drop = FALSE]),
                         start = 1, end = 2))

})

test_that("period selection exotic timeseries", {

  num_ts <- regts(matrix(c(1, 2), ncol = 1), period = "2014q1/2014q2",
                  names = "a")
  txt_ts <- regts(matrix(c("x", "y"), ncol = 1), period = "2014q1/2014q2",
                  names = "a")
  logi_ts <- regts(matrix(c(FALSE, TRUE), ncol = 1), period = "2014q1/2014q2",
                   names = "a")

  expect_identical(num_ts["2018Q1", 1], regts(NA_real_, period = "2018Q1"))
  expect_identical(logi_ts["2013Q4/2014Q1"][, 1], regts(c(NA, FALSE),
                                                   period = "2013q4/2014q1"))
  expect_identical(logi_ts["2018Q1/2018Q2"][, 1], regts(NA,
                                                        period = "2018q1/2018q2"))

  expect_identical(txt_ts["2018Q1/2018Q2"][, 1], regts(NA_character_,
                                                        period = "2018q1/2018q2"))

  expect_identical(txt_ts["2014Q2/2014Q3"][, 1], regts(c("y", NA),
                                                       period = "2014q2/2014q3"))

  expect_identical(txt_ts["2018Q1/2018Q2", 1], regts(NA_character_,
                                                       period = "2018q1/2018q2"))

  # factor timeseries (behaves a little bit strangely)
  factors <- as.factor(c("x", "y"))
  factor_regts <- regts(factors, period = "2014q1/2014q2")
  factor_ts <- ts(factors, start = 2014, frequency = 4)
  expect_identical(as.ts(factor_ts), factor_ts)
  expect_identical(as.ts(factor_regts["2018Q1/2018Q2"]),
                   window(factor_ts, start = c(2018,1), end = c(2018, 2),
                          extend = TRUE))
})


test_that("row selection with NA", {

  regts1a <- regts(matrix(1:10, ncol = 1), start = "2010Q4", names = "a")
  regts1b <- regts1a[, 1]

  ts1a <- as.ts(regts1a)
  ts1b <- as.ts(regts1b)

  expect_identical(regts1a[NA, ], ts1a[NA, ])
  expect_identical(regts1b[NA], ts1b[NA])

  regts2a <- regts1a
  regts2a[NA] <- 2L
  expect_identical(regts2a, regts1a)

  regts2b <- regts1b
  regts2b[NA] <- 2L
  expect_identical(regts2b, regts1b)

  msg <- "Illegal period NA."
  expect_error(regts1a[NA_character_, ], msg)
  expect_error(regts1b[NA_character_], msg)

  expect_error(regts1a[NA_character_, ] <- 999, msg)
  expect_error(regts1b[NA_character_] <- 999, msg)
})

test_that("column selection with NA", {
  regts1 <- regts(matrix(1:10, ncol = 1), start = "2010Q4", names = "a")
  ts1 <- as.ts(regts1)
  expect_identical(regts1[, NA], as.regts(ts1[, NA]))
  expect_error(regts1[, NA_character_] <- 9, "subscript out of bounds")

  regts2 <- regts1
  regts2[, NA] <- 2L
  expect_identical(regts1, regts2)

  regts3 <- regts1
  regts3[, c(NA, 1)] <- 3
  expected_result <- regts1
  expected_result[] <- 3
  expect_identical(regts3, expected_result)
})

test_that("undefined columns selected", {

  regts1 <- regts(matrix(1:10, ncol = 1), start = "2010Q4", names = "a")

  expect_error(regts1[ , NA_character_], "Undefined columns: NA.")
  expect_error(regts1[ , c("a", "z", "p", "g")], "Undefined columns: z, p, g.")

  tryCatch({
    regts1[, "xxx"]
  }, error = function(err) {
    err1 <<- err
  })
  expect_identical(err1$message, "Undefined columns: xxx.")
  expect_identical(as.character(err1$call), c("[.regts", "regts1", "", "xxx"))

  tryCatch({
    regts1[, 50]
  }, error = function(err) {
    err2 <<- err
  })
  expect_identical(err2$message, "subscript out of bounds")
  expect_identical(as.character(err2$call), c("[.default", "regts1", "", "50"))
})

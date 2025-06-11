library(regts)
library(testthat)

rm(list = ls())

a_data <- as.numeric(1:3)
b_data <- 2 * a_data
a_ts <- regts(a_data, start = "2018Q1")
b_ts <- regts(b_data, start = "2018Q1")
multi_ts <- cbind(a_ts, b_ts)
multi_data <- cbind(a_data, b_data)
colnames(multi_data) <- NULL
multi_data_rowwise <- t(multi_data)
periods <- as.character(get_periods(a_ts))

a_label <- "Var a"
b_label <- "Var b"

multi_labels <- c(a_label, b_label)
multi_names <- c("a_ts", "b_ts")

test_that("univariate timeseries without labels", {

  # columnwise ----
  expected <- data.frame(period = periods, a_ts = a_data)
  expect_identical(as_data_frame(a_ts), expected)

  # columnwise with argument period_col ----
  expected <- data.frame(quarter = periods, a_ts = a_data)
  expect_identical(as_data_frame(a_ts, format = "columnwise",
                                 period_col = "quarter"), expected)


  # rowwise ----
  expected <- data.frame("a_ts", t(a_data)) |>
    setNames(c("name", periods))
  expect_identical(as_data_frame(a_ts, format = "rowwise"),
                   expected)

  # long format ----
  a_df_long <- as_data_frame(a_ts, format = "long")
  expected <- data.frame(name = "a_ts", period = periods, value = a_data)
  expect_equal(a_df_long, expected)
})

test_that("univariate timeseries with labels", {

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- a_label

  # Columnwise ----
  expected <- data.frame(period = periods, a_ts_l = a_data)
  attr(expected[[2]], "label") <- a_label

  expect_identical(as_data_frame(a_ts_l), expected)

  # Rowwise ----
  expected <- data.frame("a_ts_l", a_label, t(a_data)) |>
    setNames(c("name", "label", periods))
  expect_identical(as_data_frame(a_ts_l, format = "rowwise"),
                   expected)

  # Long format
  expected <- data.frame(name = "a_ts_l", label = a_label,
                         period = periods, value = a_data)

  expect_equal(
    as_data_frame(a_ts_l, format = "long"),
    expected
  )
})

test_that("multivariate timeseries with labels", {

  multi_ts_l <- multi_ts
  ts_labels(multi_ts_l) <- c(a_label, b_label)

  # columnwise ----
  expected <- data.frame(period = periods, a_ts = a_data,
                         b_ts = b_data)
  attr(expected[[2]], "label") <- a_label
  attr(expected[[3]], "label") <- b_label
  expect_identical(as_data_frame(multi_ts_l), expected)

  # rowwise ----
  expected <- cbind(
    data.frame(multi_names, multi_labels),
    multi_data_rowwise
  ) |>
    setNames(c("name", "label", periods))

  expect_identical(as_data_frame(multi_ts_l, format = "rowwise"),
                   expected)

  # Long format ----

  multi_df_long <- as_data_frame(multi_ts_l, format = "long")
  expected <- data.frame(name = rep(multi_names, each = 3),
                         label = rep(multi_labels, each = 3),
                         period = rep(periods, 2),
                         value = c(a_data, b_data))
  expect_equal(multi_df_long, expected)
})

test_that("period_as_date", {

  date_periods <- c(as.Date("2018-01-01"), as.Date("2018-04-01"),
                    as.Date("2018-07-01"))

  # columnwise ----

  a_ts_df <- as_data_frame(a_ts, period_as_date = TRUE)
  expected <- data.frame(period = date_periods, a_ts = a_data)
  expect_identical(a_ts_df, expected)
  expect_identical(
    as.regts(a_ts_df, time_column = "period", frequency = 4)[, 1],
    a_ts
  )

  # rowwise ----
  expected <- data.frame("a_ts", t(a_data)) |>
    setNames(c("name", format(date_periods)))
  expect_equal(
    as_data_frame(a_ts, format = "rowwise", period_as_date = TRUE),
    expected
  )


  # Long format ----

  expected <- data.frame(name = "a_ts", period = date_periods, value = a_data)
  expect_equal(
    as_data_frame(a_ts, format = "long", period_as_date = TRUE),
    expected
  )

})

test_that("single period", {

  a <- a_ts["2018Q1"]

  # univariate columnwise ----
  expected <- data.frame(period = periods[1], a = a_data[1])
  expect_identical(as_data_frame(a), expected)

  # multivariate columnwise
  ab_ts  <- cbind(a, b = 2 * a)
  ts_labels(ab_ts) <- multi_labels

  expected <- data.frame(period = "2018Q1", a = 1, b = 2,
                         stringsAsFactors = FALSE)
  attr(expected[[2]], "label") <- a_label
  attr(expected[[3]], "label") <- b_label

  expect_identical(
    as_data_frame(ab_ts),
    expected
  )

  # Long format ----

  expect_equal(as_data_frame(a, format = "long"),
               data.frame(name = "a",
                          period = "2018Q1",
                          value = 1))

  expected <-  data.frame(name = c("a", "b"),
                          label = multi_labels,
                          period = "2018Q1",
                          value = 1:2)
  expect_equal(
    as_data_frame(ab_ts, format = "long"),
    expected
  )

})

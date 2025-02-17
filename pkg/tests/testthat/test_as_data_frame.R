library(regts)
library(testthat)

rm(list = ls())


a_ts <- regts(1:3, start = "2018Q1")
a_df <- data.frame(a_ts = 1:3, row.names = c("2018Q1", "2018Q2", "2018Q3"))

remove_row_names <- function(df, rowwise) {
  rnames <- rownames(df)
  rownames(df) <- NULL
  if (rowwise) {
    return(cbind(name = rnames, df, stringsAsFactors = FALSE))
  } else {
    return(cbind(period = rnames, df, stringsAsFactors = FALSE))
  }
}


test_that("univariate timeseries without labels", {

  a_df_2 <-  remove_row_names(a_df, FALSE)
  a_df_rowwise <- as.data.frame(t(a_df))
  a_df_rowwise_2 <-  remove_row_names(a_df_rowwise, TRUE)

  expect_identical(as.data.frame(a_ts), a_df)

  expect_identical(as.data.frame(a_ts, row_names = TRUE, rowwise = FALSE), a_df)


  expect_identical(as.data.frame(a_ts, row_names = FALSE), a_df_2)

  expect_identical(as.data.frame(a_ts, rowwise = TRUE), a_df_rowwise)

  expect_identical(as.data.frame(a_ts, rowwise = TRUE, row_names = FALSE),
                   a_df_rowwise_2)

  # Now long format

  expect_warning(
    a_df_long <- as.data.frame(a_ts, rowwise = FALSE, long = TRUE),
    "Argument 'rowwise' is ignored if long is TRUE",
    fixed = TRUE
  )

  expect_equal(
    as.data.frame(tidyr::pivot_wider(a_df_long, names_from = "name")),
    a_df_2
  )
})

test_that("univariate timeseries with labels", {

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- "Var a"

  a_df_l <- a_df
  colnames(a_df_l) <- "a_ts_l"
  a_df_l <- regts:::set_labels_df(a_df_l, "Var a")

  a_df_l_2 <- remove_row_names(a_df_l, FALSE)
  a_df_l_rowwise <- transpose_df(a_df_l)
  a_df_l_rowwise_2 <-  remove_row_names(a_df_l_rowwise, TRUE)

  expect_identical(Hmisc::label(a_df_l), c(a_ts_l = "Var a"))

  expect_identical(as.data.frame(a_ts_l), a_df_l)
  expect_identical(as.data.frame(a_ts_l, row_names = FALSE), a_df_l_2)

  expect_identical(as.data.frame(a_ts_l, rowwise = TRUE),
                   a_df_l_rowwise)

  expect_identical(as.data.frame(a_ts_l, rowwise = TRUE, row_names = FALSE),
                   a_df_l_rowwise_2)

  # Now long format

  expect_warning(
    a_df_l_long <- as.data.frame(a_ts_l, row_names = TRUE, long = TRUE),
    "Argument 'row_names' is ignored if long is TRUE",
    fixed = TRUE
  )

  expect_equal(
    as.data.frame(tidyr::pivot_wider(a_df_l_long, names_from = "period")),
    a_df_l_rowwise_2
  )
})

test_that("multivariate timeseries with labels", {

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- "Var a"

  b_ts_l <- 2 * a_ts
  ts_labels(b_ts_l) <- "Var b"

  multi_ts <- cbind(a_ts_l, b_ts_l)

  a_df_l <- a_df
  a_df_l[, 1] <- as.numeric(a_df_l[, 1])
  colnames(a_df_l) <- "a_ts_l"
  a_df_l <- regts:::set_labels_df(a_df_l, "Var a")

  b_df_l <- 2 * a_df
  colnames(b_df_l) <- "b_ts_l"
  b_df_l <- regts:::set_labels_df(b_df_l, "Var b")

  multi_df <- cbind(a_df_l, b_df_l)

  expect_identical(Hmisc::label(a_df_l), c(a_ts_l = "Var a"))
  expect_identical(Hmisc::label(b_df_l), c(b_ts_l = "Var b"))

  multi_df_2 <- remove_row_names(multi_df, FALSE)

  expect_identical(as.data.frame(multi_ts), multi_df)
  expect_identical(as.data.frame(multi_ts, row_names = FALSE),
                   multi_df_2)

  multi_df_rowwise <-  transpose_df(multi_df)
  multi_df_rowwise_2 <- remove_row_names(multi_df_rowwise, TRUE)

  expect_identical(as.data.frame(multi_ts, rowwise = TRUE), multi_df_rowwise)
  expect_identical(as.data.frame(multi_ts, rowwise = TRUE, row_names = FALSE),
                   multi_df_rowwise_2)

  # Long format
  multi_df_long <- as.data.frame(multi_ts, long = TRUE)
  expect_equal(
    as.data.frame(tidyr::pivot_wider(multi_df_long, names_from = "period")),
    multi_df_rowwise_2
  )
})

test_that("period_as_date", {

  date_periods <- c(as.Date("2018-01-01"), as.Date("2018-04-01"),
                    as.Date("2018-07-01"))

  a_ts_df <- as.data.frame(a_ts, row_names = FALSE, period_as_date = TRUE)

  expected_result <- data.frame(period = date_periods, a_ts = as.integer(a_ts))
  expect_identical(a_ts_df, expected_result)

  expect_identical(
    as.regts(a_ts_df, time_column = "period", frequency = 4)[, 1],
    a_ts
  )

  a_ts_df2 <- as.data.frame(a_ts, row_names = TRUE, period_as_date = TRUE)

  expected_result <- data.frame(a_ts = as.integer(1:3))
  rownames(expected_result) <- date_periods
  expect_identical(a_ts_df2, expected_result)

  conv_fun <- function(x) {
    return(period(as.Date(x), frequency = 4))
  }
  expect_identical(as.regts(a_ts_df2, fun = conv_fun)[, 1],
                   a_ts)

  # Long format
  a_ts_df_l <- as.data.frame(a_ts, long = TRUE, period_as_date = TRUE)
  expect_equal(a_ts_df_l,
               data.frame(name = "a_ts",
                          period = a_ts_df$period,
                          value = 1:3))
})

test_that("single period", {

  a <- a_ts["2018Q1"]
  expected_result <- a_df[1, , drop = FALSE]
  colnames(expected_result) <- "a"
  expect_identical(as.data.frame(a), expected_result)

  ab_ts  <- cbind(a, b = 2 * a)
  ts_labels(ab_ts) <- c("Var a", "Var b")

  expected_result <- data.frame(period = "2018Q1", a = 1, b = 2,
                                stringsAsFactors = FALSE)
  attr(expected_result[[2]], "label") <- "Var a"
  attr(expected_result[[3]], "label") <- "Var b"

  ab_df <- as.data.frame(ab_ts, row_names = FALSE)
  expect_identical(ab_df, expected_result)

  # Long format

  expect_equal(as.data.frame(a, long = TRUE),
               data.frame(name = "a",
                          period = "2018Q1",
                          value = 1))

  ab_ts_df_l <- as.data.frame(ab_ts, long = TRUE)
  expect_equal(ab_ts_df_l,
               data.frame(name = c("a", "b"),
                          label = c("Var a", "Var b"),
                          period = "2018Q1",
                          value = 1:2))
})

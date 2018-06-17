library(regts)
library(testthat)

rm(list = ls())

context("as.data.frame.regts")

a_ts <- regts(1:3, start = "2018Q1")
a_df <- data.frame(a_ts = 1:3, row.names = c("2018Q1", "2018Q2", "2018Q3"))

print(as.data.frame(a_ts, format = "%d-%m-%Y"))

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

  expect_identical(as.data.frame(a_ts), a_df)
  expect_identical(as.data.frame(a_ts, row_names = TRUE, rowwise = FALSE), a_df)
  expect_identical(as.data.frame(a_ts, row_names = FALSE),
                   remove_row_names(a_df, FALSE))
  expect_identical(as.data.frame(a_ts, rowwise = TRUE),
                   as.data.frame(t(a_df)))
  expect_identical(as.data.frame(a_ts, rowwise = TRUE, row_names = FALSE),
                   remove_row_names(as.data.frame(t(a_df)), TRUE))
})

test_that("univariate timeseries with labels", {

  a_ts_l <- a_ts
  ts_labels(a_ts_l) <- "Var a"

  a_df_l <- a_df
  colnames(a_df_l) <- "a_ts_l"
  a_df_l <- regts:::set_labels_df(a_df_l, "Var a")

  expect_identical(Hmisc::label(a_df_l), c(a_ts_l = "Var a"))

  expect_identical(as.data.frame(a_ts_l), a_df_l)
  expect_identical(as.data.frame(a_ts_l, row_names = FALSE),
                   remove_row_names(a_df_l, FALSE))

  expect_identical(as.data.frame(a_ts_l, rowwise = TRUE),
                   transpose_df(a_df_l))
  expect_identical(as.data.frame(a_ts_l, rowwise = TRUE, row_names = FALSE),
                   remove_row_names(transpose_df(a_df_l), TRUE))
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

  expect_identical(as.data.frame(multi_ts), multi_df)
  expect_identical(as.data.frame(multi_ts, row_names = FALSE),
                   remove_row_names(multi_df, FALSE))

  expect_identical(as.data.frame(multi_ts, rowwise = TRUE),
                   transpose_df(multi_df))
  expect_identical(as.data.frame(multi_ts, rowwise = TRUE, row_names = FALSE),
                   remove_row_names(transpose_df(multi_df), TRUE))
})

test_that("date_format argument", {

  a_ts_df <- as.data.frame(a_ts,  period_format = "%d-%m-%Y")
  print(a_ts_df)
  a_ts_df_ts <- as.regts(a_ts_df, fun = function(x)
    {period(as.Date(x, format = "%d-%m-%Y"), frequency = 4)})
  print(a_ts_df_ts)
  # TODO: example in as.regts.data.frame met dit voorbeeld
})

library(regts)
library(testthat)


rm(list = ls())

test_that("select_columns, univariate timseries", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    stringsAsFactors = FALSE
  )
  expect_identical(select_columns(df, "a"), df[, "a"])
  ts1 <- regts(df[, 2, drop = FALSE], start = "2015Q3")
  expect_identical(select_columns(df, "a"), df[, "a"])
  expect_identical(select_columns(df, "x"), df[, character(0)])
  expect_identical(select_columns(ts1, "a"), ts1[, 1])
  expect_identical(select_columns(ts1, "x"), ts1[, character(0)])

  expect_identical(
    select_columns(df, "a", drop = FALSE),
    df[, "a", drop = FALSE]
  )
  expect_identical(
    select_columns(df, "x", drop = FALSE),
    df[, character(0), drop = FALSE]
  )
  expect_identical(
    select_columns(df, "x", drop = TRUE),
    df[, character(0), drop = FALSE]
  )
  expect_identical(
    select_columns(ts1, "a", drop = FALSE),
    ts1[, 1, drop = FALSE]
  )
  expect_identical(
    select_columns(ts1, "x", drop = FALSE),
    ts1[, character(0), drop = FALSE]
  )

  # no colnames
  ts2 <- regts(df[, 2], start = "2015Q3")
  expect_error(
    select_columns(ts2, "x"),
    "No column names available. No selection possible"
  )
})

test_that("select_columns, multivariate timseries", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    b = 10:12,
    stringsAsFactors = FALSE
  )
  ts1 <- as.regts(df, time_column = 1)
  expect_identical(select_columns(df, "b.*"), df[, "b"])
  expect_identical(select_columns(df, "x.*"), df[, character(0)])
  expect_identical(select_columns(ts1, "b.*"), ts1[, "b"])
  expect_identical(select_columns(ts1, "x.*"), ts1[, character(0)])

  expect_identical(
    select_columns(df, "b.*", drop = FALSE),
    df[, "b", drop = FALSE]
  )
  expect_identical(
    select_columns(df, "x.*", drop = FALSE),
    df[, character(0), drop = FALSE]
  )
  expect_identical(
    select_columns(ts1, "b.*", drop = FALSE),
    ts1[, "b", drop = FALSE]
  )
  expect_identical(
    select_columns(ts1, "x.*", drop = FALSE),
    ts1[, character(0), drop = FALSE]
  )
  expect_identical(
    select_columns(ts1, "x.*", drop = TRUE),
    ts1[, character(0), drop = FALSE]
  )

  # test arguments of function grep
  expect_identical(
    select_columns(df, "B.*", drop = FALSE, ignore.case = TRUE),
    df[, "b", drop = FALSE]
  )

  result <- select_columns(df, "B.*", drop = FALSE, ignore.case = FALSE)
  expected_result <- as.data.frame(matrix(ncol = 0, nrow = 3))
  names(expected_result) <- character(0)
  expect_identical(result, expected_result)

  expect_error(
    select_columns(ts1[, character(0), drop = FALSE], regex = "."),
    "No column names available. No selection possible"
  )
})

test_that("select_columns, multivariate timseries with labels", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    b = 10:12,
    stringsAsFactors = FALSE
  )
  ts1 <- as.regts(df, time_column = 1)
  ts_labels(ts1) <- paste("Var", c("a", "b"))

  ts1_a <- select_columns(ts1, "a", drop = FALSE)
  expect_identical(ts1_a, ts1[, "a", drop = FALSE])
  expect_equal(ts_labels(ts1_a), c(a = "Var a"))

  ts1_a <- select_columns(ts1, "a", drop = TRUE)
  expect_identical(ts1_a, ts1$a)
  expect_equal(ts_labels(ts1_a), "Var a")
})

test_that("drop_columns, multivariate timeseries", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    b = 10:12,
    stringsAsFactors = FALSE
  )
  ts1 <- as.regts(df, time_column = 1)

  # drop matching columns
  expect_identical(drop_columns(df, "^a"), df[, c("period", "b"), drop = FALSE])
  expect_identical(drop_columns(ts1, "^a"), ts1[, "b", drop = FALSE])

  # drop with no match: all columns retained
  expect_identical(
    drop_columns(df, "^x"),
    df[, c("period", "a", "b"), drop = FALSE]
  )
  expect_identical(drop_columns(ts1, "^x"), ts1[, c("a", "b"), drop = FALSE])

  # drop all matching columns
  expect_identical(drop_columns(ts1, ".*"), ts1[, character(0), drop = FALSE])

  # pass extra argument to grep (ignore.case)
  expect_identical(
    drop_columns(ts1, "^A", ignore.case = TRUE),
    ts1[, "b", drop = FALSE]
  )

  # no column names
  ts2 <- regts(df[, 2], start = "2015Q3")
  expect_error(
    drop_columns(ts2, "a"),
    "No column names available. No selection possible"
  )
})

test_that("select_cols_by_name, basic selection", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    b = 10:12,
    stringsAsFactors = FALSE
  )
  ts1 <- as.regts(df, time_column = 1)

  # single column
  expect_identical(select_cols_by_name(df, "a"), df[, "a", drop = FALSE])
  expect_identical(select_cols_by_name(ts1, "a"), ts1[, "a", drop = FALSE])

  # multiple columns: result follows column order in x, not names argument order
  expect_identical(
    select_cols_by_name(ts1, c("b", "a")),
    ts1[, c("a", "b"), drop = FALSE]
  )

  # empty selection
  expect_identical(
    select_cols_by_name(ts1, character(0)),
    ts1[, character(0), drop = FALSE]
  )

  # strict = TRUE: error on unknown name
  expect_error(
    select_cols_by_name(ts1, c("a", "z"), strict = TRUE),
    "The following names are not column names of x: z"
  )

  # strict = FALSE: unknown names silently ignored
  expect_identical(
    select_cols_by_name(ts1, c("a", "z"), strict = FALSE),
    ts1[, "a", drop = FALSE]
  )

  # no column names
  ts2 <- regts(df[, 2], start = "2015Q3")
  expect_error(
    select_cols_by_name(ts2, "a"),
    "No column names available. No selection possible"
  )
})

test_that("drop_cols_by_name, basic dropping", {
  df <- data.frame(
    period = c("2015Q3", "2015Q4", "2016Q1"),
    a = 1:3,
    b = 10:12,
    stringsAsFactors = FALSE
  )
  ts1 <- as.regts(df, time_column = 1)

  # drop one column
  expect_identical(drop_cols_by_name(ts1, "a"), ts1[, "b", drop = FALSE])

  # drop multiple columns
  expect_identical(
    drop_cols_by_name(ts1, c("a", "b")),
    ts1[, character(0), drop = FALSE]
  )

  # drop nothing
  expect_identical(drop_cols_by_name(ts1, character(0)), ts1)

  # strict = TRUE: error on unknown name
  expect_error(
    drop_cols_by_name(ts1, c("a", "z"), strict = TRUE),
    "The following names are not column names of x: z"
  )

  # strict = FALSE: unknown names silently ignored (only known ones dropped)
  expect_identical(
    drop_cols_by_name(ts1, c("a", "z"), strict = FALSE),
    ts1[, "b", drop = FALSE]
  )

  # no column names
  ts2 <- regts(df[, 2], start = "2015Q3")
  expect_error(
    drop_cols_by_name(ts2, "a"),
    "No column names available. No selection possible"
  )
})

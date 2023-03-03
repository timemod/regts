library(regts)
library(testthat)


rm(list = ls())

test_that("select_columns, univariate timseries", {
  df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3,
                   stringsAsFactors = FALSE)
  expect_identical(select_columns(df, 'a'), df[, 'a'])
  ts1 <- regts(df[, 2, drop = FALSE] , start = "2015Q3")
  expect_identical(select_columns(df, 'a'), df[, 'a'])
  expect_identical(select_columns(df, 'x'), df[, character(0)])
  expect_identical(select_columns(ts1, 'a'), ts1[, 1])
  expect_identical(select_columns(ts1, 'x'), ts1[, character(0)])

  expect_identical(select_columns(df, 'a', drop = FALSE),
                   df[, 'a', drop = FALSE])
  expect_identical(select_columns(df, 'x', drop = FALSE),
                   df[, character(0), drop = FALSE])
  expect_identical(select_columns(df, 'x', drop = TRUE),
                   df[, character(0), drop = FALSE])
  expect_identical(select_columns(ts1, 'a', drop = FALSE),
                   ts1[, 1, drop = FALSE])
  expect_identical(select_columns(ts1, 'x', drop = FALSE),
                   ts1[, character(0), drop = FALSE])

  # no colnames
  ts2 <- regts(df[, 2] , start = "2015Q3")
  expect_error(select_columns(ts2, 'x'),
               "No column names available. No selection possible")
})

test_that("select_columns, multivariate timseries", {
  df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3,
                   b = 10:12, stringsAsFactors = FALSE)
  ts1 <- as.regts(df, time_column = 1)
  expect_identical(select_columns(df, 'b.*'), df[, 'b'])
  expect_identical(select_columns(df, 'x.*'), df[, character(0)])
  expect_identical(select_columns(ts1, 'b.*'), ts1[, 'b'])
  expect_identical(select_columns(ts1, 'x.*'), ts1[, character(0)])

  expect_identical(select_columns(df, 'b.*', drop = FALSE), df[, 'b',
                                                               drop = FALSE])
  expect_identical(select_columns(df, 'x.*', drop = FALSE),
                   df[, character(0), drop = FALSE])
  expect_identical(select_columns(ts1, 'b.*', drop = FALSE),
                   ts1[, 'b', drop = FALSE])
  expect_identical(select_columns(ts1, 'x.*', drop = FALSE),
                   ts1[, character(0), drop = FALSE])
  expect_identical(select_columns(ts1, 'x.*', drop = TRUE),
                   ts1[, character(0), drop = FALSE])

  # test arguments of function grep
  expect_identical(select_columns(df, 'B.*', drop = FALSE, ignore.case = TRUE),
                   df[, 'b', drop = FALSE])

  result <- select_columns(df, 'B.*', drop = FALSE, ignore.case = FALSE)
  expected_result <-  as.data.frame(matrix(ncol = 0, nrow = 3))
  names(expected_result) <- character(0)
  expect_identical(result, expected_result)
})



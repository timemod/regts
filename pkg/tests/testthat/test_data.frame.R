library(regts)
library(testthat)

rm(list = ls())


test_that("as.regts.data.frame for univariate quarterly timeseries", {
  df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3,
                   stringsAsFactors = FALSE)
  ts1 <- as.regts(df, time_column = 1)
  ts2 <- regts(matrix(1:3, ncol = 1) , start = "2015Q3", names = "a")
  expect_identical(ts1, ts2)
  df2 <- df[-1]
  rownames(df2) <- df[[1]]
  expect_identical(df2, as.data.frame(ts2))

  a <- ts2[, 1, drop=TRUE]
  expect_identical(df2, as.data.frame(a))
})

test_that("as.regts.data.frame for multivariate quarterly timeseries", {
  df <- data.frame(a = 1:3, b = 4:6)
  rownames(df) <- c("2015 3", "2015 4", "2016 1")
  ts1 <- as.regts(df, frequency = 4)
  ts2 <- regts(matrix(1:6, ncol =  2), start = "2015Q3", names = c("a", "b"))
  expect_identical(ts1, ts2)
  df2 <- df
  rownames(df) <- c("2015Q3", "2015Q4", "2016Q1")
  expect_identical(df, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate yearly timeseries", {
  df <- data.frame(periods = c(2015, 2016, 2017), a = 1:3, b = 4:6)
  ts1 <- as.regts(df, time_column = 1)
  ts2 <- regts(matrix(1:6, ncol =  2), start = "2015", names = c("a", "b"))
  expect_identical(ts1, ts2)
  df2 <- df
  rownames(df2) <- as.character(df[[1]])
  df2 <- df2[-1]
  expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate yearly timeseries with labels", {
  df <- data.frame(periods = c(2015, 2016, 2017), a = 1:3, b = 4:6)
  ts_labels <- paste("Timeseries", c("a", "b"))
  df <- regts:::set_labels_df(df, c("", ts_labels))

  expected_labels_Hmisc <- c("", ts_labels)
  names(expected_labels_Hmisc) <- colnames(df)
  expect_identical(Hmisc::label(df), expected_labels_Hmisc)

  ts1 <- as.regts(df, time_column = 1)
  ts2 <- regts(matrix(1:6, ncol =  2), start = "2015", names = c("a", "b"),
               labels = ts_labels)
  expect_identical(ts1, ts2)
  df2 <- df
  rownames(df2) <- as.character(df[[1]])
  df2 <- df2[-1]
  expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate quarterly irregular timeseries", {
  df <- data.frame(a = 1:3, b = 4:6)
  rownames(df) <- c("2016Q2", "2015Q3", "2015Q4")
  ts1 <- as.regts(df, strict = FALSE)
  # convert ts1 from integer to double
  ts1[, ] <- as.numeric(ts1)
  ts2 <- regts(matrix(c(2,3,NA,1,5,6,NA,4), ncol =  2), start = "2015Q3",
               names = c("a", "b"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame for argument numeric", {
  df <- data.frame(periods = c(2015, 2016, 2017),
                   awn = c("1","2","3"), bwn = c("4","5","6"), stringsAsFactors = FALSE )
  ts1 <- as.regts(df, time_column = "periods")
  # use as.numeric to create non integer values, ts1 has also non integer values
  ts2 <- regts(matrix(as.numeric(1:6), ncol = 2), start = "2015", names = c("awn","bwn"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame for argument numeric, a one row data frame", {
  df <- data.frame(awn = "1.1", bwn = "2.2", stringsAsFactors = FALSE )
  rownames(df) <- 2015
  ts1 <- as.regts(df)
  ts2 <- regts(matrix(c(1.1,2.2), ncol = 2), start = "2015", names = c("awn","bwn"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame for argument numeric = FALSE", {
  df <- data.frame(periods = c(2015, 2016, 2017),
                   awn = c("1","2","3"), bwn = c("4","5","6"), stringsAsFactors = FALSE )
  ts1 <- as.regts(df, time_column = "periods", numeric = FALSE)
  # use as.numeric to create non integer values, ts1 has also non integer values
  ts2 <- regts(matrix(c("1","2","3","4","5","6"), ncol = 2), start = "2015", names = c("awn","bwn"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame with invalid period texts", {
  df <- data.frame(a = c("1", "x", "3"), b = c("2", " ", ""), c = 11:13,
                   stringsAsFactors = FALSE)
  rownames(df) <- 2015:2017

  mat <- suppressWarnings(matrix(as.numeric(unlist(df)), nrow = 3))
  colnames(mat) <- colnames(df)
  msg <- "NAs introduced by coercion.\nThe following texts could not be converted to numeric:\n\"x\""
  expect_warning(ts1 <- as.regts(df), regexp = msg)
  # use as.numeric to create non integer values, ts1 has also non integer values
  ts2 <- regts(mat, start = "2015")
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame with invalid period texts and with factors", {

  df <- data.frame(a = c("1", "x", "3"), b = c("2", " ", ""), c = 11:13,
                   stringsAsFactors = FALSE)

  mat <- suppressWarnings(matrix(as.numeric(unlist(df)), nrow = 3))
  colnames(mat) <- colnames(df)

  df_fac <- data.frame(a = c("1", "x", "3"), b = c("2", " ", ""), c = 11:13,
                       stringsAsFactors = TRUE)
  rownames(df_fac) <- 2015:2017

  msg <- "NAs introduced by coercion.\nThe following texts could not be converted to numeric:\n\"x\""
  expect_warning(ts1 <- as.regts(df_fac), regexp = msg)
  # use as.numeric to create non integer values, ts1 has also non integer values
  ts2 <- regts(mat, start = "2015")
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame for a tibble", {
  df <- data.frame(period =  c("2015q3", "2015q4", "2016q1"), a = 1:3,
                   b = 4:6)
  df <- tibble::as_tibble(df)
  # we don't want error messages about "Setting row names on a tibble is deprecated"
  expect_silent(ts1 <- as.regts(df, frequency = 4, time_column = 1))
  ts2 <- regts(matrix(1:6, ncol =  2), start = "2015Q3", names = c("a", "b"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame for a data.frame with dates", {

  periods <- c(as.Date("2015-05-30"), as.Date("2015-06-30"),
               as.Date("2015-07-30"))

  df <- data.frame(period = periods, a = 1:3)

  ts1 <- as.regts(df, time_column = "period")

  ts2 <- regts(matrix(1:3, ncol = 1) , start = "2015M05", names = "a")
  expect_identical(ts1, ts2)

  expect_error(as.regts(df, time_column = "period", frequency = 4),
               "Duplicate periods found in data \\(2015Q2\\).")
})


test_that("as.regts.data.frame for column names starting with a number", {
  df1 <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), `3` = 1:3,
                    stringsAsFactors = TRUE, check.names = FALSE)
  ts1 <- as.regts(df1, time_column = 1)
  expected_result <- regts(matrix(1:3, ncol = 1) , start = "2015Q3", names = "3")
  expect_identical(ts1, expected_result)
})


test_that("as.regts.data.frame for irregular timeseries with strict", {
  df <- data.frame(a = 1:3, b = 4:6)
  rownames(df) <- c("2016Q2", "2015Q3", "2015Q4")

  msg <- "Missing periods found \\(2016Q1\\). Set parameter strict to FALSE!"
  expect_error(as.regts(df), msg)
})

test_that("as.regts.data.frame with different frequencies", {
  df <- data.frame(a = 1:2)
  rownames(df) <- c("2016Q2", "2015m3")

  msg <- "The time column\\(s\\) contain different frequencies"
  expect_error(as.regts(df), msg)
})

test_that("as.regts.data.frame with argument fun", {
  df <- data.frame(a = 1:2, b = 3:4)
  rownames(df) <- c("3", "4")
  fun <- function(x, ...) {period(paste(2018, x), ...)}
  ts1 <- as.regts(df, frequency = 4, fun = fun)
  ts2 <- regts(matrix(1:4, ncol =  2), start = "2018Q3", names = c("a", "b"))
  expect_identical(ts1, ts2)
})

test_that("as.regts.data.frame with multiple time columns", {
  df <- data.frame(years = c(2018, 2018), quarters = c(1, 2),
                   a = 1:2, stringsAsFactors = TRUE)
  fun <- function(x) {period(paste(x[[1]], x[[2]]), frequency = 4)}
  ts1 <- as.regts(df, fun = fun, time_column = c("years", "quarters"))
  ts2 <- regts(matrix(1:2, ncol =  1), start = "2018Q1", names = "a")
  expect_identical(ts1, ts2)
})

library(testthat)
library(regts)
library(data.table)

context("tranpose_df")

test_that("data frame without row names and labels", {
  df <- data.frame(a = 1:3, b = 10:12)
  df_t <- transpose_df(df)
  df2 <- transpose_df(df_t)
  rownames(df2) <- NULL
  expect_identical(df, df2)
})

test_that("simple data frame with row names and no labels", {
  df <- data.frame(a = 1:3, b = 10:12, row.names = paste0("x", 1:3))
  df_t <- transpose_df(df)
  df2 <- transpose_df(df_t)
  expect_identical(df, df2)
})

test_that("data frame with a column with column names and no labels", {
  df <- data.frame(a = 1:3, b = 10:12, names = paste0("x", 1:3))
  df_t <- transpose_df(df, colname_column = 3)
  df_t2 <- transpose_df(df, colname_column = "names")
  expect_identical(df_t, df_t2)
  df2 <- transpose_df(df_t)

  df_correct <- df
  rownames(df_correct) <- df[[3]]
  df_correct[3] <- NULL
  expect_identical(df2, df_correct)
})

test_that("data frame with a column with column names and labels", {
  df <- data.frame(a = 1:3, b = 10:12, names = paste0("x", 1:3),
                   lbls = paste("Variable", 1:3))
  Hmisc::label(df, self = FALSE) <- c(paste("Variable", c("a", "b")), "", "")
  #View(df)

  df_t <- transpose_df(df, colname_column = 3, label_column = "lbls")
  #View(df_t)
  expect_known_output(df_t, file = file.path("expected_output/transpose_df_t.rds"))

  df_t_2 <- transpose_df(df_t, label_column = 1)
  #View(df_t_2)
  expect_known_output(df_t, file = file.path("expected_output/transpose_df_t_2.rds"))
})

test_that("data frame with numbers and texts", {

  df <- data.frame(numbers = c(1.000000000123, 10.12345678910111212),
                   texts = c("jan", "piet"), stringsAsFactors = FALSE)
  rownames(df) <- c("a", "b")

  df_t <- transpose_df(df)
  df_t_t <- transpose_df(df_t)
  df_t_t$numbers <- as.numeric(df_t_t$numbers)
  expect_equal(df, df_t_t)
})


test_that("data frame with numbers and factors", {

  df <- data.frame(numbers = c(1.000000000123, 10.12345678910111212),
                   texts = c("jan", "piet"), stringsAsFactors = TRUE)
  rownames(df) <- c("a", "b")

  df_t <- transpose_df(df)
  df_t_t <- transpose_df(df_t)
  df_t_t$numbers <- as.numeric(df_t_t$numbers)
  df_t_t$texts <- as.factor(df_t_t$texts)
  expect_equal(df, df_t_t)
})

test_that("data table with numbers, texts and labels", {

  dt <- data.table(names = c("a", "b"),
                   numbers = c(1.000000000123, 10.12345678910111212),
                   texts = c("jan", "piet"), stringsAsFactors = FALSE)
  Hmisc::label(dt, self = FALSE) <- c("Variable names", "Numeric Data",
                                      "Character Data")

  dt_t <- transpose_df(dt, colname_column = "names")

  expect_identical(class(dt_t), c("data.table", "data.frame"))

  dt_t_t <- transpose_df(dt_t, colname_colum   = "names", label_column = "labels")

  expect_identical(Hmisc::label(dt_t_t), c(names = "", numbers = "Numeric Data",
                                           texts = "Character Data"))

  dt_t_t$numbers <- as.numeric(dt_t_t$numbers)
  # the statement above has removed the label, so restore the label.
  Hmisc::label(dt_t_t$numbers) <- "Numeric Data"

  expected_result <- dt
  expected_result$names <- as.character(expected_result$names)
  attr(expected_result$names, "label") <- NULL


  # we have lost the labels, so add them again (check this later)
  #Hmisc::label(dt_t_t, self = FALSE) <- c("Variable names", "Numeric data",
  #                                    "Character Data")

  expect_equal(dt_t_t, expected_result)
})






library(testthat)
library(regts)
library(data.table)

rm(list = ls())

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

  lbls <- c(paste("Variable", c("a", "b")), "", "")
  labelled::var_label(df) <- lbls
  #View(df)

  df_t <- transpose_df(df, colname_column = 3, label_column = "lbls")
  #View(df_t)
  expect_known_value(df_t, file = "expected_output/transpose_df_t.rds")

  df_t_2 <- transpose_df(df_t, label_column = 1)
  #View(df_t_2)
  expect_known_value(df_t_2, file = "expected_output/transpose_df_t_2.rds")
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

  dt <- data.table(name = c("a", "b"),
                   numbers = c(1.000000000123, 10.12345678910111212),
                   texts = c("jan", "piet"), stringsAsFactors = FALSE)

  lbls <- c("Variable names", "Numeric Data", "Character Data")
  labelled::var_label(dt) <- lbls

  dt_t <- transpose_df(dt, colname_column = "name")

  expect_identical(class(dt_t), c("data.table", "data.frame"))

  dt_t_t <- transpose_df(dt_t, colname_colum   = "name", label_column = "label")

  expect_identical(
    labelled::var_label(dt_t_t, unlist = TRUE),
    c(name = "", numbers = "Numeric Data", texts = "Character Data")
  )

  dt_t_t$numbers <- as.numeric(dt_t_t$numbers)
  # the statement above has removed the label, so restore the label.
  attr(dt_t_t$numbers, "label") <- "Numeric Data"

  # dt_t_t has lost the label for variable names (this cannot be avoided),
  # therefore also remove the corresponding label in the expected result.
  expected_result <- dt
  expected_result$name <- as.character(expected_result$name)
  attr(expected_result$name, "label") <- NULL


  expect_equal(dt_t_t, expected_result)
})

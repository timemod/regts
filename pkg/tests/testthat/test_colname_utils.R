library(regts)
library(testthat)


rm(list = ls())

data <- regts(matrix(1:8, ncol = 2), start = "2010Q2", names = c("a", "b"))

test_that("tag_colnames and change_colnames", {
  data_tagged <- tag_colnames(data, "_input")
  expect_equal(colnames(data_tagged), paste0(colnames(data), "_input"))

  data_untagged <- change_colnames(data_tagged,
                                   fun = function(x) {sub("_input$", "", x)})
  expect_equal(data, data_untagged)

  data_tagged_2 <- change_colnames(data_tagged,
                                   fun = function(x, repl) {sub("_input$", repl, x)},
                            repl = "_output")
  expect_equal(colnames(data_tagged_2), paste0(colnames(data), "_output"))

  data_tagged_3 <- tag_colnames(data,  1)
  expect_equal(colnames(data_tagged_3), paste0(colnames(data), "1"))
})

test_that("errors for tag_colnames and change_colnames", {
  expect_error(tag_colnames(data, tag = data),
               "Argument 'tag' should be a vector with length 1")

  expect_error(change_colnames(data, fun = data),
               "Argument 'fun' is not a function")
})


test_that("rename_cols", {
  data_renamed <- rename_cols(data, x = a, y = b)
  expected_result <- data
  colnames(expected_result) <- c("x", "y")
  expect_identical(data_renamed, expected_result)

  data_renamed_2 <- rename_cols(data, y = b, p = a,  x = a)
  expect_identical(data_renamed_2, expected_result)

  data_no_cols <- data
  colnames(data_no_cols) <- NULL
  data_renamed_3 <- rename_cols(data_no_cols, y = 2, x  = 1)
  expect_identical(data_renamed_3, expected_result)

  # duplicate columns
  data_dupl <- data
  colnames(data_dupl) <- c("a", "a")
  data_renamed_4 <- rename_cols(data_dupl, x = "a")
  colnames(expected_result) <- c("x", "a")
  expect_identical(data_renamed_4, expected_result)
})


test_that("rename_cols (errors)", {
  expect_error(rename_cols(data, x = a, y = p))
  expect_error(rename_cols(data, a))
  expect_error(rename_cols(data, x = TRUE))
})


library(regts)
library(testthat)

context("colname_utils")

rm(list = ls())

data <- regts(matrix(1:8, ncol = 2), start = "2010Q2", names = c("a", "b"))

test_that("tag_colnames and change_colnames work correctly", {
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


test_that("errors", {
  expect_error(tag_colnames(data, tag = data),
               "Argument 'tag' should be a vector with length 1")

  expect_error(change_colnames(data, fun = data),
               "Argument 'fun' is not a function")
})

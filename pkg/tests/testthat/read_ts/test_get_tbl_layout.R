library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function get_tbl_layout")

test_that("test1", {

  tbl <- tibble(a = c("2010", "", "a"), b = c("", "2014", "3"))

  expected_result <- list(rowwise = TRUE, period_row = 2L, first_data_col = 2L,
                          last_data_col = 2L, is_data_col = c(FALSE, TRUE),
                          periods = "2014")

  info1 <- regts:::get_tbl_layout(tbl, frequency = NA, xlsx = FALSE)
  expect_identical(info1, expected_result)

  info2 <- regts:::get_tbl_layout(tbl, frequency = 1, xlsx = TRUE)
  expect_identical(info2, expected_result)

  info3 <- regts:::get_tbl_layout(tbl, frequency = NA, rowwise = TRUE,
                                  xlsx = FALSE)
  expect_identical(info3,  expected_result)

  info4 <- regts:::get_tbl_layout(tbl, frequency = NA, rowwise = FALSE,
                                  xlsx = TRUE)
  expect_identical(info4, list(rowwise = FALSE, period_col = 2L,
                               first_data_row = 2L, last_data_col = NA_integer_,
                               is_data_col = c(FALSE, FALSE),
                               is_data_row = c(FALSE, TRUE, TRUE),
                               names = character(0), lbls = character(0)))

  info5 <- regts:::get_tbl_layout(tbl, frequency = 4, xlsx = TRUE)
  expect_null(info5)
})

test_that("test2", {

  tbl1 <- tibble(a = c("2010", "x", "y"), b = c("aap", "z", "g"))
  info1 <- regts:::get_tbl_layout(tbl1, frequency = NA, xlsx = TRUE)
  expect_null(info1)

  tbl2 <- tibble(a = list("2010", 1, 2), b = list("aap", 3, 4))
  info2 <- regts:::get_tbl_layout(tbl2, frequency = NA, xlsx = TRUE)
  expect_identical(info2, list(rowwise = TRUE, period_row = 2L,
                               first_data_col = 2L, last_data_col = 2L,
                               is_data_col = c(FALSE, TRUE),
                               periods = "3"))
})

library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function find_periods")

#
# TODO: find find_periods has been replaced by get_tbl_layout.
# Modify the test job for this function.
#
test_that("dummy", {
  expect_identical(1, 1)
})

# test_that("test1", {
#
#   tbl <- tibble(a = c("2010", "", "a"), b = c("", "2014", "3"))
#
#   expected_result <- list(rowwise = TRUE, row_nr = 2L, col_nr = 2L,
#                           is_period = c(FALSE, TRUE))
#
#   info1 <- regts:::find_periods(tbl, frequency = NA)
#   expect_identical(info1, expected_result)
#
#   info2 <- regts:::find_periods(tbl, frequency = 1)
#   expect_identical(info2, expected_result)
#
#   info3 <- regts:::find_periods(tbl, frequency = NA, rowwise = TRUE)
#   expect_identical(info3,  expected_result)
#
#   info4 <- regts:::find_periods(tbl, frequency = NA, rowwise = FALSE)
#   expect_identical(info4,   list(rowwise = FALSE, row_nr = 2L, col_nr = 2L,
#                                  is_period = c(FALSE, TRUE, TRUE)))
#
#   info5 <- regts:::find_periods(tbl, frequency = 4)
#   expect_null(info5)
# })
#
# test_that("test2", {
#
#   tbl1 <- tibble(a = c("2010", "x", "y"), b = c("aap", "z", "g"))
#   info1 <- regts:::find_periods(tbl1, frequency = NA)
#   expect_null(info1)
#
#   tbl2 <- tibble(a = list("2010", 1, 2), b = list("aap", 3, 4))
#   info2 <- regts:::find_periods(tbl2, frequency = NA)
#   expect_identical(info2, list(rowwise = TRUE, row_nr = 2L, col_nr = 1L,
#                                is_period = c(TRUE, TRUE)))
# })

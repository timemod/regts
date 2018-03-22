library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function list_tbl_2_mat")

test_that("list_tbl_2_mat works correctly", {
  tbl <- tibble(a = list(1L, 2L, "AAP1.123"), b = list(1.123, NA, "2.3"),
              c = list(TRUE, NA, "1.123 jan"))
  msg <- paste0("NAs introduced by coercion\n",
              "The following texts could not be converted to numeric:\n",
              "\"AAP1.123\"\n\"1.123 jan\"")
  expect_warning(mat <- regts:::list_tbl_2_mat(tbl), msg)
  expected_result <- matrix(c(1, 2, NA, 1.123, NA, 2.3, 1, NA, NA), ncol = 3)
  expect_equal(mat, expected_result)
})


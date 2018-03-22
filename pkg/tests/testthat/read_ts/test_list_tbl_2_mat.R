library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function list_tbl_2_mat")

# make sure that we use the standard C ANSI locale
numeric_locale_old <- Sys.getlocale("LC_NUMERIC")
Sys.setlocale("LC_NUMERIC", "C")

test_that("list_tbl_2_mat works correctly", {

  tbl <- tibble(a = list(1L, 2L, "AAP1.123"), b = list(1.123, NA, "2.3"),
              c = list(TRUE, NA, "1.123 jan"))
  msg <- paste0("NAs introduced by coercion\n",
              "The following texts could not be converted to numeric:\n",
              "\"1.123 jan\"\n\"AAP1.123\"")
  expect_warning(mat <- regts:::list_tbl_2_mat(tbl), msg)
  expected_result <- matrix(c(1, 2, NA, 1.123, NA, 2.3, 1, NA, NA), ncol = 3)
  expect_equal(mat, expected_result)
})


test_that("list_tbl_2_mat works correctly", {

  txts <- paste("txt", 1:20)
  tbl <- tibble(a = txts)

  msg <- paste0("NAs introduced by coercion\n",
                "The following texts could not be converted to numeric:\n",
                paste(paste0("\"", sort(txts[1:10]), "\""), collapse = "\n"))
  expect_warning(mat <- regts:::list_tbl_2_mat(tbl), msg)
  expected_result <- matrix(rep(NA_real_, 20), ncol = 1)
  expect_equal(mat, expected_result)
})

# restore original locale
Sys.setlocale("LC_NUMERIC", numeric_locale_old)

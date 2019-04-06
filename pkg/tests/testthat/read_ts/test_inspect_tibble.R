library(regts)
library(testthat)
library(tibble)

rm(list = ls())

context("test internal function inspect_tibble")

test_that("test1", {

  tbl <- tibble(a = c("2010", "", "a"), b = c("", "2014", "3"))

  expected_result <- list(rowwise = TRUE, period_row = 2L, first_data_col = 2L,
                          last_data_col = 2L, is_data_col = c(FALSE, TRUE),
                          periods = "2014")

  info1 <- regts:::inspect_tibble(tbl, frequency = NA, xlsx = FALSE)
  expect_identical(info1, expected_result)

  info2 <- regts:::inspect_tibble(tbl, frequency = 1, xlsx = TRUE)
  expect_identical(info2, expected_result)

  info3 <- regts:::inspect_tibble(tbl, frequency = NA, rowwise = TRUE,
                                  xlsx = FALSE)
  expect_identical(info3,  expected_result)

  info4 <- regts:::inspect_tibble(tbl, frequency = NA, rowwise = FALSE,
                                  xlsx = TRUE)
  expect_identical(info4, list(rowwise = FALSE, period_col = 2L,
                               first_data_row = 2L, last_data_col = NA_integer_,
                               is_data_col = c(FALSE, FALSE),
                               is_data_row = c(FALSE, TRUE, TRUE),
                               names = character(0), lbls = character(0)))

  info5 <- regts:::inspect_tibble(tbl, frequency = 4, xlsx = TRUE)
  expect_null(info5)
})

test_that("test2", {

  tbl1 <- tibble(a = c("2010", "x", "y"), b = c("aap", "z", "g"))
  info1 <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)
  expect_null(info1)

  tbl2 <- tibble(a = list("2010", 1, 2), b = list("aap", 3, 4))
  info2 <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = TRUE)
  expect_identical(info2, list(rowwise = TRUE, period_row = 2L,
                               first_data_col = 2L, last_data_col = 2L,
                               is_data_col = c(FALSE, TRUE),
                               periods = "3"))
})

test_that("single period", {

  tbl1 <- tibble(a = list(NA, NA, "olie"), b = list(NA, 2010, NA),
                 c = list("a", sqrt(2), 2 * sqrt(2)), d =list("b", sqrt(3), NA))
  layout <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)

  expected_result <- list(rowwise = FALSE, period_col = 2L,
                          first_data_row = 2L, last_data_col = 4L,
                          is_data_col = c(FALSE, FALSE, TRUE, TRUE),
                          is_data_row = c(FALSE, TRUE, FALSE),
                          names = c("a", "b"), lbls = NULL)
  expect_identical(layout, expected_result)



})

test_that("missing data columns / rows ", {
  tbl1 <- tibble(a = list(NA, NA, "a", NA), b = list("xxx", 2010, 2011, 2012),
                 c = list(NA, NA, NA, NA))
  layout <- regts:::inspect_tibble(tbl1, frequency = NA, xlsx = TRUE)
  expect_identical(layout, list(rowwise = TRUE, period_row = 2L,
                               first_data_col = 2L, last_data_col = 2L,
                               is_data_col = c(FALSE, TRUE, FALSE),
                               periods = "2010"))

  tbl2 <- tibble(a = c(NA, NA, "a", NA), b = c("xxx", "2010", "2011", "2012"),
                 c = c("y", NA, 3, NA))
  wmsg <- paste("Could not determine if timeseries are stored rowwise or",
                "columnwise.\nAssuming columnwise based on the number of",
                "periods found.\nUse argument rowwise if necessary.")
  expect_warning(layout <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = FALSE,
                                   labels =  "after"), wmsg)
  expect_identical(layout, list(rowwise = FALSE, period_col = 2L,
                                first_data_row = 2L, last_data_col = 3L,
                                is_data_col = c(FALSE, FALSE, TRUE),
                                is_data_row = c(FALSE, rep(TRUE, 3)),
                                names = "y", lbls = NULL))

  layout_roww <- regts:::inspect_tibble(tbl2, frequency = NA, xlsx = FALSE,
                                   rowwise = TRUE)
  expect_identical(layout_roww, list(rowwise = TRUE, period_row = 2L,
                                first_data_col = 2L, last_data_col = 2L,
                                is_data_col = c(FALSE, TRUE, FALSE),
                                periods = "2010"))


  tbl3 <- tibble(z = list("emu", NA, NA, NA), a = list(NA, NA, 2010, NA),
                 b = list("var a", "a", 2011, NA), c = list(NA, NA, 2012, NA))
  layout <- regts:::inspect_tibble(tbl3, frequency = NA, xlsx = TRUE,
                                   labels = "before")
  expect_identical(layout, list(rowwise = FALSE, period_col = 2L,
                                first_data_row = 3L, last_data_col = 3L,
                                is_data_col = c(FALSE, FALSE, TRUE, FALSE),
                                is_data_row = c(FALSE, FALSE, TRUE, FALSE),
                                names = "a", lbls = "var a"))
})

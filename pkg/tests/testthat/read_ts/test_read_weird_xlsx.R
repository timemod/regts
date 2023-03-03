library(regts)
library(testthat)

rm(list = ls())



test_that("weird_isis.xlsx is read correctly",  {

  xlsx_file <- "xlsx/weird_isis.xlsx"

  a <- regts(sqrt(1:4), start = "2000")
  b <- 2 * a
  expected_result <- cbind(a, b)

  result1 <- read_ts_xlsx(xlsx_file)
  expect_equal(result1, expected_result)

  result2 <- read_ts_xlsx(xlsx_file, rowwise = TRUE)
  expect_equal(result2, expected_result)

  expect_error(read_ts_xlsx(xlsx_file, frequency = 4),
               "No periods found")

  expect_error(read_ts_xlsx(xlsx_file, rowwise = TRUE, frequency = 4),
               "No periods found on sheet 1 of file xlsx/weird_isis.xlsx\n")
})

test_that("weird_1.xlsx is read correctly",  {

  xlsx_file <- "xlsx/weird_1.xlsx"

  a <- regts(sqrt(1:4), start = "2000")
  b <- 2 * a
  expected_result <- cbind(a, b)

  result1 <- read_ts_xlsx(xlsx_file)
  expect_equal(result1, expected_result)
})

test_that("weird_2.xlsx is read correctly",  {

  xlsx_file <- "xlsx/weird_2.xlsx"

  a <- regts(2, start = "2002Q2")
  b <- 2 * a
  expected_result1 <- cbind(a, b)

  result1 <- read_ts_xlsx(xlsx_file, frequency = 4, labels = "no")
  expect_equal(result1, expected_result1)

  a <- regts(sqrt(1:3), start = "2000")
  b <- 2 * a
  expected_result2 <- cbind(a, b)

  result2 <- read_ts_xlsx(xlsx_file, frequency = 1)
  expect_equal(result2, expected_result2)

  msg <- paste0("The row B1:E1 of sheet 1 of file xlsx/weird_2.xlsx contains\n",
                "periods with different frequencies.")
  expect_error(read_ts_xlsx(xlsx_file), msg)
})


test_that("weird_3.xlsx is read correctly",  {

  xlsx_file <- "xlsx/weird_3.xlsx"
  result1 <- read_ts_xlsx(xlsx_file)

  expected_result <- regts(matrix(as.numeric(1:2), ncol = 2),
                           names = c("a", "b"), start = "2011")
  expect_identical(result1, expected_result)
})

test_that("weird_4.xlsx",  {

  xlsx_file <- "xlsx/weird_4.xlsx"

  wmsg <- "Assuming columnwise based on the number of periods found."
  emsg <- paste("The column B2:B4 of sheet 1 of file xlsx/weird_4.xlsx",
                "contains\nperiods with different frequencies.")

  expect_warning(
    expect_error(
      read_ts_xlsx(xlsx_file), emsg
    ), wmsg)
})


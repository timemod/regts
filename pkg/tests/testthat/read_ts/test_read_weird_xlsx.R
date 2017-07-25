library(regts)
library(testthat)

context("read_ts_xlsx for weird xlsx files")


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
               "No periods found when reading rowwise timeseries")
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

  result1 <- read_ts_xlsx(xlsx_file, frequency = 4)
  expect_equal(result1, expected_result1)

  a <- regts(sqrt(1:3), start = "2000")
  b <- 2 * a
  expected_result2 <- cbind(a, b)

  result2 <- read_ts_xlsx(xlsx_file, frequency = 1)
  expect_equal(result2, expected_result2)

  expect_error(read_ts_xlsx(xlsx_file),
               "The time column\\(s\\) contain different frequencies")
})

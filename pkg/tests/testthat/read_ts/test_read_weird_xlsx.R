library(regts)
library(testthat)

rm(list = ls())

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

  expected_result3 <- regts(matrix(NA, ncol = 2), period = "2000q1/2003q1",
                           names = c("a", "b"))
  expected_result3[c(1, 5, 9, 13), ] <- as.numeric(result1)

  result3 <- read_ts_xlsx(xlsx_file, frequency = 4, strict = FALSE)
  expect_equal(result3, expected_result3)
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

  result1 <- read_ts_xlsx(xlsx_file, frequency = 4, labels = "no",
                          strict = FALSE)

  expected_result1 <- regts(matrix(NA, ncol = 2), period = "2001q1/2002q2",
                            names = c("a", "b"))
  expected_result1[c(1, 6), ] <- c(sqrt(2), 2, sqrt(8), 4)


  expect_equal(result1, expected_result1)

  a <- regts(sqrt(1:3), start = "2000")
  b <- 2 * a
  expected_result2 <- cbind(a, b)

  result2 <- read_ts_xlsx(xlsx_file, frequency = 1)
  expect_equal(result2, expected_result2)

  expect_error(read_ts_xlsx(xlsx_file),
               "The time column\\(s\\) contain different frequencies")
})


test_that("weird_3.xlsx is read correctly",  {

  xlsx_file <- "xlsx/weird_3.xlsx"
  result1 <- read_ts_xlsx(xlsx_file)

  expected_result <- regts(matrix(as.numeric(1:2), ncol =2), names = c("a", "b"),
                           start = "2011")
  expect_identical(result1, expected_result)
})



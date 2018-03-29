library(regts)
library(testthat)

context("read_ts_csv for weird csv")

test_that("weird_1.csv is read correctly",  {

  csv_file <- "csv/weird_1.csv"
  result1 <- read_ts_csv(csv_file)

  expected_result <- regts(matrix(as.numeric(1:2), ncol =2), names = c("a", "b"),
                           start = "2011")
  expect_identical(result1, expected_result)
})

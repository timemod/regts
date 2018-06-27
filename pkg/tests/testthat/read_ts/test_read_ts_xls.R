library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx for xls files")

# construct correct result
prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1.1234567890123, NA, NA, 5, 6), period =  prd)
b <- 10 * a
b[1] <- 10.0
correct_result <- cbind(a, b)

correct_result_labels <- correct_result
ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")


test_that("example1.xls is read correctly",  {

  xls_file <- "xlsx/example1.xls"

  result <- read_ts_xlsx(xls_file, skiprow = 1, labels = "no")
  expect_equal(result, correct_result)

  result2 <- read_ts_xlsx(xls_file, skiprow = 1)
  expect_identical(result2, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  ts_labels(correct_result_labels2) <- "b Timeseries b"

  result3 <- read_ts_xlsx(xls_file, skiprow = 1, labels = "before")
  expect_identical(result3, correct_result_labels2)
})


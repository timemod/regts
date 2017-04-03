library(regts)
library(testthat)

context("read_ts_xlsx")

# construct correct result
period <- regperiod_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5, 6), period = period)
b <- 10 * a
correct_result <- cbind(a, b)

correct_result_labels <- correct_result
ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")


test_that("example1.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example1.xlsx"

  result <- read_ts_xlsx(xlsx_file, skiprow = 1)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "after")
  expect_identical(result2, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  ts_labels(correct_result_labels2) <- "b Timeseries b"

  result3 <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "before")
  expect_identical(result3, correct_result_labels2)
})

test_that("example2.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example2.xlsx"

  result <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, sheet = "example2",
                          skipcol = 1, labels = "after")
  expect_identical(result2, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  correct_result_labels2[, "Model Taxus ran for the CEP 2017"] <- NA
  correct_result_labels2 <- correct_result_labels2[, c(2,1)]
  ts_labels(correct_result_labels2) <- c("", "b Timeseries b")

  result3 <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2,
                          labels = "before")

  expect_identical(result3, correct_result_labels2)
})

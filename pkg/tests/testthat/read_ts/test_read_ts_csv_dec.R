library(regts)
library(testthat)

context("read_ts_csv for csv files with many decimals")

prd <- period_range("2010Q2/2010Q3")
num_dec <- 1.123456789
correct_result <- regts(matrix(c(num_dec, 1, NA, num_dec), ncol = 2),
                        names = c("a", "b"),
                        labels = paste("Timeseries", c("a", "b")),
                        period =  prd)

msg <- paste0("NAs introduced by coercion\n",
              "The following texts could not be converted to numeric:",
              "\"x\"")

test_that("rowwise3.csv is read correctly",  {
  csv_file <- "csv/rowwise3.csv"
  expect_warning(result <- read_ts_csv(csv_file, labels = "after"), msg = msg)
  expect_identical(result, correct_result)
})

test_that("rowwise4.csv is read correctly",  {
  csv_file <- "csv/rowwise4.csv"
  expect_warning(result <- read_ts_csv(csv_file, labels = "after", dec = ","),
                 msg = msg)
  expect_identical(result, correct_result)
})

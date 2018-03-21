library(regts)
library(testthat)

context("read_ts_xlsx for xlsx files with many decimals")

prd <- period_range("2010Q2/2010Q3")
num_dec <- 1.123456789
correct_result <- regts(matrix(c(num_dec, 1, NA, num_dec), ncol = 2),
                        names = c("a", "b"),
                        labels = paste("Timeseries", c("a", "b")),
                        period =  prd)

msg <- paste0("NAs introduced by coercion\n",
              "The following texts could not be converted to numeric:\n",
              "\"x\"")

test_that("example6.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example6.xlsx"

  expect_warning(result <- read_ts_xlsx(xlsx_file, labels = "after"),  msg)
  expect_identical(result, correct_result)
})

test_that("example7.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example7.xlsx"
  expect_warning(result <- read_ts_xlsx(xlsx_file, labels = "after"), msg)
  expect_identical(result, correct_result)
})

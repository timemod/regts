library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx for WTM file with Date cells")

test_that("read_ts_xlsx should give an error",  {

  xlsx_file <- "xlsx/wtm_date_cells.xlsx"
  warnings <- capture_warnings(
    x <- read_ts_xlsx(xlsx_file, skiprow = 1)
  )

  expect_known_output(warnings, file =  "expected_output/wtm_1_warn.txt",
                      print = TRUE)

  a <- regts(1:230, start = "2000m1")
  expected_result <- cbind(a, b = sqrt(a))
  expect_equal(x, expected_result)

  result2 <- read_ts_xlsx(xlsx_file)
  expect_equal(get_period_range(result2), period_range("2000m1/2019m2"))
  expect_equal(ncol(result2), 0)
})

library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx for WTM file with Date cells")

test_that("read_ts_xlsx should give an error",  {

  xlsx_file <- "xlsx/wtm_date_cells.xlsx"
  warnings <- capture_warnings(
    result1 <- read_ts_xlsx(xlsx_file)
  )

  expect_known_output(warnings, file =  "expected_output/wtm_1_warn.txt",
                      print = TRUE)

  a <- regts(1:230, start = "2000m1")
  expected_result <- cbind(a, b = sqrt(a))
  expect_equal(result1, expected_result)
})

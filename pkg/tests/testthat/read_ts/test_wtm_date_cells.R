library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx for WTM file with Date cells")

test_that("read_ts_xlsx should give an error",  {
  xlsx_file <- "xlsx/wtm_date_cells.xlsx"
  warnings <- capture_warnings(
    x <- read_ts_xlsx(xlsx_file)
  )
  expect_known_output(warnings, file =  "expected_output/wtm_1_warn.txt",
                      print = TRUE)
  expect_known_value(x, file =  "expected_output/wtm_output.rds")
})


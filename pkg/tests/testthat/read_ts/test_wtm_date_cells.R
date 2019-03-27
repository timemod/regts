library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx for WTM file with Date cells")

test_that("read_ts_xlsx should give an error",  {
  xlsx_file <- "xlsx/wtm_date_cells.xlsx"
  expect_error(read_ts_xlsx(xlsx_file),
               "Found Date values in cells were numerical values are expected")
})


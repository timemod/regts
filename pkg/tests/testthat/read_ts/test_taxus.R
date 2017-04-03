library(regts)
library(testthat)

context("read_ts_xlsx for some Taxus input files")

test_that("taxus_fix1.xlsx is read correctly",  {
  xlsx_file <- "xlsx/taxus_fix1.xlsx"
  fix1 <- read_ts_xlsx(xlsx_file, labels = "before")
  #View(fix1)
  expect_equal_to_reference(fix1, file = "taxus_fix1.rds")
})

test_that("taxus_fix3.xlsx is read correctly",  {
  xlsx_file <- "xlsx/taxus_fix3.xlsx"
  fix3 <- read_ts_xlsx(xlsx_file, labels = "before")
  #View(fix3)
  expect_equal_to_reference(fix3, file = "taxus_fix3.rds")
})

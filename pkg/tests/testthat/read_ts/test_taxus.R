library(regts)
library(testthat)


test_that("taxus_fix1.xlsx is read correctly",  {
  xlsx_file <- "xlsx/taxus_fix1.xlsx"
  fix1 <- read_ts_xlsx(xlsx_file, labels = "before")
  #View(fix1)
  expect_known_value(fix1, file = "taxus_fix1.rds")
})

test_that("taxus_fix3.xlsx is read correctly",  {
  xlsx_file <- "xlsx/taxus_fix3.xlsx"
  fix3 <- read_ts_xlsx(xlsx_file, labels = "before")
  #View(fix3)
  expect_known_value(fix3, file = "taxus_fix3.rds")
})

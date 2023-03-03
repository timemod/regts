library(regts)
library(testthat)

rm(list = ls())

test_that("example_duplicate_names.xlsx has duplicate names",  {
  xlsx_file <- "xlsx/example_duplicate_names.xlsx"
  msg <- paste("Duplicate names on sheet 1 of file",
               "xlsx/example_duplicate_names.xlsx: b, a")
  expect_warning(result1 <- read_ts_xlsx(xlsx_file, strict = FALSE), msg)
  expect_warning(result2 <- read_ts_xlsx(xlsx_file, strict = FALSE,
                                        warn_dupl = FALSE), NA)

  # construct correct result
  prd <- period_range("2010Q2/2011Q2")
  a_1 <- regts(c(1, NA, NA, 5, 6), period =  prd)
  b_1 <- 10 * a_1
  c <- b_1
  b_2 <- 2 * a_1
  a_2 <- b_2
  correct_result <- cbind(a_1, b_1, c, b_2, a_2)
  colnames(correct_result) <-  sub("_\\d$", "", colnames(correct_result))
  ts_labels(correct_result) <- paste("Timeseries", c("a", "b (EUR)", "c (EUR)",
                                                     "d", "e"))

  # compare with correct result:
  expect_identical(result1, correct_result)
  expect_identical(result2, correct_result)
})

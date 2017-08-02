library(regts)
library(testthat)

context("read_ts_xlsx")

# construct correct result
prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5, 6), period =  prd)
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

  result4 <- read_ts_xlsx(xlsx_file, range = "example2!B1:H6",  skipcol = 999)
  expect_identical(result4, correct_result)

  result5 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols("B:H"),
                          sheet = "example2", skipcol = 999)
  expect_identical(result5, correct_result)
})

test_that("example3.xlsx is read correctly (leading empty rows and columns are skipped)",  {

  xlsx_file <- "xlsx/example3.xlsx"

  result <- read_ts_xlsx(xlsx_file)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)))
  expect_identical(result2, correct_result)

  result3 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_rows(c(2, NA)))
  expect_identical(result2, correct_result)
})

test_that("example4.xlsx is read correctly (leading empty columns are skipped)",  {

  xlsx_file <- "xlsx/example4.xlsx"

  result <- read_ts_xlsx(xlsx_file, sheet = "example2")
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_limits(c(NA, 2),
                                                                   sheet = "example2"))
  expect_identical(result2, correct_result)
})

test_that("example5.xlsx is read correctly (leading empty are skipped)",  {
  xlsx_file <- "xlsx/example5.xlsx"
  result <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)),
                         sheet = "example2")
  expect_identical(result, correct_result)

  msg <- "The first 100 rows are all empty.\n Therefore we could not determine whether the timeseries are rowwise or columnwise.\n Please supply argument columnwise."
  expect_error(read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)),
                         sheet = "Sheet3"), msg)
})

test_that("example8.xlsx is read correctly (names to lowercase)",  {

  xlsx_file <- "xlsx/example8.xlsx"
  result <- read_ts_xlsx(xlsx_file, fun = tolower)
  expect_identical(result, correct_result)
})

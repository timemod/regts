library(regts)
library(testthat)

rm(list = ls())

context("read_ts_xlsx")

# construct correct result
prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5, 6), period =  prd)
b <- 10 * a
correct_result <- cbind(a, b)

period_fun <- function(x) {
  x <- paste("1", x)
  x <- lubridate::dmy(x, quiet = TRUE)
  ret <- paste(lubridate::year(x), "Q", (lubridate::month(x) %/% 3 + 1))
  return(ret)
}

correct_result_labels <- correct_result
ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")


test_that("example1.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example1.xlsx"

  result <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "no", strict = FALSE)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "after", strict = FALSE)
  expect_identical(result2, correct_result_labels)

  result2a <- read_ts_xlsx(xlsx_file, skiprow = 1, strict = FALSE)
  expect_identical(result2a, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  ts_labels(correct_result_labels2) <- "b Timeseries b"

  result3 <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "before", strict = FALSE)
  expect_identical(result3, correct_result_labels2)
})

test_that("example2.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example2.xlsx"

  result <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2, labels = "no",
                         strict = FALSE)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, sheet = "example2", skipcol = 1,
                          labels = "after", strict = FALSE)
  expect_identical(result2, correct_result_labels)

  result2a <- read_ts_xlsx(xlsx_file, sheet = "example2", skipcol = 1, strict = FALSE)
  expect_identical(result2a, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  correct_result_labels2[, "Model Taxus ran for the CEP 2017"] <- NA
  correct_result_labels2 <- correct_result_labels2[, c(2,1)]
  ts_labels(correct_result_labels2) <- c("", "b Timeseries b")

  result3 <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2,
                          labels = "before", strict = FALSE)

  expect_identical(result3, correct_result_labels2)

  result4 <- read_ts_xlsx(xlsx_file, range = "example2!B1:H6",  skipcol = 999,
                          labels = "no", strict = FALSE)
  expect_identical(result4, correct_result)

  result5 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols("B:H"),
                          sheet = "example2", skipcol = 999, strict = FALSE)
  expect_identical(result5, correct_result_labels)
})

test_that("example3.xlsx is read correctly (leading empty rows and columns are skipped)",  {

  xlsx_file <- "xlsx/example3.xlsx"

  result <- read_ts_xlsx(xlsx_file, period_fun = period_fun, frequency = 4,
                         strict = FALSE)
  expect_identical(result, correct_result)

  # argument labels should have no effect if there are no labels
  result2 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)),
                          labels = "after", period_fun = period_fun,
                          strict = FALSE)
  expect_identical(result2, correct_result)

  result3 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_rows(c(2, NA)),
                          labels = "before", period_fun = period_fun, strict = FALSE)
  expect_identical(result2, correct_result)
})

test_that("example4.xlsx is read correctly (leading empty columns are skipped)",  {

  xlsx_file <- "xlsx/example4.xlsx"

  result <- read_ts_xlsx(xlsx_file, sheet = "example2", labels = "no",
                         period_fun = period_fun, strict = FALSE)
  expect_identical(result, correct_result)

  result2 <- read_ts_xlsx(xlsx_file, range = cellranger::cell_limits(c(NA, 2),
                                                          sheet = "example2"),
                          period_fun = period_fun, strict = FALSE)
  expect_identical(result2, correct_result_labels)
})

test_that("example5.xlsx is read correctly (leading empty are skipped)",  {
  xlsx_file <- "xlsx/example5.xlsx"
  result <- read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)),
                         sheet = "example2", strict = FALSE)
  expect_identical(result, correct_result_labels)

  msg <- "Sheet Sheet3 of file xlsx/example5.xlsx is empty\n"
  expect_error(read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", NA)),
                         sheet = "Sheet3"), msg)

  msg <- "No periods found on Sheet example2 of file xlsx/example5.xlsx\n"
  expect_error(read_ts_xlsx(xlsx_file, range = cellranger::cell_cols(c("B", "D")),
                            sheet = "example2"), msg)
})

test_that("example8.xlsx is read correctly (names to lowercase)",  {

  xlsx_file <- "xlsx/example8.xlsx"
  result1 <- read_ts_xlsx(xlsx_file, name_fun = tolower, strict = FALSE)
  expect_identical(result1, correct_result)

  # argument labels should have no effect if there are no labels
  result2 <- read_ts_xlsx(xlsx_file, name_fun = tolower, labels = "before",
                          strict = FALSE)
  expect_identical(result2, correct_result)
})

test_that("example11.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example11.xlsx"

  result <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "no", strict = FALSE)
  expect_identical(result, correct_result)
})

test_that("example12.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example12.xlsx"

  result <- read_ts_xlsx(xlsx_file)
  expect_identical(result, correct_result_labels)
})

test_that("example13.xlsx (reversed period) is read correctly",  {

  xlsx_file <- "xlsx/example13.xlsx"
  result1 <- read_ts_xlsx(xlsx_file, skiprow = 1)
  expect_identical(result1, correct_result_labels)

  expect_message(
    # read_excel prints a message about different sheet specified in range
    # and sheet
    result2 <- read_ts_xlsx(xlsx_file, range = "example1!a2:e6", sheet= "xxx")
  )
  expect_identical(result2, correct_result_labels["2011q1/2011q2"])

  expect_error(
    read_ts_xlsx(xlsx_file, range = "example1!c5:e6", frequency = 4),
    "No periods found on Sheet example1 of file xlsx/example13.xlsx")
})

test_that("example14.xlsx is read correctly",  {

  xlsx_file <- "xlsx/example14.xlsx"

  result <- read_ts_xlsx(xlsx_file, strict = FALSE)
  expect_identical(result, correct_result_labels)
})


# test with strict parameter, by default = TRUE
msg <- "Missing periods found \\(2010Q3, 2010Q4\\). Use parameter strict!"

test_that("example5.xlsx, missing periods",  {

  xlsx_file <- "xlsx/example5.xlsx"
  expect_error(read_ts_xlsx(xlsx_file, skiprow = 2, sheet = "example2",
                            labels = "no"), msg)
})

test_that("example11.xlsx, missing periods",  {

  xlsx_file <- "xlsx/example11.xlsx"
  expect_error(read_ts_xlsx(xlsx_file, skiprow = 1, labels = "no"), msg)
})

test_that("example15.xlsx, multiple year frequencies",  {

  xlsx_file <- "xlsx/example15.xlsx"

  result1 <- read_ts_xlsx(xlsx_file)

  expected_result <- regts(matrix(c(1:4, sqrt(1:4)), nrow = 2, byrow = TRUE),
                           start = 2010, names = c("a", "b", "c", "d"))
  expect_equal(result1, expected_result)
})





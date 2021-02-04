library(regts)
library(testthat)

context("read_ts_csv")

rm(list = ls())

# construct correct results
prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5, 6), period =  prd)
b <- 10 * a
correct_result <- cbind(a, b)

correct_result_labels <- correct_result
ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

prd2 <- period_range("2010Q4/2011Q2")
a <- regts(c(1, 5, 6), period =  prd2)
b <- 10 * a
correct_result2 <- cbind(a, b)

period_fun <- function(x) {
  x <- paste("1", x)
  x <- lubridate::dmy(x, quiet = TRUE)
  ret <- paste(lubridate::year(x), "Q", (lubridate::month(x) %/% 3 + 1))
  return(ret)
}

test_that("rowwise1.csv is read correctly",  {

  csv_file <- "csv/rowwise1.csv"
  result <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result, correct_result)

  # argument labels should have no effect if there are no labels
  result2 <- read_ts_csv(csv_file, labels = "after", strict = FALSE)
  expect_identical(result2, correct_result)

  # argument labels should have no effect if there are no labels
  result3 <- read_ts_csv(csv_file, labels = "before", strict = FALSE)
  expect_identical(result3, correct_result)
})

test_that("rowwise2.csv is read correctly",  {

  csv_file <- "csv/rowwise2.csv"

  result <- read_ts_csv(csv_file, labels = "no", strict = FALSE)
  expect_identical(result, correct_result)

  result2 <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result2, correct_result_labels)

  result3 <- read_ts_csv(csv_file,  labels = "after", strict = FALSE)
  expect_identical(result3, correct_result_labels)

  result4 <- read_ts_csv(csv_file, skipcol = 1, rowwise = FALSE, strict = FALSE)
  expected_result <- regts(matrix(c(5, rep(NA, 8), 50, 6, rep(NA, 8), 60),
                           ncol = 2), names = c("2011Q1", "2011Q2"))
  expect_identical(result4, expected_result)
})

test_that("columnwise1.csv is read correctly",  {
  csv_file <- "csv/columnwise1.csv"
  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result2)

  # argument labels should have no effect if there are no labels
  result2 <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result2, correct_result2)
})


test_that("columnwise2.csv is read correctly",  {

  csv_file <- "csv/columnwise2.csv"

  result <- read_ts_csv(csv_file, labels = "no", strict = FALSE)
  expect_equal(result, correct_result)
  expect_identical(result, correct_result * 1)

  correct_result_labels_tmp <- correct_result * 1
  ts_labels(correct_result_labels_tmp) <- c("Timeseries a", "Timeseries b")

  result2 <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result2, correct_result_labels_tmp)

  result3 <- read_ts_csv(csv_file, labels = "after", strict = FALSE)
  expect_identical(result3, correct_result_labels_tmp)

  correct_result_labels2 <- correct_result * 1
  colnames(correct_result_labels2) <- c("Timeseries a", "Timeseries b")
  ts_labels(correct_result_labels2) <- c("a", "b")

  result4 <- read_ts_csv(csv_file, labels = "before", strict = FALSE)
  expect_identical(result4, correct_result_labels2)
})

test_that("columnwise3.csv is read correctly",  {

  csv_file <- "csv/columnwise3.csv"

  result <- read_ts_csv(csv_file, labels = "no", strict = FALSE)
  expect_equal(result, correct_result)
  expect_identical(result, correct_result * 1)

  correct_result_labels <- correct_result * 1
  ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

  result2 <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result2, correct_result_labels)

  result3 <- read_ts_csv(csv_file, labels = "after", strict = FALSE)
  expect_identical(result3, correct_result_labels)
})


test_that("example1.csv is read correctly",  {

  csv_file <- "csv/example1.csv"

  result <- read_ts_csv(csv_file, labels = "no", strict = FALSE)
  expect_identical(result, correct_result * 1)

  result2 <- read_ts_csv(csv_file, skiprow = 1, strict = FALSE)
  expect_identical(result2, correct_result_labels * 1)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  ts_labels(correct_result_labels2) <- "b Timeseries b"

  result3 <- read_ts_csv(csv_file, labels = "before", strict = FALSE)
  expect_identical(result3, correct_result_labels2 * 1)
})

test_that("example2.csv is read correctly",  {

  csv_file <- "csv/example2.csv"

  result <- read_ts_csv(csv_file, skipcol = 1, labels = "no", strict = FALSE)
  expect_identical(result, correct_result)

  result2 <- read_ts_csv(csv_file, skipcol = 1, labels = "after", strict = FALSE)
  expect_identical(result2, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  correct_result_labels2[, "Model Taxus ran for the CEP 2017"] <- NA
  correct_result_labels2 <- correct_result_labels2[, c(2,1)]
  ts_labels(correct_result_labels2) <- c("", "b Timeseries b")

  result3 <- read_ts_csv(csv_file, skipcol = 1, labels = "before", strict = FALSE)
  expect_identical(result3, correct_result_labels2)
})

test_that("example3.csv is read correctly",  {

  csv_file <- "csv/example3.csv"

  correct_result_tmp <- correct_result_labels
  correct_result_tmp["2010Q2", "a"] <- 1.25
  correct_result_tmp["2010Q2", "b"] <- NA

  msg <- paste("NAs introduced by coercion",
               "The following texts could not be converted to numeric:\n",
               "\"x,jan\"")
  expect_warning({
    result <- read_ts_csv(csv_file, skipcol = 1, dec = ",",
                          period_fun = period_fun, strict = FALSE)
  }, msg = msg)
  expect_identical(result, correct_result_tmp)
})

test_that("example4.csv is read correctly",  {

  csv_file <- "csv/example4.csv"

  correct_result_tmp <- correct_result_labels
  correct_result_tmp["2011Q1", "a"] <- NA
  correct_result_tmp["2011Q2", "a"] <- 6.25

  msg <- paste("NAs introduced by coercion",
               "The following texts could not be converted to numeric:\n",
               "\"aap,x\"")
  expect_warning({
    result <- read_ts_csv(csv_file, dec = ",", period_fun = period_fun,
                          strict = FALSE)
  }, msg = msg)
  expect_identical(result, correct_result_tmp)
})

test_that("rowwise5.csv is read correctly",  {
  csv_file <- "csv/rowwise5.csv"
  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result["2010Q2", ])
})

test_that("rowwise6.csv is read correctly",  {
  csv_file <- "csv/rowwise6.csv"
  result <- read_ts_csv(csv_file, labels = "after", strict = FALSE)
  expect_identical(result, correct_result_labels["2010Q2", "a", drop = FALSE])
})

test_that("comment rows are skipped (rowwise7.csv))",  {
  csv_file <- "csv/rowwise7.csv"
  result <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result, correct_result)
})

test_that("argument fill is working (rowwise8.csv))",  {
  csv_file <- "csv/rowwise8.csv"
  result <- read_ts_csv(csv_file, fill = TRUE)
  correct_result_tmp <- correct_result
  correct_result_tmp["2011q2", "a"] <- NA
  expect_identical(result, correct_result_tmp)
})

test_that("columnwise6.csv is read correctly",  {
  csv_file <- "csv/columnwise6.csv"
  result <- read_ts_csv(csv_file, labels = "after", strict = FALSE)
  expect_identical(result, correct_result_labels["2010Q2", ])
})

test_that("columnwise7.csv is read correctly",  {
  csv_file <- "csv/columnwise7.csv"
  result <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result, correct_result["2010Q2", "a", drop = FALSE])
})

test_that("columnwise8.csv is read correctly",  {
  csv_file <- "csv/columnwise8.csv"
  result <- read_ts_csv(csv_file, strict = FALSE)
  expect_identical(result, correct_result * 1)

  # if there are no labels, then the labels option should have no effect
  result2 <- read_ts_csv(csv_file, labels = "before", strict = FALSE)
  expect_identical(result, correct_result * 1)
})


test_that("example5.csv read correctly", {
  csv_file <- "csv/example5.csv"
  result1 <- read_ts_csv(csv_file, skipcol = 1)
  expect_identical(result1, regts(matrix(seq(1, 2.5, by = 0.5), ncol = 2),
                                  names = c("a", "b"), start = "2011q1"))

  expect_error(read_ts_csv(csv_file, skipcol = 1, rowwise = FALSE),
               paste("Periods with different frequencies found in column C of",
                    "file csv/example5.csv."))
})

test_that("example6.csv read correctly", {
  csv_file <- "csv/example6.csv"

  warnings <- capture_warnings(result1 <- read_ts_csv(csv_file))

  expect_known_output(warnings, "expected_output/read_ts_csv_ex61_warn.txt",
                      print = TRUE)

  expect_identical(result1, regts(matrix(c(NA, 1, 2), ncol = 1),
                                  names = c("a"), start = "2011"))

  expect_error(read_ts_csv(csv_file, rowwise = TRUE),
               paste("Periods with different frequencies found in the 2'th",
                     "non-skipped row in file file csv/example6.csv."))
})

test_that("double_row.csv is read correctly and gives warning",  {
  csv_file <- "csv/double_row.csv"
  msg <- "Duplicate names in file csv/double_row.csv: a"
  expect_warning(result1 <- read_ts_csv(csv_file, strict = FALSE), msg)
  expect_warning(result2 <- read_ts_csv(csv_file, strict = FALSE,
                                         warn_dupl = FALSE), NA)
  prd <- period_range("2010Q2/2011Q2")
  a_1 <- regts(c(1, NA, NA, 5, 6), period =  prd)
  correct_result <- cbind(a_1, b = 10 * a_1, a_2 = a_1)
  colnames(correct_result) <-  sub("_\\d$", "", colnames(correct_result))
  expect_identical(result1, correct_result)
  expect_identical(result2, correct_result)
})

test_that("example 9 and 10", {

  expected_result <- regts(matrix(NA_real_, nrow = 2, ncol = 0),
                           start = "2010q1")

  result_9 <- read_ts_csv("csv/example9.csv")
  expect_identical(result_9, expected_result)

  result_10 <- read_ts_csv("csv/example10.csv", rowwise = FALSE)
  expect_identical(result_10, expected_result)
})


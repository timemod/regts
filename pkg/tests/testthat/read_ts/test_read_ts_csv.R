library(regts)
library(testthat)

context("read_ts_csv")

# construct correct result
prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5, 6), period =  prd)
b <- 10 * a
correct_result <- cbind(a, b)

correct_result_labels <- correct_result
ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

test_that("rowwise1.csv is read correctly",  {
  csv_file <- "csv/rowwise1.csv"
  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result)
})

test_that("rowwise2.csv is read correctly",  {

  csv_file <- "csv/rowwise2.csv"

  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result)

  result2 <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result2, correct_result_labels)
})

test_that("columnwise1.csv is read correctly",  {
  csv_file <- "csv/columnwise1.csv"
  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result * 1)
})

test_that("columnwise2.csv is read correctly",  {

  csv_file <- "csv/columnwise2.csv"

  result <- read_ts_csv(csv_file)
  expect_equal(result, correct_result)
  expect_identical(result, correct_result * 1)

  correct_result_labels_tmp <- correct_result * 1
  ts_labels(correct_result_labels_tmp) <- c("Timeseries a", "Timeseries b")

  result2 <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result2, correct_result_labels_tmp)

  result3 <- read_ts_csv(csv_file, labels = "no")
  result4 <- read_ts_csv(csv_file, labels = "after")

  expect_identical(result3, correct_result * 1)
  expect_identical(result4, correct_result_labels_tmp)

  correct_result_labels2 <- correct_result * 1
  colnames(correct_result_labels2) <- c("Timeseries a", "Timeseries b")
  ts_labels(correct_result_labels2) <- c("a", "b")

  result5 <- read_ts_csv(csv_file, labels = "before")
  expect_identical(result5, correct_result_labels2)
})

test_that("columnwise3.csv is read correctly",  {

  csv_file <- "csv/columnwise3.csv"

  result <- read_ts_csv(csv_file)
  expect_equal(result, correct_result)
  expect_identical(result, correct_result * 1)

  correct_result_labels <- correct_result * 1
  ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

  result2 <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result2, correct_result_labels)

  result3 <- read_ts_csv(csv_file, labels = "no")
  result4 <- read_ts_csv(csv_file, labels = "after")

  expect_identical(result3, correct_result * 1)
  expect_identical(result4, correct_result_labels)
})


test_that("example1.csv is read correctly",  {

  csv_file <- "csv/example1.csv"

  result <- read_ts_csv(csv_file, skiprow = 1)
  expect_identical(result, correct_result * 1)

  result2 <- read_ts_csv(csv_file, skiprow = 1, labels = "after")
  expect_identical(result2, correct_result_labels * 1)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  ts_labels(correct_result_labels2) <- "b Timeseries b"

  result3 <- read_ts_csv(csv_file, skiprow = 1, labels = "before")
  expect_identical(result3, correct_result_labels2 * 1)
})

test_that("example2.csv is read correctly",  {

  csv_file <- "csv/example2.csv"

  result <- read_ts_csv(csv_file, skipcol = 1)
  expect_identical(result, correct_result)

  result2 <- read_ts_csv(csv_file, skipcol = 1, labels = "after")
  expect_identical(result2, correct_result_labels)

  correct_result_labels2 <- correct_result[ , "b", drop = FALSE]
  colnames(correct_result_labels2) <- "(EUR)"
  correct_result_labels2[, "Model Taxus ran for the CEP 2017"] <- NA
  correct_result_labels2 <- correct_result_labels2[, c(2,1)]
  ts_labels(correct_result_labels2) <- c("", "b Timeseries b")

  result3 <- read_ts_csv(csv_file, skipcol = 1, labels = "before")
  expect_identical(result3, correct_result_labels2)
})

test_that("example3.csv is read correctly",  {

  csv_file <- "csv/example3.csv"

  correct_result_tmp <- correct_result
  correct_result_tmp["2010Q2", "a"] <- 1.25
  correct_result_tmp["2010Q2", "b"] <- NA

  msg <- paste("NAs introduced by coercion",
               "The following texts could not be converted to numeric:\n",
               "\"x,jan\"")
  expect_warning({
    result <- read_ts_csv(csv_file, skipcol = 1, dec = ",")
  }, msg = msg)
  expect_identical(result, correct_result_tmp)
})

test_that("example4.csv is read correctly",  {

  csv_file <- "csv/example4.csv"

  correct_result_tmp <- correct_result
  correct_result_tmp["2011Q1", "a"] <- NA
  correct_result_tmp["2011Q2", "a"] <- 6.25

  msg <- paste("NAs introduced by coercion",
               "The following texts could not be converted to numeric:\n",
               "\"aap,x\"")
  expect_warning({
    result <- read_ts_csv(csv_file, skiprow = 1, dec = ",")
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
  result <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result, correct_result_labels["2010Q2", "a", drop = FALSE])
})

test_that("comment rows are skipped (rowwise7.csv))",  {
  csv_file <- "csv/rowwise7.csv"
  result <- read_ts_csv(csv_file)
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
  result <- read_ts_csv(csv_file, labels = "after")
  expect_identical(result, correct_result_labels["2010Q2", ])
})

test_that("columnwise7.csv is read correctly",  {
  csv_file <- "csv/columnwise7.csv"
  result <- read_ts_csv(csv_file)
  expect_identical(result, correct_result["2010Q2", "a", drop = FALSE])
})

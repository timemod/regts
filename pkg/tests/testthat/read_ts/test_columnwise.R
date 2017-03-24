library(regts)
library(testthat)

context("read_ts for columnwise files (no labels)")

# construct correct result
period <- regperiod_range("2010Q2/2011Q2")
a <- regts(as.integer(c(1, NA, NA, 5, 6)), period = period)
b <- 10L * a
correct_result <- cbind(a, b)

test_that("columnwise1.csv is read correctly",  {

    csv_file <- "csv/columnwise1.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
    result <- read_ts(df)
	expect_identical(result, correct_result)

    df2 <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
               row.names = 1)
    result2 <- read_ts(df2)
	expect_identical(result2, correct_result)
})

test_that("columnwise2.csv is read correctly",  {

    csv_file <- "csv/columnwise2.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
    result <- read_ts(df)
	expect_identical(result, correct_result)
})

test_that("columnwise3.csv is read correctly",  {

    csv_file <- "csv/columnwise3.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
    result <- read_ts(df)
	expect_equal(result, correct_result)
	expect_identical(result, correct_result * 1)

    correct_result_labels <- correct_result * 1
    ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b")

    result2 <- read_ts(df, labels = "after")
	expect_identical(result2, correct_result_labels)

	df3 <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
                   row.names = 1)
    result3 <- read_ts(df3, labels = "no")
    result4 <- read_ts(df3, labels = "after")

	expect_identical(result3, correct_result * 1)
	expect_identical(result4, correct_result_labels)
})

test_that("columnwise4.csv is read correctly",  {

    csv_file <- "csv/columnwise4.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
    result <- read_ts(df)
	expect_equal(result, correct_result)
	expect_identical(result, correct_result * 1)

    correct_result_labels <- correct_result * 1
    ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

    result2 <- read_ts(df, labels = "after")
	expect_identical(result2, correct_result_labels)

	df3 <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
                   row.names = 1)
    result3 <- read_ts(df3, labels = "no")
    result4 <- read_ts(df3, labels = "after")

	expect_identical(result3, correct_result * 1)
	expect_identical(result4, correct_result_labels)
})

test_that("columnwise5.csv is read correctly",  {

    csv_file <- "csv/columnwise5.csv"

    correct_result_labels <- correct_result * 1
    ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)

    result <- read_ts(df, columnwise = TRUE)
	expect_identical(result[, 1:2], correct_result * 1)

    result2 <- read_ts(df, columnwise = TRUE, labels = "after")
	expect_identical(result2[, 1:2], correct_result_labels)

    result3 <- read_ts(df, frequency = 4)
	expect_identical(result3[, 1:2], correct_result * 1)

    result4 <- read_ts(df, frequency = 4, labels = "after")
	expect_identical(result4[, 1:2], correct_result_labels)
})

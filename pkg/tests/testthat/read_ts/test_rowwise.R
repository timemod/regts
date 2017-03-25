library(regts)
library(testthat)

context("read_ts for rowwise files (no labels)")

# construct correct result
period <- regperiod_range("2010Q2/2011Q2")
a <- regts(as.integer(c(1, NA, NA, 5, 6)), period = period)
b <- 10L * a
correct_result <- cbind(a, b)

test_that("rowwise1.csv is read correctly",  {

	csv_file <- "csv/rowwise1.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
	result <- read_ts(df)
	expect_identical(result, correct_result)

	df2 <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
                       row.names = 1)
	result2 <- read_ts(df2)
	expect_identical(result2, correct_result)

	df3 <- read.csv(csv_file, header = FALSE, stringsAsFactors = FALSE)
	result3 <- read_ts(df3, use_colnames = FALSE)
	expect_identical(result3, correct_result * 1)
})

test_that("rowwise2.csv is read correctly",  {

  # create a version with the correct labels
  correct_result_labels <- correct_result
  ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b (EUR)")

	csv_file <- "csv/rowwise2.csv"

	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
	result <- read_ts(df)
	expect_identical(result, correct_result)

	result2 <- read_ts(df, labels = "after")
	expect_identical(result2, correct_result_labels)

	df3 <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
                    row.names = 1)
	result3 <- read_ts(df, labels = "after")
	expect_identical(result3, correct_result_labels)

	expect_error(read_ts(df3, labels = "before"),
	             paste("Label option 'before' is not allowed if the row",
	                   "names are not numbered"))

	df4 <- read.csv(csv_file, header = FALSE, stringsAsFactors = FALSE)
	result4 <- read_ts(df4, use_colnames = FALSE)
	expect_identical(result4, correct_result * 1)

	result5 <- read_ts(df4, labels = "after", use_colnames = FALSE)
	expect_identical(result5, correct_result_labels * 1)


})

test_that("rowwise3.csv is read correctly",  {

    # create a version with the correct labels
    correct_result_labels <- correct_result
    ts_labels(correct_result_labels) <- c("Timeseries a", "Timeseries b")

	csv_file <- "csv/rowwise3.csv"
	df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)
	result <- read_ts(df)
    correct_result_tmp <- correct_result
    colnames(correct_result_tmp) <- c("Timeseries a", "Timeseries b")
	expect_identical(result, correct_result_tmp)

	result2 <- read_ts(df, labels = "before")
	expect_identical(result2, correct_result_labels)
})

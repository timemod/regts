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
})


context("head & tail")

test_that("head for regts / df", {

	data <- regts(matrix(1:200, ncol = 20), start = "2010Q2",
                  names = paste0("abc", 1:20))
	df <- as.data.frame(data)

	expect_identical(head(data), head(df[, 1:10]))
	expect_identical(head(data, method = "last"), head(df[, 11:20]))
	expect_identical(head(data, size = 6), head(df[, 1:6]))
	expect_identical(head(data, size = 6, method = "last"), head(df[, 15:20]))
})

test_that("tail for regts / df", {

	data <- regts(matrix(1:200, ncol = 20), start = "2010Q2",
	              names = paste0("abc", 1:20))
	df <- as.data.frame(data)

	expect_identical(tail(data), tail(df[, 1:10]))
	expect_identical(tail(data, method = "last"), tail(df[, 11:20]))
	expect_identical(tail(data, size = 6), tail(df[, 1:6]))
	expect_identical(tail(data, size = 6, method = "last"), tail(df[, 15:20]))
})

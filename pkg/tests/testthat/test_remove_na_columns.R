context("remove_na_columns")

test_that("univariate timeseries", {
  ts1 <- regts(c(NA,1,3,NA,4,8), start = "2000")
	ts2 <- regts(c(0,1,3,9,4,8), start = "2000")
	nl <- remove_na_columns(ts1)

  expect_identical(remove_na_columns(ts1), NULL)

  expect_identical(remove_na_columns(ts2), ts2)
})

test_that("multivariate timeseries, is_na = all or any", {
  dataNA <- matrix(rep(NA, 15), ncol = 3)
  rtsNA  <- regts(dataNA, start = "2017Q1", names = c("a", "b", "c"))
	expect_identical(remove_na_columns(rtsNA), NULL)
	expect_identical(remove_na_columns(rtsNA, "any"), remove_na_columns(rtsNA))

	data <- matrix(c(1,3, 5, NA, NA, NA,3,7,9), ncol = 3)
  rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
  expect_identical(remove_na_columns(rts, "any"), remove_na_columns(rts))
	expect_identical(remove_na_columns(rts), rts[, c("a", "c")])
	rts[, "d"] <- NA
	expect_identical(remove_na_columns(rts), rts[, c("a", "c")])
	rts["2010Q3", ] <- NA
	expect_identical(remove_na_columns(rts, "any"), NULL)
})

test_that("errors", {
  msg <- "Argument x is not a \\(multivariate\\) timeseries"
  expect_error(remove_na_columns("aap"), msg)
})

test_that("timeseries with zero columns", {
  x1 <- regts(matrix(0, nr = 3, nc = 0), period = "2000/2003")
  expect_equal(remove_na_columns(x1), x1)
})



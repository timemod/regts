library(testthat)
library(regts)

test_that("head, tail & topleft for regts", {

  data <- regts(matrix(1:200, ncol = 20), start = "2010Q1",
                names = paste0("abc", 1:20))
  data2 <- as.regts(as.data.frame(data))
  period1 <- period_range("2010Q1/2011Q2")
  period2 <- period_range("2011Q1/2012Q2")

  expect_identical(head(data), head(data2))
  expect_identical(head(data), tail(data[period1, ]))
  expect_identical(head(data[period2, ]), tail(data))

  expect_identical(topleft(data), head(data[, 1:10]))
  expect_identical(topleft(data), tail(data[period1, 1:10]))
  expect_identical(topleft(data, ncol = 6), head(data[, 1:6]))
  expect_identical(topleft(data, ncol = 5), topleft(data2, ncol = 5))
})

test_that("topleft for small regts", {
  data <- regts(matrix(1:12, ncol = 3), start = "2010",
                names = paste0("a", 1:3))
  expect_identical(topleft(data), head(data))
})

test_that("topleft handles empty regts properly", {
  data <- regts(matrix(1:10, ncol = 1, nrow = 10), names = "a", start = "2010")
  data <- data[, character(0), drop = FALSE]
  expect_equal(ncol(data), 0)
  expect_error(
    data_tl <- topleft(data),
    NA
  )
  expect_equal(data_tl, data["2010/2015"])
})

test_that("errors", {
  expect_error(topleft(2), "Argument x is not a timeseries")

  data <- regts(1:3, start = "2010")
  expect_error(topleft(data), "Argument x is not a matrix timeseries")
})


test_that("topleft for normal ts", {

  data <- regts(matrix(1:200, ncol = 20), start = "2010Q1",
                names = paste0("abc", 1:20))
  data_ts <- as.ts(data)

  expect_identical(
    topleft(data_ts),
    as.regts(data_ts)["2010Q1/2011Q2", 1:10]
  )
})

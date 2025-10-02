library(regts)
library(testthat)

rm(list = ls())

test_that("univariate timeseries", {
  x <- regts(1:10, start = "2018q1")

  expect_identical(x[["2018q2/2018q4"]], 2:4)
  expect_identical(x[["2017Q1"]], NA_integer_)
  expect_identical(x[["2017Q4/2018Q1"]], c(NA_integer_, 1L))

  expect_identical(x[[20]], NA_integer_)
  expect_identical(x[[c(20, 30)]], rep(NA_integer_, 2))
  expect_identical(x[[2]], 2L)
  expect_identical(x[[2:3]], c(2L, 3L))

  expect_identical(x[[]], 1:10)

  x_lbls <- x
  ts_labels(x_lbls) <- "jan"
  expect_identical(x_lbls[["2018q2/2018q4"]], 2:4)
  expect_identical(x_lbls[[c(2, 4)]], c(2L, 4L))

  x_char <- regts(c("aap", "noot", "mies"), start = "2018q1")
  expect_identical(x_char[["2018q1"]], "aap")
})


test_that("multivariate timeseries", {

  a <- regts(1:10, start = "2018q1")
  b <- 2 * a
  x <- cbind(a, b)

  m <- as_matrix(x)
  rownames(m) <- NULL

  expect_identical(x[["2018q2/2018q4"]], m[2:4, ])

  expect_identical(x[["2018q2/2018q4", "b"]], m[2:4, 2])
  expect_identical(x[["2018q2/2018q4", "b", drop = FALSE]],
                   m[2:4, 2, drop = FALSE])

  expect_identical(x[["2017Q1"]],
                   matrix(c(NA_real_, NA_real_), ncol = 2,
                          dimnames = list(NULL, c("a", "b"))))

  expect_identical(x[["2017Q4/2018Q1"]],
                   matrix(c(NA_real_, 1, NA_real_, 2), ncol = 2,
                          dimnames = list(NULL, c("a", "b"))))


  expect_identical(x[[20]], 20)
  expect_identical(x[[c(20, 30)]], c(20, NA_real_))
  expect_identical(x[[2]], 2)
  expect_identical(x[[2:3]], c(2, 3))

  expect_identical(x[[]], m)

  x_lbls <- x
  ts_labels(x_lbls) <- c("var a", "var b")
  expect_identical(x_lbls[["2018q2/2018q4"]], m[2:4, ])

  x_char <- regts(matrix(c("aap", "noot", "mies", "piet"), ncol = 2),
                  start = "2018q1")
  expect_identical(x_char[["2018q1"]], matrix(c("aap", "mies"), ncol = 2))
})

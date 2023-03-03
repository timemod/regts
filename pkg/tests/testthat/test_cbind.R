library(regts)
library(testthat)

rm(list = ls())


source("utils/univec2unimat.R")

test_that("univariate timeseries", {

  a <- regts(1:5, start = "2010Q2")
  b <- regts(matrix(11:15, nc = 1), start = "2011Q1")

  expect_identical(colnames(cbind(a, b)), c("a", "b"))
  expect_identical(colnames(cbind(x = a, y = b)), c("x", "y"))
  expect_identical(colnames(cbind(x = a, b)), c("x", "b"))
  expect_identical(cbind(a, b), as.regts(ts.union(a, b)))
  expect_identical(cbind(a, b, union = FALSE),
                   as.regts(ts.intersect(a, b)))

  expected_result <- univec2unimat(a, "a")
  expect_identical(cbind(a, NULL), expected_result)
  expect_identical(cbind(NULL, a), expected_result)

  colnames(b) <- "var_b"
  ref <- as.regts(ts.union(a, b))
  colnames(ref)[2] <- "var_b"
  expect_identical(cbind(a, b), ref)
  expect_identical(as.regts(list(a = a, b = b)), ref)

  ref <- as.regts(ts.intersect(a, a))
  colnames(ref) <- c("a", "a_2")
  expect_identical(cbind(a, a, suffixes = c("", "_2")), ref)
  expect_identical(as.regts(list(a = a, a_2 = a)), ref)

  expect_identical(cbind(a, NULL, a, suffixes = c("", "", "_2")), ref)

  zerocol <- regts(matrix(0, ncol =0, nrow = length(a)),
                   period = get_period_range(a))
  expect_identical(cbind(a, zerocol, NULL, a, suffixes = c("", "", "", "_2")), ref)

  ref <- as.regts(ts.intersect(a, a))
  colnames(ref) <- c("a", "a_copy")
  expect_identical(cbind(a, a, suffixes = c("", "_copy")), ref)

  expect_error(cbind(a, a, a),
               "Duplicate column names \\(a\\). Specify argument suffixes.")
  expect_error(cbind(a, a, suffixes = "a"),
               paste("Length of argument 'suffixes' is smaller than the number",
                     "of objects to be joined \\(2\\)"))
})


test_that("univariate timeseries and vector", {
  a <- regts(1:5, start = "2010Q2")
  vec <- 21:25
  expect_identical(cbind(a, vec), as.regts(ts.union(a, vec)))
  expected_result <- as.regts(ts.intersect(a, vec))
  expect_identical(cbind(a, vec, union = FALSE), expected_result)
  expect_identical(as.regts(list(a = a, vec = vec), union = FALSE),
                   expected_result)
})

test_that("univariate timeseries and matrix", {
  a <- regts(1:5, start = "2010Q2")
  ts_labels(a) <- "Timeseries a"
  m <- matrix(21:30, nc = 2)
  ref <- as.regts(ts.union(a, m))
  colnames(ref)[2:3] <- c("m.1", "m.2")
  ts_labels(ref) <- c("Timeseries a", "", "")
  expect_identical(cbind(a, m), ref)

  m <- matrix(21:22, nc = 2)
  colnames(m) <- c("x", "y")
  b <- regts(11:15, start = "2011Q1")
  ref <- as.regts(ts.intersect(a, b, m))
  colnames(ref)[3:4] <- c("x", "y")
  ts_labels(ref) <- c("Timeseries a", "",  "", "")
  expect_identical(cbind(a, b, m, union = FALSE), ref)
  expect_identical(as.regts(list(a = a, b = b, m = m), union = FALSE), ref)
})

test_that("multivariate timeseries", {
  labels1 <- c("Var a", "Var b")
  regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010Q2",
                  names = c("a", "b"), labels = labels1)
  labels2 <- c("Var c", "Var d")
  regts2 <- regts(matrix(rnorm(10), ncol = 2), start = "2011Q2",
                  names = c("c", "d"), labels = labels2)
  ref <- as.regts(ts.union(regts1, regts2))
  colnames(ref) <- c(colnames(regts1), colnames(regts2))
  ts_labels(ref) <- c(labels1, labels2)
  expect_identical(cbind(regts1, regts2), ref)
  expect_identical(as.regts(list(regts1, regts2)), ref)

  expect_identical(cbind(regts1, NULL, regts2), ref)
  expect_identical(cbind(regts1, NULL), regts1)
  expect_identical(cbind(NULL, regts1), regts1)

  # test zero columns
  expect_identical(cbind(regts1[ , character(0)], regts1), regts1)

  expect_identical(cbind(regts1, NULL, regts1[ , character(0)], regts2), ref)
  expect_identical(cbind(regts2[ , character(0)], regts1,
                         regts1[ , character(0)], regts2), ref)

  p_union <- range_union(get_period_range(regts1), get_period_range(regts2))
  expected_result <- regts2[p_union]

  expect_identical(cbind(regts2, regts1[ , character(0)]), expected_result)
  expect_identical(as.regts(list(regts2, regts1[ , character(0)])),
                   expected_result)

  expect_identical(cbind(regts1[ , character(0)], regts2), expected_result)
  expect_identical(as.regts(list(regts1[ , character(0)], regts2)),
                   expected_result)

  expected_result_2 <-  expected_result[, "c", drop = FALSE]
  expect_identical(cbind(c = regts2$c, regts1[ , character(0)]),
                   expected_result_2)
  expect_identical(as.regts(list(c = regts2$c, regts1[ , character(0)])),
                   expected_result_2)

  expected_result_3 <-  expected_result[, "c", drop = FALSE]
  colnames(expected_result_3) <- "regts2$c"
  expect_identical(cbind(regts2$c, regts1[ , character(0)]), expected_result_3)

  expected_result_4 <- regts2[p_union , "c", drop = FALSE]
  expect_identical(cbind(regts2[ , "c", drop = FALSE],
                         regts1[ , character(0)]), expected_result_4)
  expect_identical(as.regts(list(regts2[ , "c", drop = FALSE],
                         regts1[ , character(0)])), expected_result_4)

  expected_result <- regts2[p_union , character(0)]
  ts_labels(expected_result) <- NULL
  dimnames(expected_result) <- list(NULL, NULL)
  expect_identical(cbind(regts2[ , character(0), drop = FALSE],
                         regts1[ , character(0)]),
                         expected_result)
  expect_identical(as.regts(list(regts2[ , character(0), drop = FALSE],
                                 regts1[ , character(0)])), expected_result)

  expect_identical(cbind(regts2[ , character(0), drop = FALSE]),
                   cbind(regts2[ , character(0), drop = FALSE]))

  expected_result_5 <- regts(2, period = get_period_range(regts2))
  expected_result_5 <- univec2unimat(expected_result_5, "x_2")
  expect_equal(cbind(regts2[ , character(0), drop = FALSE], 2),
               expected_result_5)

  expected_result_6 <- univec2unimat(regts(2, period = get_period_range(regts2)),
                                     "x_1")
  expect_equal(cbind(2, regts2[ , character(0), drop = FALSE]),
               expected_result_6)

  expected_result_7 <- regts(matrix(1:10, ncol = 2), period = get_period_range(regts2),
                             names = paste0("matrix(1:10, ncol = 2).", 1:2))
  expect_equal(cbind(matrix(1:10, ncol=2), regts2[ , character(0), drop = FALSE]),
               expected_result_7)
})

test_that("multivariate timeseries without labels", {
  regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010Q2",
                  names = c("a", "b"))
  regts2 <- regts(matrix(rnorm(10), ncol = 2), start = "2011Q2",
                  names = c("c", "d"))
  ref <- as.regts(ts.union(regts1, regts2))
  colnames(ref) <- c(colnames(regts1), colnames(regts2))
  expect_identical(cbind(regts1, regts2), ref)

  expect_identical(cbind(regts1, NULL), regts1)
  expect_identical(as.regts(list(regts1, NULL)), regts1)

  # zero columns
  p_int <- range_intersect(get_period_range(regts1), get_period_range(regts2))
  expect_identical(cbind(regts2, regts1[ , character(0)], union = FALSE),
                   regts2[p_int])
  expect_identical(as.regts(list(regts2, regts1[ , character(0)]), union = FALSE),
                   regts2[p_int])
})


test_that("multivariate timeseries without colnames", {
  labels1 <- c("Var a", "Var b")
  regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010Q2",
                  labels = labels1)
  labels2 <- c("Var c", "Var d")
  regts2 <- regts(matrix(rnorm(10), ncol = 2), start = "2011Q2",
                  labels = labels2)

  ref <- as.regts(ts.union(regts1, regts2))
  colnames(ref) <- c("regts1.1", "regts1.2", "regts2.1", "regts2.2")
  ts_labels(ref) <- c(labels1, labels2)
  expect_identical(cbind(regts1, regts2), ref)
  expect_identical(as.regts(list(regts1 = regts1, regts2 = regts2)), ref)

  ref2 <- ref
  colnames(ref2) <- c("a.1", "a.2", "regts2.1", "regts2.2")
  ts_labels(ref2) <- ts_labels(ref)

  expect_identical(cbind(a = regts1, regts2), ref2)
  expect_identical(as.regts(list(a = regts1, regts2 = regts2)), ref2)

  ref3 <- regts1
  colnames(ref3) <- paste("regts1", 1:2, sep = ".")
  expect_identical(cbind(regts1, NULL), ref3)
})

test_that("univariate timeseries with labels", {
  a <- regts(1:5, start = "2010Q2", labels = "ts a")
  expected_result <- univec2unimat(a, "a")
  expect_equal(cbind(a, NULL), expected_result)
  expect_equal(as.regts(list(a = a, NULL)), expected_result)

  expected_result_2 <- expected_result
  colnames(expected_result_2) <- "list(a, NULL).1"
  expect_equal(as.regts(list(a, NULL)), expected_result_2)

})

library(regts)
library(testthat)

rm(list = ls())

context("tsdif")

# prepare input data
ts1 <- regts(matrix(data = rep(1:9), nc = 3), start = "2008Q4",
             names = c("a", "b", "c"))
ts2 <- ts1 + 0.01
colnames(ts2) <- c("a", "b", "d")
ts2["2008Q3", ] <- 2
ts1["2009Q3", ] <- 2

difference <- regts(matrix(data = rep(-0.01, 6), nc = 2), start = "2008Q4",
                    names = c("a", "b"))

create_tsdif <- function(...) {
  return(structure(list(...), class = "tsdif"))
}

res_correct <- create_tsdif(equal = FALSE, difnames = c("a", "b"),
                            dif = difference, common_names = c("a", "b"),
                            missing_names1 = "d",  missing_names2 = "c",
                            common_range = period_range("2008Q4/2009Q2"),
                            period_range1 = get_period_range(ts1),
                            period_range2 = get_period_range(ts2),
                            ranges_equal = FALSE, ts_names = c("ts1", "ts2"),
                            tol = 0, fun = NULL)

test_that("simple example", {
  res <- tsdif(ts1, ts2)
  expect_equal(res, res_correct)
})

test_that("argument fun", {
  res2 <- tsdif(ts1, ts2, fun = function(x1, x2) (sin(x2 - x1)))
  res2_correct <- res_correct
  res2_correct$fun <- "function(x1, x2) (sin(x2 - x1))"
  res2_correct$dif <- sin(-res2_correct$dif)
  expect_equal(res2, res2_correct)
})

test_that("check simple output", {
  expect_known_output(tsdif(ts1, ts2), "expected_output/tsdif.txt",
                      print = TRUE)
})

test_that("no difference", {
  res <- tsdif(ts1, ts1)
  res_no_dif <- create_tsdif(equal = TRUE, difnames = character(0),
                             dif = NULL,
                             common_names = c("a", "b", "c"),
                             missing_names1 = character(0),
                             missing_names2 = character(0),
                             common_range  = get_period_range(ts1),
                             period_range1 = get_period_range(ts1),
                             period_range2 = get_period_range(ts1),
                             ranges_equal = TRUE,
                             ts_names = c("ts1", "ts1"),
                             tol = 0, fun = NULL)
  expect_equal(res, res_no_dif)
  expect_known_output(res, "expected_output/tsdif_no_dif.txt",
                      print = TRUE)
})

test_that("errors", {

  msg <- "Argument tol should be >= 0"
  expect_error(tsdif(ts1, ts1, tol = -1),  msg)

  # univariate timeseries
  expect_error(tsdif(ts1[, "a"], ts2[, "a"]),
               "Argument x1 \\(ts1\\[, \"a\"\\]\\) is not a multivariate timeseries")
  expect_error(tsdif(ts1, ts2[, "a", drop = FALSE]),
               "Argument x2 \\(ts2\\[, \"a\", drop = FALSE\\]\\) is not a multivariate timeseries")

  # different frequencies
  tsy <- regts(matrix(data = rep(1:9), nc = 3), start = "2008",
               names = c("a", "b", "c"))
  expect_error(tsdif(ts1, tsy),
               "Timeseries x1 and x2 \\(ts1 and tsy\\) have different frequencies")
})

test_that("only one NA difference", {
  ts1_NA <- ts1
  ts1_NA[2, 2] <- NA
  res <- tsdif(ts1, ts1_NA, tol = 0)
  res_no_dif <- create_tsdif(equal = FALSE, difnames = "b",
                             dif = zero_trim((ts1 - ts1_NA)[, "b", drop = FALSE]),
                             common_names = c("a", "b", "c"),
                             missing_names1 = character(0),
                             missing_names2 = character(0),
                             common_range  = get_period_range(ts1),
                             period_range1 = get_period_range(ts1),
                             period_range2 = get_period_range(ts1),
                             ranges_equal = TRUE,
                             ts_names = c("ts1", "ts1_NA"),
                             tol = 0, fun = NULL)
  expect_equal(res, res_no_dif)
})

test_that("two NA differences", {
  ts1_NA <- ts1
  ts1_NA[2, 2] <- NA
  ts1_NA[4,3] <- NA
  res <- tsdif(ts1, ts1_NA, tol = 0)
  res_no_dif <- create_tsdif(equal = FALSE, difnames = c("b", "c"),
                             dif = zero_trim((ts1 - ts1_NA)[, -1]),
                             common_names = c("a", "b", "c"),
                             missing_names1 = character(0),
                             missing_names2 = character(0),
                             common_range  = get_period_range(ts1),
                             period_range1 = get_period_range(ts1),
                             period_range2 = get_period_range(ts1),
                             ranges_equal = TRUE,
                             ts_names = c("ts1", "ts1_NA"),
                             tol = 0, fun = NULL)
  expect_equal(res, res_no_dif)
})


test_that("differences smaller than tol", {

  res <- tsdif(ts1, ts2,  tol = 0.1)
  res_tol <- res_correct
  res_tol$tol <- 0.1
  res_tol["dif"] <- list(NULL)
  res_tol$difnames <- character()
  expect_equal(res, res_tol)

  # now use the sample without NA values, there should be no differences
  sample <- period_range("2008Q4", "2009Q2")
  res2 <- tsdif(ts1[sample, ], ts2[sample,  ],  tol = 0.1)
  res2_tol <- res_tol
  res2_tol$difnames <- character(0)
  res2_tol["dif"] <- list(NULL)
  res2_tol$common_range <- sample
  res2_tol$period_range1 <- sample
  res2_tol$period_range2 <- sample
  res2_tol$ranges_equal <- TRUE
  res2_tol$ts_names <- c("ts1[sample, ]", "ts2[sample, ]")
  expect_equal(res2, res2_tol)
})

test_that("single period", {
  res <- tsdif(ts1['2008Q4', ], ts2['2008Q4', ])
  res_correct2 <- res_correct
  res_correct2$dif <- difference['2008Q4', ]
  res_correct2$common_range <- period_range("2008Q4")
  res_correct2$period_range1 <- period_range("2008Q4")
  res_correct2$period_range2 <- period_range("2008Q4")
  res_correct2$ranges_equal <- TRUE
  res_correct2$ts_names <- c("ts1[\"2008Q4\", ]", "ts2[\"2008Q4\", ]")
  expect_equal(res, res_correct2)
})

test_that("no common columns", {
  x2 <- ts2
  colnames(x2) <- toupper(colnames(ts2))
  msg <- "Timeseries ts1 and x2 have no common columns"
  expect_warning(res <- tsdif(ts1, x2), msg)
  res_correct2 <- create_tsdif(equal = FALSE, difnames = character(0),
                               dif = NULL,
                               common_names = character(0),
                               missing_names1 = c("A", "B", "D"),
                               missing_names2 = c("a", "b", "c"),
                               common_range = period_range("2008Q4/2009Q2"),
                               period_range1 = get_period_range(ts1),
                               period_range2 = get_period_range(x2),
                               ranges_equal = FALSE,
                               ts_names = c("ts1", "x2"),
                               tol = 0, fun = NULL)
  expect_equal(res, res_correct2)
})


test_that("single ts as result", {
  sample <- period_range("2008Q4", "2009Q2")
  x2 <- ts2[sample, ]
  x2[sample, 'b'] <- ts1[sample, 'b'] + 0.11
  res <- tsdif(ts1[sample, ], x2,  tol = 0.1)

  res_correct2 <- res_correct
  res_correct2$tol <- 0.1
  res_correct2$difnames <- 'b'
  res_correct2$dif <- regts(matrix(data = rep(-0.11, 3), nc = 1), start = "2008Q4",
                            names = c("b"))
  res_correct2$ts_names <- c("ts1[sample, ]", "x2")
  res_correct2$period_range1 <- sample
  res_correct2$period_range2 <- sample
  res_correct2$ranges_equal <- TRUE

  expect_equal(res, res_correct2)
})

test_that("single common column", {
  res <- tsdif(ts1[, c("a", "c")], ts2[, c("d", "a")])
  res_correct2 <- create_tsdif(equal = FALSE, difnames = c("a"),
                               dif = regts(matrix(data = rep(-0.01, 3), nc = 1),
                                           start = "2008Q4", names = c("a")),
                               common_names = "a",
                               missing_names1 = "d",  missing_names2 = "c",
                               common_range = period_range("2008Q4/2009Q2"),
                               period_range1 = get_period_range(ts1),
                               period_range2 = get_period_range(ts2),
                               ranges_equal = FALSE,
                               ts_names = c("ts1[, c(\"a\", \"c\")]",
                                            "ts2[, c(\"d\", \"a\")]"),
                               tol = 0, fun = NULL)
  expect_equal(res, res_correct2)
})

test_that("no column names simple", {
  x <- ts1
  y <- ts2[, 1:2]
  colnames(x) <- NULL
  colnames(y) <- NULL
  res <- tsdif(x, y)

  difference3 <- difference[, 1:2]
  colnames(difference3) <- c("column 1", "column 2")
  res_correct3 <- create_tsdif(equal = FALSE,
                               difnames = paste("column", 1:2),
                               dif = difference3,
                               common_names = paste("column", 1:2),
                               missing_names1 = character(0),
                               missing_names2 = "column 3",
                               common_range  = period_range("2008Q4", "2009Q2"),
                               period_range1 = get_period_range(x),
                               period_range2 = get_period_range(y),
                               ranges_equal  = FALSE,
                               ts_names = c("x", "y"),
                               tol = 0, fun = NULL)
  expect_equal(res, res_correct3)
})


test_that("no overlapping periods", {
  msg <- "Timeseries ts1[\"2008Q4/2009Q1\", ] and ts2[\"2009Q2\", ] have no common period range"
  msg <- Hmisc::escapeRegex(msg)
  expect_warning(res <- tsdif(ts1['2008Q4/2009Q1', ], ts2['2009Q2', ]), msg)
  res_correct2 <- res_correct
  res_correct2["dif"] <- list(NULL)
  res_correct2$difnames <- character(0)
  res_correct2["common_range"] <- list(NULL)
  res_correct2$period_range1 <- period_range("2008Q4", "2009Q1")
  res_correct2$period_range2 <- period_range("2009Q2")
  res_correct2$ranges_equal <- FALSE
  res_correct2$ts_names <- c("ts1[\"2008Q4/2009Q1\", ]", "ts2[\"2009Q2\", ]")
  expect_equal(res, res_correct2)
})

test_that("a combination of negative and positive differences", {
  ts1 <- regts(matrix(data = rep(1:9), nc = 3), start = "2008Q4",
               names = c("a", "b", "c"))
  ts2 <- ts1
  ts2[, "a"] <- ts1[, "a"] + c(-0.2, 0, 0.1)
  dif_correct <- ts2[, "a", drop = FALSE] - ts1[, "a", drop = FALSE]
  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$dif, dif_correct)
  dif2 <- tsdif(ts1, ts2)
  expect_equal(dif2$dif, -dif_correct)
  dif3 <- tsdif(ts2, ts1, tol = 0.15)
  expect_equal(dif3$dif, dif_correct["2008Q4"])
  dif4 <- tsdif(ts1, ts2, tol = 0.15)
  expect_equal(dif4$dif, -dif_correct["2008Q4"])
})

test_that("NA, NaN, Inf and -Inf values", {

  ts1 <- regts(matrix(data = rep(NA, 4), nc = 2), start = "2016",
               names = c("a", "b"))
  ts2 <- ts1 + 0.01
  colnames(ts2) <- c("a", "b")

  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$equal, TRUE)

  tsN1 <- regts(matrix(data = rep(NaN, 4), nc = 2), start = "2016",
                names = c("a", "b"))
  tsN2 <- tsN1 + 0.01
  colnames(tsN2) <- c("a", "b")

  dif1 <- tsdif(tsN2, tsN1)
  expect_equal(dif1$equal, TRUE)

  ts1 <- regts(matrix(data = rep(1:4)/0, nc = 2), start = "2016",
               names = c("a", "b"))
  ts2 <- ts1 + 0.01
  colnames(ts2) <- c("a", "b")

  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$dif, tsN1)

  ts1 <- regts(matrix(data = -rep(1:4)/0, nc = 2), start = "2016",
               names = c("a", "b"))
  ts2 <- ts1 + 0.01

  dif1 <- tsdif(ts2, ts1)
  expect_equal(dif1$dif, tsN1)
})

test_that("test combinations of NA, NaN, Inf and proper values", {

  ts1 <- regts(matrix(data = c(NA, NA, NA, NA, NaN, NaN), nc = 3), start = "2016",
               names = c("a", "b", "c"))
  ts2 <- regts(matrix(data = c(NaN, NaN, rep(1:4)), nc = 3), start = "2016",
               names = c("a", "b", "c"))

  res__dif <- create_tsdif(equal = FALSE, difnames = c("a", "b", "c"),
                           dif = ts1,
                           common_names = c("a", "b", "c"),
                           missing_names1 = character(0),
                           missing_names2 = character(0),
                           common_range  = get_period_range(ts1),
                           period_range1 = get_period_range(ts1),
                           period_range2 = get_period_range(ts1),
                           ranges_equal = TRUE,
                           ts_names = c("ts1", "ts2"),
                           tol = 0, fun = NULL )

  dif1 <- tsdif(ts1, ts2)
  expect_equal(dif1, res__dif )


  tsInf <- regts(matrix(data = c(rep(1/0,2), rep(-1/0,2), rep(1/0,2), rep(-1/0,2)),
                        nc = 2), start = "2016", names = c("a", "b"))
  tsNA  <- regts(matrix(data = c(rep(NA, 4), rep(NaN, 4)),
                        nc = 2), start = "2016", names = c("a", "b"))
  expect_equal(tsdif(tsInf, tsNA)$dif, tsNA)
})

test_that("check more complex output with combinations of NA and proper values", {
  ts1 <- regts(matrix(data = c(1:144), nc = 12), start = "2016q1",
               names = c("a", "b", "c", "d","e","f","g","h","i","j","k","l"))
  ts2 <- (ts1 + 1) + 2 * ts1
  ts1["2016q3", "c"] <- NA
  ts2["2017q4", "f"] <- NA
  expect_known_output(tsdif(ts1, ts2), "expected_output/tsdif_complex.txt",
                      print = TRUE)
})

test_that("function cvgdif", {
  expect_equal(cvgdif(0.12, 0.14), 0.02)
  expect_equal(cvgdif(0.12, -0.14), 0.26)
  expect_equal(cvgdif(-0.12, 8.0), 8.12/8)
  expect_equal(cvgdif(-0.12, -8.0), 7.88/8)
})


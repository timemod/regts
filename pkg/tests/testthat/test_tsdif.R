library(regts)
context("tsdif")

# prepare input data
ts1 <- regts(matrix(data = rep(1:9), nc = 3), start = "2008Q4",
            names = c("a", "b", "c"))
ts2 <- ts1 + 0.01
colnames(ts2) <- c("a", "b", "d")
ts2["2008Q3", ] <- 2
ts1["2009Q3", ] <- 2

difference <- regts(matrix(data = rep(0.01, 6), nc = 2), start = "2008Q4",
                     names = c("a", "b"))

create_tsdif <- function(...) {
  return(structure(list(...), class = "tsdif"))
}

res_correct <- create_tsdif(equal = FALSE, difnames = c("a", "b"), dif = difference,
       missing_names1 = "d",  missing_names2 = "c",
       period_range1 = get_period_range(ts1),
       period_range2 = get_period_range(ts2),
       ranges_equal = FALSE, ts_names = c("ts1", "ts2"), tol = 0, fun = NULL)


test_that("simple example", {
    res <- tsdif(ts1, ts2)
    expect_equal(res, res_correct)
})

test_that("no difference", {
    res <- tsdif(ts1, ts1)
    res_no_dif <- create_tsdif(equal = TRUE, difnames = character(0), dif = NULL,
                       missing_names1 = character(0),
                       missing_names2 = character(0),
                       period_range1 = get_period_range(ts1),
                       period_range2 = get_period_range(ts1),
                       ranges_equal = TRUE,
                       ts_names = c("ts1", "ts1"),
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
    res_correct2$period_range1 <- period_range("2008Q4")
    res_correct2$period_range2 <- period_range("2008Q4")
    res_correct2$ranges_equal <- TRUE
    res_correct2$ts_names <- c("ts1[\"2008Q4\", ]", "ts2[\"2008Q4\", ]")
    expect_equal(res, res_correct2)
})

test_that("no common columns", {
    x2 <- ts2
    colnames(x2) <- toupper(colnames(ts2))
    res <- tsdif(ts1, x2)
    res_correct2 <- create_tsdif(equal = FALSE, difnames = character(0),
                         dif = NULL, missing_names1 = c("A", "B", "D"),
                         missing_names2 = c("a", "b", "c"),
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
    res_correct2$dif <- regts(matrix(data = rep(0.11, 3), nc = 1), start = "2008Q4",
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
                         dif = regts(matrix(data = rep(0.01, 3), nc = 1),
                                     start = "2008Q4", names = c("a")),
                         missing_names1 = "d",  missing_names2 = "c",
                         period_range1 = get_period_range(ts1),
                         period_range2 = get_period_range(ts2),
                         ranges_equal = FALSE,
                         ts_names = c("ts1[, c(\"a\", \"c\")]",
                                      "ts2[, c(\"d\", \"a\")]"),
                         tol = 0, fun = NULL)
    expect_equal(res, res_correct2)
})

test_that("two univariate timeseries", {
    expect_error(tsdif(ts1[, "a"], ts2[, "a"]),
       "Argument x1 \\(ts1\\[, \"a\"\\]\\) is not a multivariate timeseries")
    expect_error(tsdif(ts1, ts2[, "a", drop = FALSE]),
        "Argument x2 \\(ts2\\[, \"a\", drop = FALSE\\]\\) is not a multivariate timeseries")
})

test_that("different frequencies", {
  tsy <- regts(matrix(data = rep(1:9), nc = 3), start = "2008",
               names = c("a", "b", "c"))
  expect_error(tsdif(ts1, tsy),
               "Timeseries x1 and x2 \\(ts1 and tsy\\) have different frequencies")
})

test_that("no column names simple", {
    x <- ts1
    y <- ts2[, 1:2]
    colnames(x) <- NULL
    colnames(y) <- NULL
    res <- tsdif(x, y)

    difference3 <- difference[, 1:2]
    colnames(difference3) <- c("column 1", "column 2")
    res_correct3 <- create_tsdif(equal = FALSE, difnames = c("column 1", "column 2"),
                        dif = difference3, missing_names1 = character(0),
                        missing_names2 = "column 3",
                        period_range1 = get_period_range(x),
                        period_range2 = get_period_range(y),
                        ranges_equal  = FALSE,
                        ts_names = c("x", "y"),
                        tol = 0, fun = NULL)
    expect_equal(res, res_correct3)
})







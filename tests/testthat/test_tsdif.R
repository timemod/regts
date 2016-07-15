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
difference["2008Q3", ] <- NA
difference["2009Q3", ] <- NA

res_correct <- list(tol = 0, missing_names1 = "d",  missing_names2 = "c",
                    equal = FALSE, difnames = c("a", "b"),
                    dif = difference)

test_that("simple example", {
    res <- tsdif(ts1, ts2)
    expect_equal(res, res_correct)
})

test_that("no difference", {
    res <- tsdif(ts1, ts1)
    res_no_dif <- list(tol = 0, missing_names1 = character(0),
                         missing_names2 = character(0),
                         equal = TRUE, difnames = character(0),
                         dif = NULL)
    expect_equal(res, res_no_dif)
})

test_that("differences smaller than tol", {
    res <- tsdif(ts1, ts2,  tol = 0.1)
    res_tol <- res_correct
    res_tol$tol <- 0.1
    # the differences are the same as for tol = 0.0, because the NA values
    expect_equal(res, res_tol)

    # now use the sample without NA values, there should be no differences
    sample <- regperiod_range("2008Q4", "2009Q2")
    res2 <- tsdif(ts1[sample, ], ts2[sample,  ],  tol = 0.1)
    res2_tol <- res_tol
    res2_tol$difnames <- character(0)
    res2_tol["dif"] <- list(NULL)
    expect_equal(res2, res2_tol)
})

test_that("single period", {
    res <- tsdif(ts1['2008Q4', ], ts2['2008Q4', ])
    res_correct2 <- res_correct
    res_correct2$dif <- difference['2008Q4', ]
    expect_equal(res, res_correct2)
})

test_that("no common columns", {
    x2 <- ts2
    colnames(x2) <- toupper(colnames(ts2))
    res <- tsdif(ts1, x2)
    res_correct2 <- list(tol = 0, missing_names1 = c("A", "B", "D"),
                         missing_names2 = c("a", "b", "c"),
                         equal = FALSE, difnames = character(0),
                          dif = NULL)
    expect_equal(res, res_correct2)
})


test_that("single ts as result", {
    sample <- regperiod_range("2008Q4", "2009Q2")
    x2 <- ts2[sample, ]
    x2[sample, 'b'] <- ts1[sample, 'b'] + 0.11
    res <- tsdif(ts1[sample, ], x2,  tol = 0.1)
    res_correct2 <- res_correct
    res_correct2$tol <- 0.1
    res_correct2$difnames <- 'b'
    res_correct2$dif <- regts(matrix(data = rep(0.11, 3), nc = 1), start = "2008Q4",
                              names = c("b"))
    expect_equal(res, res_correct2)
})

test_that("single common column", {
    res <- tsdif(ts1[, c("a", "c")], ts2[, c("d", "a")])
    res_correct2 <- list(tol = 0, missing_names1 = "d",  missing_names2 = "c",
                         equal = FALSE, difnames = c("a"),
                         dif = regts(matrix(data = c(NA, rep(0.01, 3), NA), nc = 1),
                                     start = "2008Q3", names = c("a")))
    expect_equal(res, res_correct2)
})

test_that("two univariate timeseries", {
    res <- tsdif(ts1[, "a"], ts2[, c("a")])
    res_correct2 <- list(tol = 0, missing_names1 = character(0),  missing_names2 = character(0),
                         equal = FALSE, difnames = c("a"),
                         dif = regts(matrix(data = c(NA, rep(0.01, 3), NA), nc = 1),
                                     start = "2008Q3", names = c("a")))
    expect_equal(res, res_correct2)
})








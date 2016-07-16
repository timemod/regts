library(regts)
context("list")

set.seed(12345)

test_that("as.list for univariate timeseries without colnames", {
    labels <- "Var a"
    a <- regts(1:10, start = "2010Q2", labels = labels)
    l1 <- as.list(a)
    expect_identical(l1[[1]], a)
    expect_identical(names(l1), "a")
    expect_identical(lapply(l1, FUN = ts_labels), list(a = labels[1]))

    ref <- a
    ts_labels(ref) <- ts_labels(ref)  # add names
    expect_identical(do.call(cbind, l1), ref)

    # for a list with one element, ts.intersect gives the same result as
    # cbind
    expect_identical(do.call(ts.intersect, l1), a)
})

test_that("as.list for univariate timeseries with colnames", {
    labels <- "Var a"
    regts1 <- regts(1:10, start = "2010Q2",  names = "a", labels = labels)
    l1 <- as.list(regts1)

    expect_identical(names(l1), colnames(regts1))
    ref <- regts1[, 1]
    colnames(ref) <- NULL
    ts_labels(ref) <- ts_labels(ref)
    expect_identical(l1[[1]], ref)

    label_a <- labels[1]
    expect_identical(lapply(l1, FUN = ts_labels), list(a = label_a))
    ref <- regts1
    dim(ref) <- NULL
    names(ts_labels(ref)) <- NULL

    expect_identical(do.call(cbind, l1), ref)

    # for a list with one element, ts.intersect gives the same result as
    # cbind
    expect_identical(do.call(ts.intersect, l1), ref)
})

test_that("as.list for multivariate timeseries", {
    labels <- c("Var a", "Var b")
    regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010Q2",
                    names = c("a", "b"), labels = labels)
    l1 <- as.list(regts1)
    ref <- regts1
    colnames(ref) <- NULL
    names(ts_labels(ref)) <- NULL
    expect_identical(l1[[1]], ref[, 1])
    expect_identical(l1[[2]], ref[, 2])
    expect_identical(names(l1), colnames(regts1))
    label_a <- labels[1]
    label_b <- labels[2]
    expect_identical(lapply(l1, FUN = ts_labels), list(a = label_a, b = label_b))
    expect_identical(do.call(cbind, c(l1, union = FALSE)), regts1)
})

test_that("as.list for multivariate timeseries without colnames and labels", {
    regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010M2")
    l1 <- as.list(regts1)
    ts_names <- paste("regts1", 1:2, sep = ".")
    expect_identical(l1[[1]], regts1[, 1])
    expect_identical(l1[[2]], regts1[, 2])
    expect_identical(names(l1), ts_names)
    ref <- regts1
    colnames(ref) <- ts_names
    expect_identical(do.call(cbind, l1), ref)
})

test_that("usage of within", {
    regts1 <- regts(matrix(1:6, ncol = 2), start = "2015Q3",
                    names = c("a", "b"))
    l <- as.list(regts1)
    l <- within(l, {
        b["2015q4"] <- 0.25
        c <- a * b
        d <- lag(c)
    })
    regts2 <- do.call(cbind, l)
    regts2 <- regts2[, c("a", "b", "c", "d")]

    ref <- regts1
    ref['2015q4', 'b'] <- 0.25
    ref[, 'c'] <- ref[, 'a'] * ref[, 'b']
    ref['2015q2/2015q4', 'd'] <- lag(ref[, 'c'])
    expect_identical(regts2, ref)
})

test_that("usage of within with labels", {
    regts1 <- regts(matrix(1:6, ncol = 2), start = "2015Q3",
                    names = c("a", "b"), labels = c("Var a", "Var b"))
    l <- as.list(regts1)
    l <- within(l, {
        b["2015q4"] <- 0.25
        c <- a * b
        d <- lag(c)
    })
    regts2 <- do.call(cbind, l)
    regts2 <- regts2[, c("a", "b", "c", "d")]

    ref <- regts1
    ref['2015q4', 'b'] <- 0.25
    ref[, 'c'] <- ref[, 'a'] * ref[, 'b']
    ref['2015q2/2015q4', 'd'] <- lag(ref[, 'c'])
    ts_labels(ref) <- c("Var a", "Var b", "Var a", "Var a")
    expect_identical(regts2, ref)
})

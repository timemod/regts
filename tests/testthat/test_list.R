library(regts)
context("list")

set.seed(12345)

test_that("as.list for univariate timeseries", {
    labels <- "Var a"
    regts1 <- regts(1:10, start = "2010Q2",  names = "a", labels = labels)
    l1 <- as.list(regts1)
    expect_identical(l1[[1]], regts1[, 1])
    expect_identical(names(l1), colnames(regts1))
    label_a <- labels[1]
    names(label_a) <- "a"
    expect_identical(lapply(l1, FUN = ts_labels), list(a = label_a))
    expect_identical(do.call(regts.intersect, l1), regts1)

    # for a list with one element, ts.intersect gives the same result as
    # regts.intersect
    expect_identical(do.call(ts.intersect, l1), regts1)
})

test_that("as.list for multivariate timeseries", {
    labels <- c("Var a", "Var b")
    regts1 <- regts(matrix(rnorm(10), ncol = 2), start = "2010Q2",
                    names = c("a", "b"), labels = labels)
    l1 <- as.list(regts1)
    expect_identical(l1[[1]], regts1[, 1])
    expect_identical(l1[[2]], regts1[, 2])
    expect_identical(names(l1), colnames(regts1))
    label_a <- labels[1]
    names(label_a) <- "a"
    label_b <- labels[2]
    names(label_b) <- "b"
    expect_identical(lapply(l1, FUN = ts_labels), list(a = label_a, b = label_b))
    expect_identical(do.call(regts.intersect, l1), regts1)
})

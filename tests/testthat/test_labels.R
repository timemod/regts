context("labels")

test_that("constructor regts for univariate timeseries", {

    regts1 <- regts(1:10, start = "2010Q4", names = "a",
                    labels = "Timeseries a")
    res <- "Timeseries a"
    names(res) <- "a"
    expect_identical(ts_labels(regts1), res)

    regts2 <- update_ts_labels(regts1, list(a = "ts a", x = "???"))
    res2 <- res
    res2['a'] <- "ts a"
    expect_identical(ts_labels(regts1), res)
})

test_that("constructor regts for multivariate timeseries", {

    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"),
                    labels = c("Timeseries a", "Timeseries b"))
    res <- c("Timeseries a", "Timeseries b")
    names(res) <- c("a", "b")
    expect_identical(ts_labels(regts1), res)

    regts2 <- update_ts_labels(regts1, list(a = "ts a", x = "???"))
    res2 <- res
    res2['a'] <- "ts a"
    expect_identical(ts_labels(regts1), res)

    expect_identical(ts_labels(regts1[, 1]), res['a'])
    expect_identical(ts_labels(regts1[, 'b']), res['b'])
    expect_identical(ts_labels(regts1[, c('a', 'b')]), res)
    expect_identical(ts_labels(regts1[, c('b', 'a')]), res[c("b", "a")])
})

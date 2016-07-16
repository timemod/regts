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
    expect_identical(ts_labels(regts2), res2)
})

test_that("constructor regts for univariate character timeseries", {

    regts1 <- regts(paste0("text", as.character(1:10)), start = "2010Q4",
                           names = "a", labels = "Timeseries a")
    res <- "Timeseries a"
    names(res) <- "a"
    expect_identical(ts_labels(regts1), res)

    regts2 <- update_ts_labels(regts1, list(a = "ts a", x = "???"))
    res2 <- res
    res2['a'] <- "ts a"
    expect_identical(ts_labels(regts2), res2)
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

    expect_identical(ts_labels(regts1[, 1, drop = FALSE]), res['a'])
    expect_identical(ts_labels(regts1[, 'b']), unname(res['b']))
    expect_identical(ts_labels(regts1[, c('a', 'b')]), res)
    expect_identical(ts_labels(regts1[, c('b', 'a')]), res[c("b", "a")])
})


test_that("adding a column to a regts", {
    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("a", "b"),
                    labels = c("Timeseries a", "Timeseries b"))
    regts1['2012Q2', 'x'] <- 2

    ref <- c("Timeseries a", "Timeseries b", "")
    names(ref) <- colnames(regts1)

    expect_identical(ts_labels(regts1), ref)

    regts2 <- update_ts_labels(regts1, list(x = "Timeseries x"))
    ref2 <- c("Timeseries a", "Timeseries b", "Timeseries x")
    names(ref2) <- colnames(regts2)
    expect_identical(ts_labels(regts2), ref2)
})

test_that("labels are preserved in miscellaneous timeseries functions", {
    x <- regts(1:10, start = "2010Q4", names = "a", labels = "Timeseries a")
    x_sin <- sin(x)
    x_lag <- lag(x)
    x_diff <- diff(x)
    x_agg <- aggregate(x)
    x_agg_gr <- aggregate_gr(x, method = "cgr")
    x_windows <- window(x, start = c(2011, 4))
    expect_identical(ts_labels(x), ts_labels(x_sin))
    expect_identical(ts_labels(x), ts_labels(x_lag))
    expect_identical(ts_labels(x), ts_labels(x_diff))
    expect_identical(ts_labels(x), ts_labels(x_agg))
    expect_identical(ts_labels(x), ts_labels(x_agg_gr))
    expect_identical(ts_labels(x), ts_labels(x_windows))
})

test_that("column selection in a timeseries with 1 row and labels", {
    # selecting more than one column in a timeseries with 1 row has an odd
    # result for class ts if drop = TRUE. This behaviour is corrected for
    # in regts (see the implementation of "[.regts")

    regts1 <- regts(matrix(1:3, nc = 3), "2010Q2", names = c("a", "b", "c"),
                    labels = c("Var a", "Var b", "Var c"))

    # select two columns
    ref <- regts(matrix(2:3, nc = 2), "2010Q2", names = c("b", "c"),
                 labels = c("Var b", "Var c"))
    expect_identical(regts1[, c("b", "c")], ref)

    # select one column (regts1[, "c"] is a named vector)
    ref <- regts(3, "2010Q2", labels = "Var c")
    names(ref) <- "c"
    expect_equal(regts1[, "c"], ref)
})


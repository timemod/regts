context("join_ts")

test_that("univariate timeseries", {

    a <- regts(1:5, start = "2010Q2")
    b <- regts(11:15, start = "2011Q1")

    expect_identical(join_ts(a, b), as.regts(ts.union(a, b)))
    expect_identical(join_ts(a, b, union = FALSE),
                             as.regts(ts.intersect(a, b)))

    colnames(b) <- "var_b"
    ref <- as.regts(ts.union(a, b))
    colnames(ref)[2] <- "var_b"
    expect_identical(join_ts(a, b), ref)

    ref <- as.regts(ts.intersect(a, a))
    colnames(ref) <- c("a", "a_2")
    expect_identical(join_ts(a, a, suffixes = c("", "_2")), ref)

    expect_error(join_ts(a, a),
                    "Duplicate column names. Specify argument suffixes")
})


test_that("univariate timeseries and vector", {
    a <- regts(1:5, start = "2010Q2")
    vec <- 21:25
    expect_identical(join_ts(a, vec), as.regts(ts.union(a, vec)))
    expect_identical(join_ts(a, vec, union = FALSE),
                     as.regts(ts.intersect(a, vec)))
})

test_that("univariate timeseries and matrix", {
    a <- regts(1:5, start = "2010Q2")
    ts_labels(a) <- "Timeseries a"
    m <- matrix(21:30, nc = 2)
    ref <- as.regts(ts.union(a, m))
    colnames(ref)[2:3] <- c("m_1", "m_2")
    ts_labels(ref) <- c("Timeseries a", "", "")
    expect_identical(join_ts(a, m), ref)

    m <- matrix(21:22, nc = 2)
    colnames(m) <- c("x", "y")
    b <- regts(11:15, start = "2011Q1")
    ref <- as.regts(ts.intersect(a, b, m))
    colnames(ref)[3:4] <- c("x", "y")
    ts_labels(ref) <- c("Timeseries a", "",  "", "")
    expect_identical(join_ts(a, b, m, union = FALSE), ref)
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
    expect_identical(join_ts(regts1, regts2), ref)
})

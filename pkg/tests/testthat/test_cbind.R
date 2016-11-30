context("cbind")

test_that("univariate timeseries", {

    a <- regts(1:5, start = "2010Q2")
    b <- regts(matrix(11:15, nc = 1), start = "2011Q1")

    expect_identical(colnames(cbind(a, b)), c("a", "b"))
    expect_identical(colnames(cbind(x = a, y = b)), c("x", "y"))
    expect_identical(colnames(cbind(x = a, b)), c("x", "b"))
    expect_identical(cbind(a, b), as.regts(ts.union(a, b)))
    expect_identical(cbind(a, b, union = FALSE),
                             as.regts(ts.intersect(a, b)))

    colnames(b) <- "var_b"
    ref <- as.regts(ts.union(a, b))
    colnames(ref)[2] <- "var_b"
    expect_identical(cbind(a, b), ref)

    ref <- as.regts(ts.intersect(a, a))
    colnames(ref) <- c("a", "a_2")
    expect_identical(cbind(a, a, suffixes = c("", "_2")), ref)

    ref <- as.regts(ts.intersect(a, a))
    colnames(ref) <- c("a", "a_copy")
    expect_identical(cbind(a, a, suffixes = c("", "_copy")), ref)

    expect_error(cbind(a, a, a),
                    "Duplicate column names \\(a\\). Specify argument suffixes.")
    expect_error(cbind(a, a, suffixes = "a"),
                 paste("Length of argument suffixes is smaller than the number",
                       "of objects to be joined \\(2\\)"))
})


test_that("univariate timeseries and vector", {
    a <- regts(1:5, start = "2010Q2")
    vec <- 21:25
    expect_identical(cbind(a, vec), as.regts(ts.union(a, vec)))
    expect_identical(cbind(a, vec, union = FALSE),
                     as.regts(ts.intersect(a, vec)))
})

test_that("univariate timeseries and matrix", {
    a <- regts(1:5, start = "2010Q2")
    ts_labels(a) <- "Timeseries a"
    m <- matrix(21:30, nc = 2)
    ref <- as.regts(ts.union(a, m))
    ts_labels(ref) <- c("Timeseries a", "", "")
    expect_identical(cbind(a, m), ref)

    m <- matrix(21:22, nc = 2)
    colnames(m) <- c("x", "y")
    b <- regts(11:15, start = "2011Q1")
    ref <- as.regts(ts.intersect(a, b, m))
    colnames(ref)[3:4] <- c("x", "y")
    ts_labels(ref) <- c("Timeseries a", "",  "", "")
    expect_identical(cbind(a, b, m, union = FALSE), ref)
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

    ref2 <- ref
    colnames(ref2) <- c("a.1", "a.2", "regts2.1", "regts2.2")
    ts_labels(ref2) <- ts_labels(ref)

    expect_identical(cbind(a = regts1, regts2), ref2)

})

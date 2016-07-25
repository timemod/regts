context("Ops.regts")

test_that("Ops.regts for univariate timeseries", {
    regts1 <- regts(as.numeric(1:8), start = "2010Q2", labels = "Var a")
    ts1 <- regts:::unregts(regts1)

    expect_identical(colnames(regts1 + 1), colnames(regts1))
    expect_identical(colnames(1 + regts1), colnames(regts1))
    expect_identical(colnames(regts1 + matrix(1:8, ncol = 1)), colnames(regts1))
    expect_identical(colnames(regts1 + regts1), colnames(regts1))

    expect_identical(ts_labels(regts1 < 1), ts_labels(regts1))
    expect_identical(ts_labels(1 < regts1), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < matrix(1:8, ncol = 1)), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < regts1), ts_labels(regts1))

    expect_identical(regts1 + 1, as.regts(ts1 + 1))
    expect_identical(1 + regts1, as.regts(ts1 + 1))
    expect_identical(1:8 + regts1, 2 * regts1)
    expect_identical(regts1 + 1:8, 2 * regts1)
    expect_identical(regts1 + regts1, 2 * regts1)
})

test_that("Ops.regts for univariate timeseries with colnames", {
    regts1 <- regts(matrix(as.numeric(1:8), nc = 1), start = "2010Q2", name =
                        "a", labels = "Var a")
    ts1 <- regts:::unregts(regts1)

    expect_identical(colnames(regts1 + 1), colnames(regts1))
    expect_identical(colnames(1 + regts1), colnames(regts1))
    expect_identical(colnames(regts1 + matrix(1:8, ncol = 1)), colnames(regts1))
    expect_identical(colnames(regts1 + regts1), colnames(regts1))

    expect_identical(ts_labels(regts1 < 1), ts_labels(regts1))
    expect_identical(ts_labels(1 < regts1), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < matrix(1:8, ncol = 1)), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < regts1), ts_labels(regts1))

    expect_identical(regts1 + 1, as.regts(ts1 + 1))
    expect_identical(regts1 + matrix(1:8, ncol = 1), 2 * regts1)
    expect_identical(regts1 + regts1, 2 * regts1)
})

test_that("Ops.regts for multivariate timeseries", {
    regts1 <- regts(matrix(as.numeric(1:8), ncol = 2), start = "2010Q2",
                    names = c("a", "b"), labels = c("Var a", "Var b"))
    regts2 <- regts1
    ts_labels(regts2) <- NULL
    ts1 <- regts:::unregts(regts1)

    expect_identical(colnames(regts1 / 1), colnames(regts1))
    expect_identical(colnames(1 == regts1), colnames(regts1))
    expect_identical(colnames(regts1 > matrix(1:8, ncol = 2)), colnames(regts1))
    expect_identical(colnames(regts1 %/% regts1), colnames(regts1))

    expect_identical(ts_labels(regts1 < 1), ts_labels(regts1))
    expect_identical(ts_labels(1 < regts1), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < matrix(1:8, ncol = 2)), ts_labels(regts1))
    expect_identical(ts_labels(regts1 < regts1), ts_labels(regts1))
    expect_null(ts_labels(regts2 + 2))
    expect_null(ts_labels(regts2 + regts1))

    expect_identical(regts1 + 1, as.regts(ts1 + 1))
    expect_identical(1 + regts1, as.regts(ts1 + 1))
    expect_identical(1:8 + regts1, 2 * regts1)
    expect_identical(regts1 + matrix(1:8, ncol = 2), 2 * regts1)
    expect_identical(regts1 + regts1, 2 * regts1)
})

test_that("Ops.regts for unary operators (- and !)", {
    regts1 <- regts(matrix(as.numeric(1:8), ncol = 2), start = "2010Q2",
                    names = c("a", "b"), labels = c("Var a", "Var b"))
    ts1 <- regts:::unregts(regts1)
    expect_identical(ts_labels(-regts1), ts_labels(regts1))
    expect_identical(colnames(-regts1), colnames(regts1))
    expect_identical(-regts1, as.regts(-ts1))

    regts1 <- regts(matrix(rep(FALSE, 8), ncol = 2), start = "2010Q2",
                    names = c("a", "b"), labels = c("Var a", "Var b"))
    ts1 <- regts:::unregts(regts1)
    expect_identical(ts_labels(!regts1), ts_labels(regts1))
    expect_identical(colnames(!regts1), colnames(regts1))
    expect_identical(!regts1, as.regts(!ts1))
})


context("methods")

# functions window.regts and diff.regts have additional code for labels. This
# is already tested in test_labels.R

test_that("aggregate: no creation of colnames", {
    regts1 <- regts(matrix(rep(1:10), ncol = 2), start = "2010Q4",
                    names = c("", ""),labels = c("Timeseries a", "Timeseries b"))
    agg <- aggregate(regts1)
    expect_equal(colnames(regts1), colnames(agg))
    expect_equal(ts_labels(regts1), ts_labels(agg))
})

test_that("regts.intersect and regts.union", {

    regts1 <- regts(1:4, start = "2010Q1")
    regts2 <- regts(11:14, start = "2010Q3")
    i <- regts.intersect(regts1, regts2)
    u <- regts.union(regts1, regts2)
    expect_equal(i, regts(matrix(c(3, 4, 11, 12), ncol = 2),
                          start = "2010Q3", names = c("regts1", "regts2")))
    expect_equal(u, regts(matrix(c(1,2,3,4,NA,NA,NA,NA,11,12,13,14), ncol = 2),
                          start = "2010Q1", names = c("regts1", "regts2")))


})

test_that("regts.intersect and regts.union, with labels", {

    regts1 <- regts(1:4, start = "2011Y", labels = "Timeseries 1")
    regts2 <- regts(11:14, start = "2014", labels = "Timeseries 2")
    i <- regts.intersect(regts1, regts2)
    u <- regts.union(regts1, regts2)
    expect_equal(i, regts(matrix(c(4, 11), ncol = 2),
                          start = "2014", names = c("regts1", "regts2")),
                          check.attributes = FALSE)
    expect_equal(u, regts(matrix(c(1,2,3,4,NA,NA,NA,NA,NA,NA,11,12,13,14), ncol = 2),
                          start = "2011", names = c("regts1", "regts2")),
                          check.attributes = FALSE)

    lbls_i <- ts_labels(i)
    lbls_u <- ts_labels(u)
    expect_equal(lbls_i, lbls_u)
    expect_equal(lbls_i[1], ts_labels(regts1), check.attributes = FALSE)
    expect_equal(lbls_u[2], ts_labels(regts2), check.attributes = FALSE)
})

test_that("cbind and regts.intersect", {

    regts1 <- regts(1:4, start = "2011Y", labels = "Timeseries 1")
    regts2 <- regts(11:14, start = "2014", labels = "Timeseries 2")
    i <- regts.intersect(regts1, regts2)
    c <- cbind.regts(regts1, regts2)
    expect_equal(c, i)
})



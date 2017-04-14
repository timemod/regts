context("na_trim")

test_that("univariate timeseries", {
    ts1 <- regts(c(NA,1,3,NA,4,8), start = "2000")
    ts2 <- regts(c(NA,1,3,NA,4,8,NA), start = "2000")
    expect_identical(ts1, na_trim(ts2, method = "last"))
    expect_identical(na_trim(ts1, method = "first"), na_trim(ts2))
    expect_identical(na_trim(ts2), na_trim(ts2, method = "both"))
})

test_that("multivariate timeseries, is_na = all", {
    dataNA <- matrix(rep(NA, 15), ncol = 3)
    rtsNA <- regts(dataNA, start = "2017Q1", names = c("a", "b", "c"))

    data <- matrix(1:6, ncol = 3)
    prd <- period_range("2017Q2/2017Q3")
    rts <- regts(data, period = prd, names = c("a", "b", "c"))

    rtsNA[prd] <- rts
    rts1 <- rtsNA["2017Q1/2017Q3"]
    rts2 <- rtsNA["2017Q2/2018Q1"]

    # remove leading/trailing/all NAs
    expect_identical(na_trim(rtsNA, method = "both"), rts)
    expect_identical(na_trim(rtsNA, method = "last"), rts1)
    expect_identical(na_trim(rtsNA, method = "first"), rts2)

    expect_identical(na_trim(rts, method = "last"), na_trim(rts, is_na = "all"))
})

test_that("multivariate timeseries, is_na = any", {
    dataNA <- matrix(rep(NA, 12), ncol = 3)
    rtsNA <- regts(dataNA, start = "2017Q1", names = c("a", "b", "c"))

    data <- matrix(c(1,2,NA,3,4,NA,NA,6,7), ncol = 3)
    prd <- period_range("2017Q1/2017Q3")
    rts <- regts(data, period = prd, names = c("a", "b", "c"))

    rtsNA[prd] <- rts
    rts1 <- rtsNA["2017Q2/2017Q4"]
    rts2 <- rtsNA["2017Q2/2017Q2"]

    # remove NAs if any element in the row is NA
    expect_identical(na_trim(rtsNA, method = "first", is_na = "any"), rts1)
    expect_identical(na_trim(rtsNA, is_na = "any"), na_trim(rts2))

    expect_identical(na_trim(rts1, is_na = "any"), na_trim(rts2, is_na = "any"))
    expect_identical(na_trim(rts[, c("b","c")], is_na = "any", method = "both"),
                     na_trim(rts1[, c("b","c")], is_na = "any", method = "last"))
})

test_that("preserve labels", {

    ts <- regts(c(NA,1,3,NA,4,8), start = "2000", labels = "tijdreeks")
    expect_identical(ts_labels(ts), ts_labels(na_trim(ts)))

    dataNA <- matrix(rep(NA, 12), ncol = 3)
    rtsNA <- regts(dataNA, start = "2017Q1", names = c("a", "b", "c"),
                   labels = c("label_a", "label_b", "label_c"))

    data <- matrix(c(1,2,NA,3,4,NA,NA,6,NA), ncol = 3)
    prd <- period_range("2017Q1/2017Q3")
    rts <- regts(data, period = prd, names = c("a", "b", "c"),
                 labels = c("label_a", "label_b", "label_c"))

    rts1 <- na_trim(rts)
    expect_identical(ts_labels(rts), ts_labels(rts1))

    rts2 <- na_trim(rts, method = "last")
    expect_identical(ts_labels(rts), ts_labels(rts2))

    rts3 <- na_trim(rts, is_na = "any")
    expect_identical(ts_labels(rts), ts_labels(rts3))
})

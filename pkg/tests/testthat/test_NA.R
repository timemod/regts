context("test_na_skip")

test_that("na_skip, univariate timeseries", {
    ts1 <- regts(c(NA,1,3,NA,4,8), start = "2000")
    expect_identical(na_skip(ts1), na_skip(ts1, sides = "left"))
    ts2 <- regts(c(NA,1,3,NA,4,8,NA), start = "2000")
    expect_identical(na_skip(ts2), na_skip(ts2, sides = "both"))
})

test_that("na_skip, multivariate timeseries, all & any", {
    # remove all trailing NAs
    data <- matrix(c(1,3,NA,2,5,NA,3,7,NA), ncol = 3)
    rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
    expect_identical(na_skip(rts, sides = "right"), na_skip(rts, is.na = "all"))

    # remove only NAs if all elements in the row are NA
    data1 <- matrix(c(NA,3,NA,NA,5,6,NA,7,NA), ncol = 3)
    rts1 <- regts(data1, start = "2010Q1", names = c("a", "b", "c"))
    # only the first quarter in 2010 is removed
    expect_identical(na_skip(rts1, is.na = "all"), na_skip(rts1, sides = "left"))

    # remove only NAs if some elements in the row are NA
    data2 <- matrix(c(1,3,NA,NA,5,6,NA,7,NA), ncol = 3)
    rts2 <- regts(data2, start = "2010Q1", names = c("a", "b", "c"))

    expect_identical(na_skip(rts1[, c("b","c")], is.na = "any", sides = "right"),
                     na_skip(rts2[, c("b","c")], is.na = "any", sides = "right"))


    expect_identical(na_skip(rts1, is.na = "any"), na_skip(rts2, is.na = "any"))
})

context("data.frame")

test_that("as.regts.data.frame for univariate timeseries", {
    df <- data.frame(period = c("2015 Q3", "2015 Q4", "2016 Q1"), a = 1:3, stringsAsFactors = FALSE)
    ts1 <- as.regts(df, FUN = as.yearqtr)
    ts2 <- regts(1:3 , start = "2015Q3", names = "a")
    expect_identical(ts1, ts2)
    df2 <- df[-1]
    rownames(df2) <- df[[1]]
    expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate timeseries", {
    df <- data.frame(a = 1:3, b = 4:6)
    rownames(df) <- c("2015 3", "2015 4", "2016 1")
    ts1 <- as.regts(df, FUN = as.yearqtr, format = "%Y %q", index.column = "row.names")
    ts2 <- regts(matrix(1:6, ncol =  2), start = "2015Q3", names = c("a", "b"))
    expect_identical(ts1, ts2)

    df2 <- df
    rownames(df2) <- c("2015 Q3", "2015 Q4", "2016 Q1")
    expect_identical(df2, as.data.frame(ts2))
})


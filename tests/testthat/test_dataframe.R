context("data.frame")

test_that("as.regts.data.frame for univariate quarterly timeseries", {
    df <- data.frame(period = c("2015 Q3", "2015 Q4", "2016 Q1"), a = 1:3,
                     stringsAsFactors = FALSE)
    ts1 <- as.regts(df, time_column = 1)
    ts2 <- regts(1:3 , start = "2015Q3", names = "a")
    expect_identical(ts1, ts2)
    df2 <- df[-1]
    rownames(df2) <- df[[1]]
    expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate quarterly timeseries", {
    df <- data.frame(a = 1:3, b = 4:6)
    rownames(df) <- c("2015 3", "2015 4", "2016 1")
    ts1 <- as.regts(df, frequency = 4)
    ts2 <- regts(matrix(1:6, ncol =  2), start = "2015Q3", names = c("a", "b"))
    expect_identical(ts1, ts2)

    df2 <- df
    rownames(df2) <- c("2015 Q3", "2015 Q4", "2016 Q1")
    expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate yearly timeseries", {
    df <- data.frame(periods = c(2015, 2016, 2017), a = 1:3, b = 4:6)
    ts1 <- as.regts(df, time_column = 1)
    ts2 <- regts(matrix(1:6, ncol =  2), start = "2015", names = c("a", "b"))
    expect_identical(ts1, ts2)
    df2 <- df
    rownames(df2) <- as.character(df[[1]])
    df2 <- df2[-1]
    expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate yearly timeseries with labels", {
    df <- data.frame(periods = c(2015, 2016, 2017), a = 1:3, b = 4:6)
    ts_labels <- paste("Timeseries", c("a", "b"))
    label(df, self = FALSE) <- c("", ts_labels)
    ts1 <- as.regts(df, time_column = 1)
    ts2 <- regts(matrix(1:6, ncol =  2), start = "2015", names = c("a", "b"),
                 labels = ts_labels)
    expect_identical(ts1, ts2)
    df2 <- df
    rownames(df2) <- as.character(df[[1]])
    df2 <- df2[-1]
    expect_identical(df2, as.data.frame(ts2))
})

test_that("as.regts.data.frame for multivariate quarterly irregular timeseries", {
    df <- data.frame(a = 1:3, b = 4:6)
    rownames(df) <- c("2016Q2", "2015Q3", "2015Q4")
    ts1 <- as.regts(df)
    # convert ts1 from integer to double
    ts1[, ] <- as.numeric(ts1)
    ts2 <- regts(matrix(c(2,3,NA,1,5,6,NA,4), ncol =  2), start = "2015Q3",
                 names = c("a", "b"))
    expect_identical(ts1, ts2)
})

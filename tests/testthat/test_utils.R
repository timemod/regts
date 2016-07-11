context("utils")

test_that("filter_columns, univariate timseries", {
    df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3,
                     stringsAsFactors = FALSE)
    ts1 <- regts(1:3 , start = "2015Q3", names = "a")
    expect_identical(filter_columns(df, 'a'), df[, 'a', drop = FALSE])
    expect_identical(filter_columns(df, 'x'), df[, character(0), drop = FALSE])
    expect_identical(filter_columns(ts1, 'a'), ts1)
    expect_identical(filter_columns(ts1, 'x'), ts1[, character(0)])
})

test_that("filter_columns, multivariate timseries", {
    df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3,
                     b = 10:12, stringsAsFactors = FALSE)
    ts1 <- as.regts(df)
    expect_identical(filter_columns(df, 'b.*'), df[, 'b', drop = FALSE])
    expect_identical(filter_columns(df, 'x.*'), df[, character(0), drop = FALSE])
    expect_identical(filter_columns(ts1, 'b.*'), ts1[, 'b'])
    expect_identical(filter_columns(ts1, 'x.*'), ts1[, character(0)])
})



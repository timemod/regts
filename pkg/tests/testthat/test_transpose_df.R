context("tranpose_df")

test_that("data frame without row names and labels", {
    df <- data.frame(a = 1:3, b = 10:12)
    df_t <- transpose_df(df)
    df2 <- transpose_df(df_t)
    rownames(df2) <- NULL
    expect_identical(df, df2)
})

test_that("simple data frame with row names and no labels", {
    df <- data.frame(a = 1:3, b = 10:12, row.names = paste0("x", 1:3))
    df_t <- transpose_df(df)
    df2 <- transpose_df(df_t)
    expect_identical(df, df2)
})

test_that("data frame with a column with column names and no labels", {
    df <- data.frame(a = 1:3, b = 10:12, names = paste0("x", 1:3))
    df_t <- transpose_df(df, colname_column = 3)
    df_t2 <- transpose_df(df, colname_column = "names")
    expect_identical(df_t, df_t2)
    df2 <- transpose_df(df_t)

    df_correct <- df
    rownames(df_correct) <- df[[3]]
    df_correct[3] <- NULL
    expect_identical(df2, df_correct)
})

test_that("data frame with a column with column names and labels", {
    df <- data.frame(a = 1:3, b = 10:12, names = paste0("x", 1:3),
                     labels = paste("Variable", 1:3))
    Hmisc::label(df, self = FALSE) <- c(paste("Variable", c("a", "b")), "", "")
    df_t <- transpose_df(df, colname_column = 3, label_column = 4)
})


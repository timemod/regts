library(zoo, warn.conflicts = FALSE, quietly = TRUE)

context("zoo")

data <- matrix(1:20, ncol = 2)
colnames(data) <- c("a", "b")

test_that("as.regts.zoo, as.zoo.regts and as.zooreg.regts", {
    zoo1  <- zooreg(data[, 1], start = as.yearqtr("1980Q2"), frequency = 4)
    ts1 <- regts(data[, 1], start = "1980Q2")
    expect_identical(as.regts(zoo1), ts1)
    # expect_identical(as.zoo(ts1), zoo1)

    zoo2  <- zooreg(data[, 1, drop = FALSE], start = as.yearqtr("1980Q2"), frequency = 4)
    ts2 <- regts(data[, 1, drop = FALSE], start = "1980Q2")
    expect_identical(as.regts(zoo2), ts2)
    expect_identical(as.zoo(ts2), zoo2)
    expect_identical(as.zooreg(ts2), zoo2)

    zoo3  <- zooreg(data, start = as.yearqtr("1980Q2"), frequency = 4)
    ts3 <- regts(data, start = "1980Q2")
    expect_identical(as.regts(zoo3), ts3)
    expect_identical(as.zoo(ts3), zoo3)
    expect_identical(as.zooreg(ts2), zoo2)
})

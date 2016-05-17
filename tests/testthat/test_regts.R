context("regts")

test_that("constructor regts for univariate timeseries", {

    regts1 <- regts(1:10, start = "2010Q4")
    ts1 <- ts(1:10, start = c(2010,4), frequency = 4)
    expect_identical(regts1, as.regts(ts1))
    expect_identical(class(regts1), c("regts", "ts"))
    expect_identical(is.regts(regts1), TRUE)
    expect_identical(is.regts(ts1), FALSE)
    expect_identical(is.ts(regts1), TRUE)

    regts1 <- regts(matrix(1:10, ncol = 1), start = "2010Q4")
    ts1 <- ts(matrix(1:10, ncol = 1), start = c(2010,4), frequency = 4)
    expect_identical(regts1, as.regts(ts1))

    regts1 <- regts(1:10, start = "2010", end = "2012", names = "a")
    ts1 <- ts(1:10, start = 2010, end = 2012, frequency = 1, names = 'a')
    expect_identical(regts1, as.regts(ts1))

    expect_error(regts(1:10, start = "2010-1"),
                 "Frequency unknown. Specify argument frequency")

    regts1 <- regts(1:10, start = "2010-1", frequency = 3)
    ts1 <- ts(1:10, start = c(2010, 1), frequency = 3)
    expect_identical(as.regts(regts1), as.regts(ts1))
})

test_that("constructor regts for multivariate time series", {
    data <- matrix(1: 9, ncol = 3)
    regts1 <- regts(data, start = "2010Q4", names = c("a", "b", "c"))
    ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b", "c"))
    expect_identical(regts1, as.regts(ts1))
    expect_identical(class(regts1), c("regts", "mts", "ts", "matrix"))
    expect_identical(is.regts(regts1), TRUE)
    expect_identical(is.regts(ts1), FALSE)
    expect_identical(is.ts(regts1), TRUE)
    expect_identical(is.mts(regts1), TRUE)
    expect_identical(is.matrix(regts1), TRUE)

    regts1 <- regts(data, start = "2010M1", end = "2012m3")
    ts1 <- ts(data, start = c(2010,1), end = c(2012,3), frequency = 12)
    expect_identical(regts1, as.regts(ts1))
})

test_that("get_regperiod_range / check_extend", {
    regts1 <- regts(1, start = "2010Q1", end = "2011Q4")
    expect_identical(get_regperiod_range(regts1),
                     regperiod_range("2010Q1", "2011Q4"))
    expect_identical(check_extend(regts1, as.regperiod_range("2010Q1/2011Q3")),
                     FALSE)
    expect_identical(check_extend(regts1, as.regperiod_range("2008Q1/2011Q3")),
                     TRUE)
    expect_identical(check_extend(regts1, regperiod_range("2010Q2", "2012Q1")),
                     TRUE)
})

test_that("period selection in univariate timeseries", {
    ts1 <- ts(1:8, start = c(2010, 1), end = c(2011, 4), frequency = 4)
    regts1 <- as.regts(ts1)

    regts2 <- regts1["2010Q2/2011Q3"]
    expect_is(regts2, "regts")
    expect_identical(regts2,
                     as.regts(window(ts1, start = c(2010,2), end = c(2011,3))))

    regts2 <- regts1["2008/2012"]
    expect_is(regts2, "regts")
    expect_identical(regts2,
                     as.regts(window(ts1, start = c(2008,1), end = c(2012,4),
                                     extend = TRUE)))

    regts2 <- regts1["2008/"]
    expect_is(regts2, "regts")
    expect_identical(regts2,
                     as.regts(window(ts1, start = c(2008,1), extend = TRUE)))

    regts2 <- regts1["/2011Q2"]
    expect_is(regts2, "regts")
    expect_identical(regts2,
                     as.regts(window(ts1, end = c(2011,2), extend = TRUE)))

    expect_identical(regts1[3], ts1[3])
    expect_identical(regts1[3:10], ts1[3:10])
})

test_that("period / column selection in multivariate timeseries", {
    data <- matrix(1: 9, ncol = 3)
    ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b", "c"))
    regts1 <- as.regts(ts1)

    regts2 <- regts1[, 'b']
    expect_is(regts2, "regts")
    expect_identical(regts2, as.regts(ts1[, 'b']))

    expect_identical(regts1[, c("b", "a")], as.regts(ts1[, c("b", "a")]))

    expect_identical(regts1['2011Q1', "c"], as.regts(
        window(ts1, start = c(2011,1), end = c(2011,1)))[, "c"])

    expect_identical(regts1['2011Q1', c("b", "a")], as.regts(
        window(ts1, start = c(2011,1), end = c(2011,1)))[, c("b", "a")])

    expect_identical(regts1['2011'], as.regts(
        window(ts1, start = c(2011,1), end = c(2011,4), extend = TRUE)))
})


test_that("period selection at the lhs of a univariate timeseries", {

    ts1 <- ts(1:8, start = c(2010, 1), end = c(2011, 4), frequency = 4)
    regts1 <- as.regts(ts1)

    # without period extension
    regts2 <- regts1
    regts2['2010Q2'] <- 2
    regts2['2011Q3/'] <- 2 * regts1['2011Q3/']
    ts2 <- ts1
    window(ts2, start = c(2010, 2), end = c(2010,2)) <- 2
    window(ts2, start = c(2011, 3)) <- 2 * window(ts1, start = c(2011, 3))
    expect_identical(regts2, as.regts(ts2))

    # with period extension
    regts2 <- regts1
    regts2['/2013Q2'] <- 9
    regts2['2008Q2/2009Q2'] <- regts1['2010Q2/2011Q2']
    ts2 <- ts1
    suppressWarnings({
        window(ts2, end = c(2013,2), extend = TRUE) <- 9
        window(ts2, start = c(2008, 2), end = c(2009, 2), extend = TRUE) <-
            window(ts1, start = c(2010, 2), end = c(2011,2))
    })
    expect_equivalent(regts2, as.regts(ts2))
})


test_that("period and column selection at the lhs of an assignment", {

    data <- matrix(1: 10, ncol = 2)
    ts1 <- ts(data, start = c(2010,4), frequency = 4, names = c("a", "b"))
    regts1 <- as.regts(ts1)

    regts2 <- regts1
    regts2['2011Q1/'] <- 2 * regts1['2011Q1/']
    regts2['/2011Q1', 'a'] <- c(10, 20)

    ts2 <- ts1
    window(ts2, start = c(2011, 1)) <- 2 * window(ts1, start = c(2011,1))
    window(ts2, end = c(2011, 1))[, 'a'] <- c(10, 20)

    expect_identical(regts2, as.regts(ts2))

    regts2 <- regts1
    regts2[, 'x'] <- 1
    regts2['2010Q1'] <- 2 * regts2['2011Q1']
    regts2['2012Q3/2012Q4', 'xx'] <- 2 * regts2['2011Q2/2011Q3', 'x']
    regts2['2012Q3/',  c('a_2', 'b_2')] <- regts1['2011Q2/2011Q3',
                                                       c('a', 'b')]
    regts2['2010Q3/',  c('a', 'a_3')] <- 2 * regts1['2011Q2/2011Q3',
                                                  c('a', 'a')]
    expect_equal_to_reference(regts2, "period_and_column.rds")
})

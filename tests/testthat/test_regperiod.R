library(regts)
context("regperiod")

test_that("constructor regperiod", {
    expect_identical(as.character(regperiod("2010 q 2")), "2010Q2")
    expect_identical(as.character(regperiod("2010Q8")), "2011Q4")
    expect_identical(as.character(regperiod("2012")), "2012")
    expect_identical(as.character(regperiod("2012M3")), "2012M3")
    expect_identical(as.character(regperiod("2001-4", frequency = 4)), "2001Q4")
    expect_error(regperiod("2001-4"),
                 "Frequency unknown. Specify argument frequency")
    expect_error(regperiod("2001Q4", frequency = 12),
                 "Supplied frequency does not agree with actual frequency in regperiod")
})

test_that("subtracting period", {
    expect_identical(as.character(regperiod("2010q2") + 1), "2010Q3")
    expect_identical(as.character(regperiod("2010q1") - 3), "2009Q2")
    expect_identical(as.character(regperiod("2011") - 3), "2008")
})

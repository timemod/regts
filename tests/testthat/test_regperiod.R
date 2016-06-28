library(regts)
context("regperiod")

test_that("constructor regperiod", {
    expect_identical(as.character(regperiod("2010 q 2")), "2010Q2")
    expect_identical(as.character(regperiod("2010Q8")), "2011Q4")
    expect_identical(as.character(regperiod("2012")), "2012")
    expect_identical(as.character(regperiod("2012M3")), "2012M3")
    expect_identical(as.character(regperiod("2001-4", frequency = 4)), "2001Q4")
    expect_identical(as.character(regperiod("2001 4", frequency = 12)), "2001M4")
    expect_identical(as.character(regperiod("4 2001", frequency = 12)), "2001M4")
})

test_that("subtracting period", {
    expect_identical(as.character(regperiod("2010q2") + 1), "2010Q3")
    expect_identical(as.character(regperiod("2010q1") - 3), "2009Q2")
    expect_identical(as.character(regperiod("2011") - 3), "2008")
})

test_that("Isis type periods", {
    expect_identical(regperiod("2010.2q"), regperiod("2010Q2"))
    expect_identical(regperiod("may2010"), regperiod("2010M5"))
    expect_identical(regperiod("3 q 2005"), regperiod("2005Q3"))
    expect_identical(regperiod("6 q 2005"), regperiod("507Q1"))
    expect_identical(regperiod("2016Y"), regperiod("2016"))
})

test_that("errors", {
    expect_error(regperiod("2001-4"),
             "Frequency of period 2001-4 unknown. Specify argument frequency.")
    expect_error(regperiod("2001Q4", frequency = 12),
      "Specified frequency 12 does not agree with actual frequency in period 2001Q4.")
    expect_error(regperiod("xxx"), "Illegal period xxx")
    expect_error(regperiod("2010M2a"), "Illegal period 2010M2a")
    expect_error(regperiod("2010z2"), "Illegal period 2010z2")
    expect_error(regperiod("a2010M2"), "Illegal period a2010M2")

    # check that the parser still works after errors:
    expect_identical(as.character(regperiod("2010Q8")), "2011Q4")
})

test_that("frequency", {
    expect_identical(frequency(regperiod("2010q2")), 4)
    expect_identical(frequency(regperiod("2010-2", frequency = 2)), 2)
})

context("regperiod_range")

test_that("constructor regperiod_range", {
    expect_identical(as.character(regperiod_range("2010 q 2", "2011Q3")),
                     "2010Q2/2011Q3")
    expect_identical(as.character(regperiod_range("2010M2")),
                     "2010M2/2010M2")
    expect_identical(as.character(regperiod_range("2010q2", NULL)),
                     "2010Q2/")
    expect_error(regperiod_range("2001-4", "2014Q"),
           "Frequency of period 2001-4 unknown. Specify argument frequency.")
    expect_error(regperiod_range("2001", "2014Q4"),
                 "The two periods have different frequency")
    expect_error(as.character(regperiod_range("2010Q4", "2010Q2")),
                     "The start period (2010Q4) is after the end period (2010Q2)")
})

test_that("as.regperiod_range.character", {
    expect_identical(as.character(as.regperiod_range("2010 q 2/2011Q3")),
                     "2010Q2/2011Q3")
    expect_identical(as.character(as.regperiod_range("2010M2")),
                     "2010M2/2010M2")
    expect_identical(as.character(as.regperiod_range("2010q2/")),
                     "2010Q2/")
    expect_error(as.regperiod_range("2001-4 / 2014Q"),
         "Frequency of period 2001-4 unknown. Specify argument frequency.")
    expect_error(as.regperiod_range("2001/2014Q4"),
                 "The two periods have different frequency")
    expect_error(as.character(as.regperiod_range("2010Q4/2010Q2")),
                 "The start period (2010Q4) is after the end period (2010Q2)")
})

test_that("start_period and end_period", {
    r <- regperiod_range("2010Q4", "2011Q3")
    expect_identical(start_period(r), regperiod("2010Q4"))
    expect_identical(end_period(r), regperiod("2011Q3"))
    r <- regperiod_range("2010", NULL)
    expect_identical(start_period(r), regperiod("2010"))
    expect_identical(end_period(r), NULL)
    r <- regperiod_range(NULL, "2010m3")
    expect_identical(start_period(r), NULL)
    expect_identical(end_period(r), regperiod("2010M3"))
})

test_that("modify_frequency", {
    r <- regperiod_range("2010Q4", "2011Q3")
    r <- modify_frequency(r, new_freq = 12)
    expect_identical(r, as.regperiod_range("2010M10/2011M9"))
    r <- regperiod_range("2010Q4", NULL)
    r <- modify_frequency(r, new_freq = 12)
    expect_identical(r, regperiod_range("2010M10", NULL))
    r <- regperiod_range(NULL, "2010")
    r <- modify_frequency(r, new_freq = 4)
    expect_identical(r, regperiod_range(NULL, "2010Q4"))
})

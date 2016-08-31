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
          "The start period \\(2010Q4\\) is after the end period \\(2010Q2\\)")
    expect_error(regperiod_range(NULL, NULL),
          "At least one of the periods should not be NULL")
})

test_that("as.regperiod_range.character", {
    expect_identical(as.character(as.regperiod_range("2010 q 2/2011Q3")),
                     "2010Q2/2011Q3")
    expect_identical(as.character(as.regperiod_range("2010M2")),
                     "2010M2/2010M2")
    expect_identical(as.character(as.regperiod_range("2010q2/")),
                     "2010Q2/")
    expect_identical(as.character(as.regperiod_range("2010q2 /  ")),
                     "2010Q2/")
    expect_identical(as.character(as.regperiod_range("  /2010q2")),
                     "/2010Q2")
    expect_error(as.regperiod_range("2001-4 / 2014Q"),
          "Frequency of period 2001-4 unknown. Specify argument frequency.")
    expect_error(as.regperiod_range("2001/2014Q4"),
          "The two periods have different frequency")
    expect_error(as.character(as.regperiod_range("2010Q4/2010Q2")),
          "The start period \\(2010Q4\\) is after the end period \\(2010Q2\\)")
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

test_that("length subrange", {
    r <- regperiod_range("2010Q4", "2011Q3")
    s <- regperiod_range("2016Q1", "2016Q4")
    expect_identical(lensub(r), lensub(s))

    r <- regperiod_range("2010", "2013")
    s <- regperiod_range("2014", "2017")
    expect_identical(lensub(r), lensub(s))
    t <- as.character(r)
    expect_error(lensub(t), "Variable should be a regperiod_range object")
})

test_that("logical operators", {
    expect_true(regperiod_range("2010Q2","2010Q4") > regperiod_range("2010Q1", "2010Q3"))
    expect_true(regperiod_range("2010M2","2010M3") <= regperiod_range("2010M12","2011M1"))
    expect_true(regperiod_range("2010Q2","2010Q2") == regperiod_range("2010Q2","2010Q2"))
    expect_true(regperiod_range("2010Q2","2010Q3") != regperiod_range("2010Q1","2010Q2"))
	expect_true(regperiod_range("2010Q1","2010Q2") != regperiod_range("2010M1","2010M2"))

    expect_false(regperiod_range("2010Q2","2010Q4") < regperiod_range("2010Q1","2010Q3"))
    expect_false(regperiod_range("2010Q2","2010Q4") != regperiod_range("2010Q2","2010Q4"))
    expect_false(regperiod_range("2010Q1","2010Q2") == regperiod_range("2010Q4","2010Q5"))
	
    expect_error(regperiod_range("2010Q1","2010Q2") <= regperiod_range("2010M1","2010M2"),
                 paste("Logical operations '<, <=, >, >=' on regperiod_ranges with different",
                 "frequencies are not allowed"))
	expect_error(regperiod_range("2010Q1","2010Q2") > 1,
                 "Both operators must be regperiod_ranges when using logical operators")					
})

test_that("arithmetic operators: only + and - allowed", {
    expect_identical(regperiod_range("2010q2","2011q2") + 4, regperiod_range("2011q2","2012q2"))
    expect_identical(regperiod_range("2010","2014") - 4, regperiod_range("2006","2010"))
    expect_identical(regperiod_range("2010m2","2010m3") - 8, regperiod_range("2009m6","2009m7"))
    expect_identical(regperiod_range("2010q2","2011q2") + 4, 4 + regperiod_range("2010q2","2011q2"))

    expect_error(regperiod_range("2010Q1", "2010Q1") + 1.2,
                "Second operand must be an integer number")
    expect_error(regperiod_range("2010Q1", "2010Q1") * 2,
                "Illegal operation, only \\+ and \\- or logical operators allowed")
    expect_error(regperiod_range("2010Q1","2010Q1") - regperiod_range("2010Q1","2010Q1"),
				paste("Arithmetic operators \\+ and \\- only allowed on a",
                "combination of regperiod\\_range and integer number"))
				
               
})
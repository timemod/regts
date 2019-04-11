library(testthat)
library(regts)
context("period_range")

test_that("constructor period_range", {
  # "d1/d2" is an alternative way to ("d1","d2") to construct a period_range
  expect_identical(as.character(period_range("2010 q 2", "2011Q3")),
                   "2010Q2/2011Q3")
  expect_identical(as.character(period_range("2010M02", "2010M02")), "2010M02")
  expect_identical(as.character(period_range("2010q2", NULL)),
                   "2010Q2/")
  expect_identical(period_range("2010 q 2", "2011Q3"),
                   period_range("2010Q2/2011Q3"))
  expect_identical(period_range("2010M2", "2010M2"),
                   period_range("2010M0002/2010M02"))
  expect_identical(period_range(NULL, "2010q2"),
                   period_range("/2010Q2"))
  expect_error(period_range("2001-4", "2014Q"),
               "Frequency of period 2001-4 unknown. Specify argument frequency.")
  expect_error(period_range("2001", "2014Q4"),
               "Arguments start and end have different frequency")
  expect_error(as.character(period_range("2010Q4", "2010Q2")),
               "The start period \\(2010Q4\\) is after the end period \\(2010Q2\\)")
  expect_error(period_range(NULL, NULL),
               "Either argument start or end should be specified")
  expect_error(as.character(period_range("2010Q4/2011Q3", "2010Q2")),
               "Argument end should not be specified if start is a period range string")

  d <- as.Date("2010-05-30")
  expect_identical(as.character(period_range(d)), "2010M05/")
  expect_identical(as.character(period_range(d, d+100, frequency = 4)),
                   "2010Q2/2010Q3")
  expect_identical(as.character(period_range(d, NULL, frequency = 4)),
                   "2010Q2/")

  d2 <- period(NA, frequency = 4)
  expect_identical(as.character(period_range(d, d2, frequency = 4)),
                   "2010Q2/")

  d2 <- period(NA, frequency = 12)
  expect_error(period_range(d, d2, frequency = 4),
               "Arguments start and end have different frequency")

  expect_error(period_range(d2, d2, frequency = 4),
               "Both the start and end period of the period range are NA.")

  expect_error(period_range(period(c("2018m1", "2018m2")), period("2019m2"),
                            frequency = 4),
               "Argument 'start' must be of length 1")
  expect_error(period_range(period("2018m1"), period(c("2019m2", "2019m3")),
                            frequency = 4),
               "Argument 'end' must be of length 1")
})

test_that("as.period_range.character", {
  expect_identical(as.character(as.period_range("2010 q 2/2011Q3")),
                   "2010Q2/2011Q3")
  expect_identical(as.character(as.period_range("2010M2")), "2010M02")
  expect_identical(as.character(as.period_range("2010q2/")),
                   "2010Q2/")
  expect_identical(as.character(as.period_range("2010q2 /  ")),
                   "2010Q2/")
  expect_identical(as.character(as.period_range("  /2010q2")),
                   "/2010Q2")
  expect_error(as.period_range("2001-4 / 2014Q"),
               "Frequency of period 2001-4 unknown. Specify argument frequency.")
  expect_error(as.period_range("2001/2014Q4"),
               "The two periods have different frequency")
  expect_error(as.character(as.period_range("2010Q4/2010Q2")),
               "The start period \\(2010Q4\\) is after the end period \\(2010Q2\\)")
})

test_that("as.period_range.period", {
  prd <- period("2010")
  r2010 <- period_range("2010", "2010")
  expect_identical(as.period_range(prd), r2010)
})

test_that("as.period_range.character", {
  rng <- "/2010"
  rng2010 <- period_range(NULL, "2010")
  expect_identical(as.period_range(rng), rng2010)
})

test_that("as.period_range.numeric", {
  r2010 <- period_range("2010", "2010")
  expect_identical(as.period_range(2010), r2010)
})


test_that("as.character.period_range", {
  r00_10 <- period_range("2000", "2010")
  expect_identical(as.character(r00_10),"2000/2010")
})


test_that("length subrange", {
  r <- period_range("2010Q4", "2011Q3")
  s <- period_range("2016Q1", "2016Q4")
  expect_identical(nperiod(r), nperiod(s))

  r <- period_range("2010", "2013")
  s <- period_range("2014", "2017")
  expect_identical(nperiod(r), nperiod(s))
  t <- as.character(r)
  expect_error(nperiod(t), "Variable should be a period_range object")
})

test_that("logical operators", {
  expect_true(period_range("2010Q2","2010Q4") > period_range("2010Q1", "2010Q3"))
  expect_true(period_range("2010M2","2010M3") <= period_range("2010M12","2011M1"))
  expect_true(period_range("2010Q2","2010Q2") == period_range("2010Q2","2010Q2"))
  expect_true(period_range("2010Q2","2010Q3") != period_range("2010Q1","2010Q2"))
  expect_true(period_range("2010Q1","2010Q2") != period_range("2010M1","2010M2"))

  expect_false(period_range("2010Q2","2010Q4") < period_range("2010Q1","2010Q3"))
  expect_false(period_range("2010Q2","2010Q4") != period_range("2010Q2","2010Q4"))
  expect_false(period_range("2010Q1","2010Q2") == period_range("2010Q3","2010Q3"))

  expect_true(period_range(NULL,"2010Q2") == period_range(NULL,"2010Q2"))
  expect_false(period_range(NULL,"2010Q2") != period_range(NULL,"2010Q2"))
  expect_false(period_range(NULL,"2010Q2") == period_range("2010Q1","2010Q2"))
  expect_true(period_range(NULL,"2010Q2") != period_range("2010Q1","2010Q2"))

  expect_true(period_range("2010Q2", NULL) == period_range("2010Q2", NULL))
  expect_false(period_range("2010Q2", NULL) != period_range("2010Q2", NULL))
  expect_false(period_range("2010Q2", NULL) == period_range("2010Q1","2010Q2"))
  expect_true(period_range("2010Q2", NULL) != period_range("2010Q1","2010Q2"))

  expect_true(period_range("2010Q3", NULL) > period_range("2010Q2", NULL))
  expect_false(period_range("2010Q2", NULL) > period_range("2010Q2", NULL))
  expect_true(period_range("2010Q2", NULL) >= period_range("2010Q2", NULL))

  expect_false(period_range(NULL, "2010Q3") < period_range(NULL, "2010Q2"))
  expect_true(period_range(NULL, "2010Q2") < period_range(NULL, "2010Q3"))
  expect_true(period_range(NULL, "2010Q2") <= period_range(NULL, "2010Q2"))

  expect_true(is.na(period_range(NULL, "2010Q2") < period_range("2010Q2", NULL)))

  expect_error(period_range("2010Q1","2010Q2") <= period_range("2010M1","2010M2"),
               paste("Logical operations '<, <=, >, >=' on period_ranges with different",
                     "frequencies are not allowed"))
  expect_error(period_range("2010Q1","2010Q2") > 1,
               "Both operators must be period_ranges when using logical operators")
})

test_that("arithmetic operators: only + and - allowed", {
  expect_identical(period_range("2010q2","2011q2") + 4, period_range("2011q2","2012q2"))
  expect_identical(period_range("2010","2014") - 4, period_range("2006","2010"))
  expect_identical(period_range("2010m2","2010m3") - 8, period_range("2009m6","2009m7"))
  expect_identical(period_range("2010q2","2011q2") + 4, 4 + period_range("2010q2","2011q2"))

  expect_error(period_range("2010Q1", "2010Q1") + 1.2,
               "Second operand must be an integer number")
  expect_error(period_range("2010Q1", "2010Q1") * 2,
               "Illegal operation, only \\+ and \\- or logical operators allowed")
  expect_error(period_range("2010Q1","2010Q1") - period_range("2010Q1","2010Q1"),
               paste("Arithmetic operators \\+ and \\- only allowed on a",
                     "combination of period\\_range and integer number"))
})

test_that("is.period_range",{
  expect_identical(is.period_range(period_range("2010q2","2011q2")), TRUE)
  expect_identical(is.period_range("2010q2/2011q2"), FALSE)
  r00_10 <- as.period_range("2000/2010")
  expect_identical(is.period_range(r00_10), TRUE)
  expect_identical(r00_10, period_range("2000", "2010"))
})

test_that("frequency", {
  expect_identical(frequency(period_range("2016Q1", "2018Q2")), 4)
  expect_identical(frequency(period_range("2016M1", NULL)), 12)
  expect_identical(frequency(period_range("2016", "2019")), 1)
})


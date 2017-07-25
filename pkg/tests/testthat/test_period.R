library(regts)
library(testthat)

context("period")

test_that("constructor period", {
  expect_identical(as.character(period("2010 q 2")), "2010Q2")
  expect_identical(as.character(period("9999Q3")), "9999Q3")
  expect_identical(as.character(period("1m2")), "1M2")
  expect_identical(as.character(period("2012")), "2012")
  expect_identical(as.character(period(2012)), "2012")
  expect_identical(as.character(period("2012M3")), "2012M3")
  expect_identical(as.character(period("2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("2001 4", frequency = 12)), "2001M4")
  expect_identical(as.character(period("4 2001", frequency = 12)), "2001M4")
})

test_that("constructor period with T prefix", {
  expect_identical(as.character(period("T2010 q 2")), "2010Q2")
  expect_identical(as.character(period("t2011Q3")), "2011Q3")
  expect_identical(as.character(period("t2012")), "2012")
  expect_identical(as.character(period("t2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("t 2001 4", frequency = 12)), "2001M4")
  expect_identical(as.character(period("T4 2001", frequency = 12)), "2001M4")
})

test_that("constructor period with Y prefix", {
  expect_identical(as.character(period("Y2010 q 2")), "2010Q2")
  expect_identical(as.character(period("y2010Q3")), "2010Q3")
  expect_identical(as.character(period("J2012")), "2012")
  expect_identical(as.character(period("j2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("j 2001 4", frequency = 12)), "2001M4")
  expect_identical(as.character(period("j4 2", frequency = 12)), "4M2")
  expect_identical(as.character(period("Y2 3", frequency = 4)), "2Q3")
})

test_that("Isis type periods", {
  expect_identical(period("2010.2q"), period("2010Q2"))
  expect_identical(period("may2010"), period("2010M5"))
  expect_identical(period("3 q 2005"), period("2005Q3"))

  expect_identical(period("2016Y"), period("2016"))
})

test_that("errors", {
  expect_error(period("2001-4"),
               "Frequency of period 2001-4 unknown. Specify argument frequency.")
  expect_error(period("2001Q4", frequency = 12),
               paste("Specified frequency 12 does not agree with actual frequency",
                    "in period 2001Q4."))
  expect_error(period("xxx"), "Illegal period xxx")
  expect_error(period("2010M2a"), "Illegal period 2010M2a")
  expect_error(period("2010z2"), "Illegal period 2010z2")
  expect_error(period("a2010M2"), "Illegal period a2010M2")

  expect_error(period("2010Q8"), "Illegal period 2010Q8")
  expect_error(period("2010.0", frequency = 1), "Illegal period 2010.0")
  expect_error(period("6 q 2005"), "Illegal period 6 q 2005")
  expect_error(period("100000Q1"), "Illegal period 100000Q1")

  expect_error(period("2010.8", frequency = 4),
               paste("Subperiod of period 2010.8 is larger than the",
                     "specified frequency 4"))

  # check that the parser still works after errors:
  expect_identical(as.character(period("2011Q3")), "2011Q3")
})

test_that("frequency", {
  expect_identical(frequency(period("2010q2")), 4)
  expect_identical(frequency(period("2010-2", frequency = 2)), 2)
})

test_that("logical operators", {
  expect_true(period("2010Q2") > period("2010Q1"))
  expect_true(period("2010M2") <= period("2010M12"))
  expect_true(period("2010Q2") == period("2010Q2"))
  expect_true(period("2010Q2") != period("2010Q1"))
  expect_false(period("2010Q2") < period("2010Q1"))
  expect_false(period("2010Q2") != period("2010Q2"))
  expect_false(period("2010Q1") == period("2010M1"))

  expect_error(period("2010Q1") > period("2010M1"),
               "Illegal logical operation, only == and != allowed")
})

test_that("arithmetic operators: only + and - allowed", {
  expect_identical(period("2010q2") + 4, period("2011q2"))
  expect_identical(period("2010") - 4, period("2006"))
  expect_identical(period("2010m2") - 8, period("2009m6"))
  expect_identical(period("2010m2") - period("2009m6"), 8)
  expect_identical(period("2010q2") + 4, 4 + period("2010q2"))

  expect_error(period("2010Q1") * 2,
               "Illegal arithmetic operation, only \\+ and \\- allowed")
  expect_error(period("2010Q1") + period("2010Q2"),
               "Arithmetic operation \\+ on two periods is not allowed")
  expect_error(period("2010Q1") - period("2010M1"),
               paste("Arithmetic operations on periods with different",
                     "frequencies are not allowed"))
})

test_that("as.period", {
  expect_identical(as.period("2010.2q"), period("2010Q2"))
  expect_identical(as.period("2005q3"), period("2005Q3"))

})

test_that("print period", {
  expect_output(print(period("2010q2")),"2010Q2")
  expect_output(print(period("2010-1", freq = 12) - 3),"2009M10")
})

test_that("get_year / get_subperiod", {
  p1 <- period("2010Q3")
  expect_identical(get_year(p1), 2010)
  expect_identical(get_subperiod(p1), 3)
})

test_that("as.period.numeric", {
  expect_identical(as.period(2010), period("2010"))
  expect_identical(as.period(2010.001, frequency = 1), period("2010"))
  expect_identical(as.period(as.integer(2010)), period("2010"))
  expect_identical(as.period(2010, frequency = 4), period("2010Q1"))
  expect_identical(as.period(2010.75, frequency = 4), period("2010Q4"))
  expect_error(as.period(2010.75), "Argument frequency should be specified")
})


test_that("regts:::is_period_text", {
  expect_identical(regts:::is_period_text(c("2060", "noot", "2012M2",
                                            "2010-2", "2010Q3QA")),
                   c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(regts:::is_period_text(c("2060", "noot", "2012M2",
                                            "2010-2", "2010Q3QA")),
                   c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_true(regts:::is_period_text("2010-2", frequency = 4))
  expect_false(regts:::is_period_text("2010-8", frequency = 4))
  expect_false(regts:::is_period_text("2010Q2", frequency = 1))
  expect_false(regts:::is_period_text("2010Q2", frequency = 12))
  expect_true(regts:::is_period_text("2010Q2", frequency = 4))
  expect_true(regts:::is_period_text("2010-8"))
  expect_false(regts:::is_period_text("2010Q8"))
  expect_false(regts:::is_period_text("2010Q0"))
  expect_false(regts:::is_period_text("2010.0"))
  expect_false(regts:::is_period_text("2010.0", frequency = 1))
})

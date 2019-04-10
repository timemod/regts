library(regts)
library(testthat)

rm(list = ls())

context("period")

test_that("constructor period", {
  expect_identical(as.character(period("2010 q 2")), "2010Q2")
  expect_identical(as.character(period("9999Q3")), "9999Q3")
  expect_identical(as.character(period("1m2")), "1M02")
  expect_identical(as.character(period("2012")), "2012")
  expect_identical(as.character(period(2012)), "2012")
  expect_identical(as.character(period(as.factor("2012M3"))), "2012M03")
  expect_identical(as.character(period("2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("2001 4", frequency = 12)), "2001M04")
  expect_identical(as.character(period("4 2001", frequency = 12)), "2001M04")
  d <- as.Date("2010-05-30")
  expect_identical(as.character(period(d)), "2010M05")
  d <- as.Date("2010-05-01")
  expect_identical(as.character(period(d)), "2010M05")
  expect_identical(as.character(period(d, frequency = 4)), "2010Q2")
  expect_identical(as.Date(period(d, frequency = 4)), as.Date("2010-04-01"))
  expect_identical(as.character(period(d + 100, freq = 4)), "2010Q3")
  expect_identical(as.character(period(d, frequency = 4)), "2010Q2")
  expect_identical(as.character(period(as.POSIXct(d), frequency = 1)), "2010")
  expect_identical(as.character(period(as.POSIXlt(d), frequency = 3)), "2010-2")
  expect_identical(as.character(period("2001-3", frequency = 9999)), "2001-0003")



  p <- period(as.Date(NA))
  expect_identical(as.numeric(p), NA_real_)
  expect_identical(p, period(NA, frequency = 12))
  expect_identical(p, period(NA_character_, frequency = 12))
  expect_identical(p, period(NA_real_, frequency = 12))


})


test_that("constructor period with T prefix", {
  expect_identical(as.character(period("T2010 q 2")), "2010Q2")
  expect_identical(as.character(period("t2011Q3")), "2011Q3")
  expect_identical(as.character(period("t2012")), "2012")
  expect_identical(as.character(period("t2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("t 2001 4", frequency = 12)), "2001M04")
  expect_identical(as.character(period("T4 2001", frequency = 12)), "2001M04")
})

test_that("constructor period with Y prefix", {
  expect_identical(as.character(period("Y2010 q 2")), "2010Q2")
  expect_identical(as.character(period("y2010Q3")), "2010Q3")
  expect_identical(as.character(period("J2012")), "2012")
  expect_identical(as.character(period("j2001-4", frequency = 4)), "2001Q4")
  expect_identical(as.character(period("j 2001 4", frequency = 12)), "2001M04")
  expect_identical(as.character(period("j4 2", frequency = 12)), "4M02")
  expect_identical(as.character(period("Y2 3", frequency = 4)), "2Q3")
})

test_that("constructor period with numerical input", {
  expect_identical(as.character(period(2010)), "2010")
  expect_identical(as.character(period(2010.25, frequency = 4)), "2010Q2")
  expect_identical(as.character(period(2010.25, frequency = 12)), "2010M04")
})

test_that("Isis type periods", {
  expect_identical(period("2010.2q"), period("2010Q2"))
  expect_identical(period("may2010"), period("2010M05"))
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
  expect_error(period("2010.0", frequency = 12), "Illegal period 2010.0")
  expect_error(period("6 q 2005"), "Illegal period 6 q 2005")
  expect_error(period("100000Q1"), "Illegal period 100000Q1")

  expect_error(period("2010.8", frequency = 4),
               paste("Subperiod of period 2010.8 is larger than the",
                     "specified frequency 4"))

  # check that the parser still works after errors:
  expect_identical(as.character(period("2011Q3")), "2011Q3")

  msg <- "12 is not divisible by frequency timeseries \\(5\\)."
  expect_error(as.Date(period("2010-3", frequency = 5)), msg)
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

  expect_true(is.na(period(NA, frequency = 12) == period(NA, frequency = 12)))
  expect_false(period(NA, frequency = 12) == period(NA, frequency = 4))
  expect_true(period(NA, frequency = 12) != period(NA, frequency = 4))

  msg <- paste("Illegal logical operator >, only == and != allowed if the two",
               "periods have different frequencies.")
  expect_error(period("2010Q1") > period("2010M1"), msg)

  expect_error(period("2010q1") | period("2010q2"),
               "Operator \\| not implemented for period objects.")
})

test_that("arithmetic operators: only + and - allowed", {
  expect_identical(period("2010q2") + 4, period("2011q2"))
  expect_identical(period("2010") - 4, period("2006"))
  expect_identical(period("2010m2") - 8, period("2009m6"))
  expect_identical(period("2010m2") - period("2009m6"), 8)
  expect_identical(period("2010q2") + 4, 4 + period("2010q2"))

  expect_identical(period("2010m2") + c(1, 4),
                   list(period("2010m3"), period("2010m6")))
  expect_identical(period("2010") - 1:3,
                   list(period("2009"), period("2008"), period("2007")))

  p <- period("2010q1")
  expect_identical(p + NA, period(NA, frequency = 4))
  expect_identical(NA + p, period(NA, frequency = 4))
  expect_identical(period(NA, frequency = 4) + 2, period(NA, frequency = 4))

  expect_identical(period("2010Q1") - period(NA, frequency = 4), NA_real_)

  expect_identical(p + c(1, NA, 0),
                   list(p + 1, period(NA, frequency = 4), p))

  # errors
  expect_error(period("2010Q1") * 2,
               "Operator \\* not implemented for period objects.")
  expect_error(period("2010Q1") + period("2010Q2"),
               "Arithmetic operation \\+ on two periods is not allowed")
  expect_error(period("2010Q1") - period("2010M1"),
               paste("Arithmetic operations on periods with different",
                     "frequencies are not allowed."))
  expect_error(period("2010Q1") + 0.1,
               "Second operand contains non-integer values")
  expect_error(c(5, 5.1) + period("2010Q1"),
               "First operand contains non-integer values")

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
  expect_identical(as.period(as.integer(2010)), period("2010"))
  expect_identical(as.period(2010, frequency = 4), period("2010Q1"))
  expect_identical(as.period(2010.75, frequency = 4), period("2010Q4"))

  expect_error(as.period(2010.75), "Argument frequency should be specified")
  expect_error(as.period(2010.001, frequency = 1),
               "If frequency == 1, then x should be an integer.")
})


test_that("regts:::is_period_text", {
  expect_identical(regts:::is_period_text(c("2060", "noot", "2012M2",
                                            "2010-2", "2010Q3QA"),
                                          frequency = NA),
                   c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(regts:::is_period_text(c("2060", "noot", "2012M2",
                                            "2010-2", "2010Q3QA"),
                                          frequency = NA),
                   c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_true(regts:::is_period_text("2010-2", frequency = 4))
  expect_false(regts:::is_period_text("2010-8", frequency = 4))
  expect_false(regts:::is_period_text("2010Q2", frequency = 1))
  expect_false(regts:::is_period_text("2010Q2", frequency = 12))
  expect_true(regts:::is_period_text("2010Q2", frequency = 4))
  expect_true(regts:::is_period_text("2010-8", frequency = NA))
  expect_false(regts:::is_period_text("2010Q8", frequency = NA))
  expect_false(regts:::is_period_text("2010Q0", frequency = NA))
  expect_true(regts:::is_period_text("2010.0", frequency = NA))
  expect_true(regts:::is_period_text("2010.0", frequency = 1))
  expect_false(regts:::is_period_text("2010.0", frequency = 4))
  expect_true(regts:::is_period_text("2010.25", frequency = NA))
  expect_false(regts:::is_period_text("2010.25", frequency = 1))
  expect_false(regts:::is_period_text(as.character(period(NA_character_, 4)),
                                                   frequency = 1))
})


test_that("c and seq", {

  pq <- period("2018q2")
  py <- period(2016)
  expect_identical(c(pq, py), list(pq, py))
  expect_identical(c(pq, py, 3, "xxx"), list(pq, py, 3, "xxx"))

  p <- period("2018q2")
  expect_identical(seq(p, p + 2), p + 0:2)
  expect_identical(seq(p, p + 8, by = 3), p + c(0, 3, 6))
  expect_identical(seq(p, "2020q2", length.out = 3), p + c(0, 4, 8))

  expect_identical(seq(to = p + 8, by = 3, length.out = 3), p + c(2, 5, 8))
  expect_identical(seq(to = p + 8, length.out = 3), p + c(6, 7, 8))

  # errors
  msg <- paste("The number of periods \\(8\\) between 2018Q2 and 2020Q2",
               "is not divisible by\nlength.out - 1 = 3.")
  expect_error(seq(p, p + 8, length.out = 4), msg)

  expect_error(seq(p, 8), paste("Argument to has a different frequency",
        "\\(1\\) than argument from \\(4\\)"))

  expect_error(seq(p, p + 2, by = 0.5), "Argument by is not an integer")

  expect_error(seq(p, p + 8, by = 3, length.out = 3))

  expect_error(seq(p, "2012m2"), paste("Argument to has a different frequency",
               "\\(12\\) than argument from \\(4\\)"))
})

test_that("as.character and print", {

  # NA timeseries
  py <- period(NA, frequency = 1)
  pm <- period(NA, frequency = 12)
  expect_identical(as.character(py), NA_character_)
  expect_identical(as.character(pm), "NAMNA")
  expect_output(print(py), "\\[1\\] NA")
  expect_output(print(pm), "\\[1\\] \"NAMNA\"")

  py <- period(12, frequency = 1)
  pm <- period(12, frequency = 12)
  expect_identical(as.character(py), "12")
  expect_identical(as.character(pm), "12M01")
  expect_output(print(py), "\\[1\\] 12")
  expect_output(print(pm), "\\[1\\] \"12M01\"")
})

test_that("multiple periods", {

  # create some reference periods
  pq1 <- period("2010q2")
  pq2 <- period("2010q3")
  pq3 <- period(NA, frequency = 4)
  pm1 <- period("2018m3")
  py1 <- period(2010)
  py2 <- period(2011)
  py3 <- period(NA, frequency = 1)

  expect_identical(period(c("2010q2", "2018m3")), list(pq1, pm1))
  expect_identical(period(2010:2011), list(py1, py2))

  expect_identical(period(c(2010, NA, 2011), frequency = 1),
                   list(py1, py3, py2))

  expect_identical(period(c(2010.25, 2010.5, NA), frequency = 4),
                   list(pq1, pq2, pq3))

  expect_error(period(c(2010, 2010.5), frequency = 1),
              "If frequency == 1, then x should be an integer.")

  expect_error(period(c(2010, NA, 2011)),
                      "Argument frequency should be specified.")

  expect_identical(
    period(c(as.Date("2010-04-01"), as.Date("2010-07-01")), frequency = 4),
          list(pq1, pq2))

  # multiple texts
  expect_identical(period(c("2010q2", "2011", "2018m3")), list(pq1, py2, pm1))

  expect_error(period(as.factor(c("2018q1", "2011", "2012m2")), frequency = 4),
               paste("Specified frequency 4 does not agree with actual",
                     "frequency in period 2011."))

  expect_error(period(c("2018q1", NA_character_, "2012m2")),
               "Frequency of NA period unknown. Specify argument frequency.")


})

library(testthat)

context("change_frequency")

test_that("change_frequency.period", {

  p <- period("2017M3")
  expect_identical(as.character(change_frequency(p, 1)), "2017")
  expect_identical(as.character(change_frequency(p, 2)), "2017-1")
  expect_identical(as.character(change_frequency(p, 4)), "2017Q1")
  expect_identical(as.character(change_frequency(p, 12)), "2017M03")

  p <- period("2017M4")
  expect_identical(as.character(change_frequency(p, 1)), "2017")
  expect_identical(as.character(change_frequency(p, 2)), "2017-1")
  expect_identical(as.character(change_frequency(p, 4)), "2017Q2")
  expect_identical(as.character(change_frequency(p, 12)), "2017M04")

  p <- period("2017M8")
  expect_identical(as.character(change_frequency(p, 1)), "2017")
  expect_identical(as.character(change_frequency(p, 2)), "2017-2")
  expect_identical(as.character(change_frequency(p, 4)), "2017Q3")
  expect_identical(as.character(change_frequency(p, 12)), "2017M08")

  msg <- paste0("It is not possible to convert a period to higher frequency.\n",
               "Consider converting it to a period_range.")
  expect_error(change_frequency(p, 24), msg)

  msg <- "Old frequency \\(12\\) not divisible by new frequency \\(5\\)."
  expect_error(change_frequency(p, 5), msg)

  msg <- "Argument 'new_frequency' should be a scalar integer value."
  expect_error(change_frequency(p, "aap"), msg)
  expect_error(change_frequency(p, 1:2), msg)
})

test_that("change_frequency.period_range", {

  r <- period_range("2017M3/2018M5")
  expect_identical(as.character(change_frequency(r, 1)), "2017/2018")
  expect_identical(as.character(change_frequency(r, 2)), "2017-1/2018-1")
  expect_identical(as.character(change_frequency(r, 4)), "2017Q1/2018Q2")
  expect_identical(as.character(change_frequency(r, 12)), "2017M03/2018M05")

  r <- period_range("/2018M8")
  expect_identical(as.character(change_frequency(r, 1)), "/2018")
  expect_identical(as.character(change_frequency(r, 2)), "/2018-2")
  expect_identical(as.character(change_frequency(r, 4)), "/2018Q3")
  expect_identical(as.character(change_frequency(r, 12)), "/2018M08")

  r <- period_range("2017Q3/2018Q2")
  expect_identical(as.character(change_frequency(r, 12)), "2017M07/2018M06")

  r <- period_range("2017Q1/2018Q1")
  expect_identical(as.character(change_frequency(r, 12)), "2017M01/2018M03")

  r <- period_range("2017", "2017")
  expect_identical(as.character(change_frequency(r, 1)), "2017")
  expect_identical(as.character(change_frequency(r, 4)), "2017Q1/2017Q4")
  expect_identical(as.character(change_frequency(r, 12)), "2017M01/2017M12")

  r <- period_range("2017Q1/2017Q2")
  expect_identical(as.character(change_frequency(r, 1)), "2017")

  msg <- "Old frequency \\(4\\) not divisible by new frequency \\(3\\)."
  expect_error(change_frequency(r, 3), msg)

  msg <- "New frequency \\(5\\) not divisible by old frequency \\(4\\)."
  expect_error(change_frequency(r, 5), msg)

  msg <- "Argument 'new_frequency' should be a scalar integer value."
  expect_error(change_frequency(r, "aap"), msg)
  expect_error(change_frequency(r, 1:2), msg)

})


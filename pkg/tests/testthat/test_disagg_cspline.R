library(regts)
library(testthat)

rm(list = ls())

context("disagg with cspline method")

q_per <- period_range("2017Q2/2018Q3")
l <- seq_len(nperiod(q_per))
a_q <- regts(l,  period = q_per)
b_q <- regts(l**2, period = q_per)
ab_q <- cbind(a = a_q, b = b_q)

test_that("example 1", {
  m_per <- period_range("2017m6/2018m9")
  a_ref <- regts(seq(from = 1, to = 6, length.out = nperiod(m_per)),
                 period = m_per)
  b_ref <- a_ref**2
  a_m <- disagg(a_q, nfrequency = 12, constraint = "last",
                 method = "cspline")
  b_m <- disagg(b_q, nfrequency = 12, constraint = "last",
                method = "cspline")
  expect_equal(a_m, a_ref)
  expect_equal(b_m, b_ref)
})

test_that("example with NA values (1)", {
  xyz_q <- cbind(x = 3 * a_q, y = 3 * a_q, z = 3 * a_q)
  xyz_q["2017Q2", "x"] <- NA
  xyz_q["2018Q3", "x"] <- NA
  xyz_q["2018Q1", "y"] <- NA
  xyz_m <- disagg(xyz_q, nfrequency = 12, constraint = "last",
                  method = "cspline")

  expected_result <- regts(matrix(rep(3:18, 3), ncol = 3), start = "2017M6",
                           names = c("x", "y", "z"))
  expected_result["/2017M8", "x"] <- NA
  expected_result["2018M7/", "x"] <- NA
  expected_result[, "y"] <- NA
  expect_equal(xyz_m, expected_result)
})


test_that("few observations", {

  # 1 observation
  a_y <- regts(1, period = "2017")
  expect_error(disagg(a_y, nfrequency = 4, constraint = "last",
                          method = "cspline"),
               "Not enough observations for the nakn method")
})

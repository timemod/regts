library(regts)
library(testthat)

context("disagg")

q_per <- period_range("2017Q2/2018Q3")
l <- seq_len(nperiod(q_per))
a_q <- regts(l,  period = q_per)
b_q <- regts(l**2, period = q_per)
ab_q <- cbind(a = a_q, b = b_q)

test_that("last method, univariate and multivariate", {
  ab_m <- disagg(ab_q, nfrequency = 12, constraint = "last")
  expect_identical(disagg(a_q, nfrequency = 12, constraint = "last"),
                   ab_m[, "a"])
  expect_identical(disagg(ab_q[, "a", drop = FALSE], nfrequency = 12,
                          constraint = "last"),
                   ab_m[, "a", drop = FALSE])
  expect_identical(disagg(b_q, nfrequency = 12, constraint = "last"),
                   ab_m[, "b"])
  m_per <- period_range("2017m6/2018m9")
  a_ref <- regts(seq(from = 1, to = 6, length.out = nperiod(m_per)),
                 period = m_per)
  b_ref <- a_ref**2
  expect_equal(ab_m[, "a"], a_ref)
  expect_equal(ab_m[, "b"], b_ref)

  # test first method
  expect_equal(disagg(a_q, nfrequency = 12, constraint = "first"),
               lag(a_ref, 2))
  expect_equal(disagg(b_q, nfrequency = 12, constraint = "first"),
               lag(b_ref, 2))


  # test other methods
  expect_equal(disagg(b_q, nfrequency = 12, constraint = "last",
                      method = "natural"),
               ab_m[, "b"])
})


test_that("sum and average method", {
  ab_m <- disagg(ab_q, nfrequency = 12)
  expect_equal(disagg(b_q, nfrequency = 12,
                          constraint = "average") / 3,
                   ab_m[, "b"])

  m_per <- period_range("2017m4/2018m9")
  a_ref <- regts(seq(from = 2, to = 19, length.out = nperiod(m_per)),
                 period = m_per) / 9
  expect_equal(ab_m[, "a"], a_ref)

  #print(ab_m[, "b"])
  expect_equal_to_reference(ab_m[, "b"], "diagg_b_sum.rds")

  b_q_agg <- aggregate(ab_m[, "b"], FUN = sum, nfrequency = 4)
  expect_equal(b_q, b_q_agg)
})

test_that("example with NA values (1)", {
  xyz_q <- cbind(x = 3 * a_q, y = 3 * a_q, z = 3 * a_q)
  xyz_q["2017Q2", "x"] <- NA
  xyz_q["2018Q3", "x"] <- NA
  xyz_q["2018Q1", "y"] <- NA
  xyz_m <- disagg(xyz_q, nfrequency = 12, constraint = "last")

  expected_result <- regts(matrix(rep(3:18, 3), ncol = 3), start = "2017M6",
                           names = c("x", "y", "z"))
  expected_result["/2017M8", "x"] <- NA
  expected_result["2018M7/", "x"] <- NA
  expected_result[, "y"] <- NA
  expect_equal(xyz_m, expected_result)

  xyz_m_av <- disagg(xyz_q, nfrequency = 12, constraint = "average")

  expected_result <- regts(matrix(rep(2:19, 3), ncol = 3), start = "2017M4",
                           names = c("x", "y", "z"))
  expected_result["/2017M6", "x"] <- NA
  expected_result["2018M7/", "x"] <- NA
  expected_result[, "y"] <- NA
  expect_equal(xyz_m_av, expected_result)
})

test_that("example with NA values (2)", {
  xyz_q <- cbind(x = 3 * a_q, y = 3 * a_q, z = 3 * a_q)
  xyz_q["2017Q2", "x"] <- NA
  xyz_q["2018Q3", "x"] <- NA
  xyz_q["2018Q1", "y"] <- NA
  xyz_q[, "z"] <- NA
  xyz_m <- disagg(xyz_q, nfrequency = 12, constraint = "last")

  expected_result <- regts(matrix(rep(3:18, 3), ncol = 3), start = "2017M6",
                           names = c("x", "y", "z"))
  expected_result["/2017M8", "x"] <- NA
  expected_result["2018M7/", "x"] <- NA
  expected_result[, "y"] <- NA
  expected_result[, "z"] <- NA
  expect_equal(xyz_m, expected_result)
})

test_that("errors", {
  msg <- "nfrequency \\(10\\) is not an integer multiple of the input frequency \\(4\\)"
  expect_error(disagg(ab_q, nfrequency = 10), msg)
  msg <- "nfrequency \\(3\\) is not larger than the input frequency \\(4\\)"
  expect_error(disagg(ab_q, nfrequency = 3), msg)
  msg <- "Input timeseries contains only NA values"
  expect_error(disagg(ab_q * NA, nfrequency = 12), msg)
})

#
# ts <- regts(1:1, start = "2010Q2")
# disagg(ts, nfrequency = 12, constraint = "last")
# disagg(ts, nfrequency = 12, constraint = "average")


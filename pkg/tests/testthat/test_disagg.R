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


  # natural cubic spline, should give the same result for timeseries a
  # but a different result for timeseries b (which is nonlinear)
  expect_equal(disagg(a_q, nfrequency = 12, constraint = "last",
                      method = "natural"), a_ref)

  expect_false(isTRUE(all.equal(disagg(b_q, nfrequency = 12, constraint = "last",
                      method = "natural"), b_ref)))
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
  expect_known_output(ab_m[, "b"], file.path("expected_output/disagg_b_sum.rds"))

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

test_that("example with NA values (3)", {
  xy_q <- cbind(x = 3 * a_q, y = 3 * a_q)
  xy_q[, "y"] <- NA
  xy_q["2018Q1", "y"] <- 3
  xy_m <- disagg(xy_q, nfrequency = 12, constraint = "last")
  expected_result <- regts(matrix(c(3:18, rep(NA, 16)), ncol = 2),
                           start = "2017M6", names = c("x", "y"))
  expected_result["2018M3", "y"] <- 3
  expect_equal(xy_m, expected_result)

  xy_m_av <- disagg(xy_q, nfrequency = 12, constraint = "average")
  expected_result_av <- regts(matrix(c(2:19, rep(NA, 18)), ncol = 2),
                           start = "2017M4", names = c("x", "y"))
  expected_result_av["2018M1/2018M3", "y"] <- 3
  expect_equal(xy_m_av, expected_result_av)
})

test_that("single period", {
  a <- regts(666, start = "2010Q2")
  a_ref_last <- regts(666, start = "2010M6")
  a_ref_sum <- regts(222, period = "2010M4/2010M6")
  expect_identical(disagg(a, nfrequency = 12, constraint = "last"),
                   a_ref_last)
  expect_equal(disagg(a, nfrequency = 12, constraint = "sum"),
                   a_ref_sum)

  ab <- cbind(a = a, b = 2 * a)
  ab_ref_last <- cbind(a =a_ref_last, b = 2 * a_ref_last)
  ab_ref_sum <- cbind(a = a_ref_sum, b = 2 * a_ref_sum)
  expect_identical(disagg(ab, nfrequency = 12, constraint = "last"),
                   ab_ref_last)
  expect_equal(disagg(ab, nfrequency = 12, constraint = "sum"),
               ab_ref_sum)
})

test_that("errors", {
  msg <- "nfrequency \\(10\\) is not an integer multiple of the input frequency \\(4\\)"
  expect_error(disagg(ab_q, nfrequency = 10), msg)
  msg <- "nfrequency \\(3\\) is not larger than the input frequency \\(4\\)"
  expect_error(disagg(ab_q, nfrequency = 3), msg)
  msg <- "Input timeseries contains only NA values"
  expect_error(disagg(ab_q * NA, nfrequency = 12), msg)
})

test_that("year to quarter", {

  y_per <- period_range("2017Y/2040Y")
  n_y <- nperiod(y_per)
  a_y <- regts(seq_len(n_y), period = y_per)
  a_q <- disagg(a_y, nfrequency = 4, constraint = "last", method = "natural")

  q_per <- period_range("2017Q4/2040Q4")
  n_q <- nperiod(q_per) / 4
  a_q_ref <- regts(seq(1, 24, by = 0.25), period = q_per)

  expect_equal(a_q, a_q_ref)
})

test_that("year to month, sine series", {
  y_per <- period_range("2017Y/2130Y")
  n_y <- nperiod(y_per)
  data <- 1 +  sin((seq_len(n_y) - 1) * 2 * pi / (n_y - 1))
  a_y <- regts(data, period = y_per)

  a_m <- disagg(a_y, nfrequency = 12, constraint = "first")

  m_per <- period_range("2017M1/2130M1")
  n_m <- nperiod(m_per)
  data <- 1 +  sin((seq_len(n_m) - 1) * 2 * pi / (n_m - 1))
  a_m_ref <- regts(data, period = m_per)

  expect_equal(a_m, a_m_ref)

  expect_equal(a_y, aggregate(a_m["2017M1/2130M12"], nfrequency = 1,
                              FUN = function(x) {x[1]}))
})

library(testthat)
library(regts)


rm(list = ls())

update_expected <- FALSE

test_that("yearly univariate series", {
  y_ts <- regts(1:2, start = 2016, labels = "xxx")
  expect_known_output(print(y_ts), "expected_output/print_y_ts.txt",
                      update = update_expected)
  expect_known_output(print(y_ts["2016"]), "expected_output/print_y_ts_sel.txt",
                      update = update_expected)
  expect_known_output(printobj(y_ts["2016"]),
                      "expected_output/printobj_y_ts_sel.txt",
                      update = update_expected)
  y_ts2 <- regts(1:20, start = 2016, labels = "xxx")
  expect_known_output(print(y_ts2), "expected_output/print_y_ts2.txt",
                      update = update_expected)
})

test_that("quarterly series", {
  q_ts <- regts(1:2, start = "2016Q2", labels = "xxx")
  expect_known_output(print(q_ts), "expected_output/print_q_ts.txt",
                      update = update_expected)
  q_mts <- cbind(q_ts, q_ts2 = 2 * q_ts)
  expect_known_output(print(q_mts), "expected_output/print_q_mts.txt",
                      update = update_expected)
  expect_known_output(print(q_mts["2016Q3"]),
                      "expected_output/print_q_mts_sel.txt",
                      update = update_expected)
})

test_that("frequency 3", {
  f3_ts <- regts(1:2, start = "2016-2", frequency = 3)
  expect_known_output(print(f3_ts), "expected_output/print_f3_ts.txt",
                      update = update_expected)
})



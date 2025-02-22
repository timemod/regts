library(regts)
library(testthat)

rm(list = ls())


update_expected <- FALSE

prd <- period_range("2010Q2/2010Q3")
num_dec <- 1.123456789
correct_result <- regts(matrix(c(num_dec, 1, NA, num_dec), ncol = 2),
                        names = c("a", "b"),
                        labels = paste("Timeseries", c("a", "b")),
                        period =  prd)

test_that("example6.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example6.xlsx"

  expect_warning(result <- read_ts_xlsx(xlsx_file, labels = "after"),
                 "Expecting numeric in C4 / R4C3: got 'x'")
  expect_identical(result, correct_result)
})

test_that("example7.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example7.xlsx"
  expect_warning(result <- read_ts_xlsx(xlsx_file),
                 "Expecting numeric in C3 / R3C3: got 'x'")
  expect_identical(result, correct_result)
})

test_that("example9.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example9.xlsx"
  warnings <- capture_warnings(result <- read_ts_xlsx(xlsx_file,
                                                      labels = "after"))
  expect_identical(warnings,
                   c("Expecting numeric in D2 / R2C4: got 'x'",
                     "Coercing text to numeric in D3 / R3C4: '10'"))
  expected_result <- regts(matrix(c(rep(NA, 3), 5, 6, 10, NA, NA, 50, 60),
                                  ncol = 2), names = c("A", "B"),
                          labels = paste("Timeseries", c("a", "b (EUR)")),
                          period =  "2010q2/2011q2")
  expect_identical(result, expected_result)
})

test_that("example10.xlsx is read correctly",  {
  xlsx_file <- "xlsx/example10.xlsx"
  warnings1 <- capture_warnings(
    result1 <- read_ts_xlsx(xlsx_file, labels = "after",
                                        skipcol = 1, strict = FALSE)
  )

  expected_result <- regts(matrix(c(1.12345678901234, NA, NA, 5, 6,
                                    10.123, NA, NA, 50, 60), ncol = 2),
                           names = c("a", "b"),
                           labels = paste("Timeseries", c("a", "b (EUR)")),
                           period =  "2010q2/2011q2")
  expect_equal(result1, expected_result)
  expect_known_output(warnings1, "expected_output/example10_warn1.txt",
                      print = TRUE, update = update_expected)

  warnings2 <- capture_warnings(
    result2 <- read_ts_xlsx(xlsx_file, labels = "after", skipcol = 1,
                            strict = FALSE, warn_num_text = FALSE)
  )
  expect_equal(result2, expected_result)
  expect_known_output(warnings2, "expected_output/example10_warn2.txt",
                      print = TRUE, update = update_expected)
})

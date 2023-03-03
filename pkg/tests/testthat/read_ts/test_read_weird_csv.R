library(regts)
library(testthat)


test_that("weird_1.csv is read correctly",  {

  csv_file <- "csv/weird_1.csv"
  result1 <- read_ts_csv(csv_file)

  expected_result <- regts(matrix(as.numeric(1:2), ncol =2), names = c("a", "b"),
                           start = "2011")
  expect_identical(result1, expected_result)
})

test_that("weird_2.csv is read correctly",  {

  csv_file <- "csv/weird_2.csv"
  msg <- paste0("NAs introduced by coercion.\n",
                "11 texts could not be converted to numeric.\n",
                "The first 10 texts that gave problems are:\n",
                "\"x1\"\n",
                "\"x2\"\n",
                "\"x3\"\n",
                "\"x4\"\n",
                "\"x5\"\n",
                "\"x6\"\n",
                "\"x7\"\n",
                "\"x8\"\n",
                "\"x9\"\n",
                "\"x10\"")

  expect_warning(result1 <- read_ts_csv(csv_file), msg)

  expected_result <- regts(matrix(NA, ncol = 1), names = "3x",
                           period = "2010q1/2013q1")
  expected_result[1, 1] <- 1
  expect_identical(result1, expected_result)
})

test_that("weird_3.csv is read correctly",  {

  csv_file <- "csv/weird_3.csv"
  msg <- paste0("NAs introduced by coercion.\n",
                "The following texts could not be converted to numeric:\n",
                "\"x\"\n",
                "\"y\"\n",
                "\"z\"")

  expect_warning(result1 <- read_ts_csv(csv_file), msg)

  expected_result <- regts(matrix(NA, ncol = 1), names = "3x",
                           period = "2010q1/2013q1")
  expected_result["2013Q1"] <- 3
  expect_identical(result1, expected_result)
})

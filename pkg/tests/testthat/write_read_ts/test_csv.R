library(regts)
library(testthat)


if (!dir.exists("csv")) {
  # this could is needed if we run the test with Travis CI,
  # even though csv/.gitignore is part of the repo.
  dir.create("csv")
}

prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5.25, 6), period =  prd)
b <- 10 * a
ts1 <- cbind(a, b)

ts1_lbls <- ts1
ts_labels(ts1_lbls) <- paste(c("Timeseries"), colnames(ts1))

test_that("ts without labels written correctly",  {

  file <- "csv/ts1.csv"
  write_ts_csv(ts1, file = file, labels = "after")
  ts1_read <- read_ts_csv(file)
  expect_identical(ts1, ts1_read)

  file <- "csv/ts1_t.csv"
  msg <- paste0(
    "^\nWriting timeseries to file csv/ts1_t\\.csv ...\n",
    ".*", # this is necessary when running via RStudio
    "2 timeseries written, period range 2010Q2/2011Q2, 0\\.\\d{2} sec\\. elapsed\\.\\n$"
  )
  expect_output({
    write_ts_csv(ts1, file = file, rowwise = FALSE, sep = ";", dec = ",",
                 verbose = TRUE)
  }, msg)

  ts1_t_read <- read_ts_csv(file, dec = ",")
  expect_identical(ts1, ts1_t_read)
})

test_that("ts with empty labels written correctly",  {
  ts1_el <- ts1
  ts_labels(ts1_el) <- rep("", 2)

  file <- "csv/ts1_el.csv"
  write_ts_csv(ts1_el, file = file)
  ts1_el_read <- read_ts_csv(file, labels = "no")
  expect_identical(ts1, ts1_el_read)

  file <- "csv/ts1_el_t.csv"
  write_ts_csv(ts1_el, file = file, rowwise = FALSE, sep = ";", dec = ",")
  ts1_el_t_read <- read_ts_csv(file, dec = ",")
  expect_identical(ts1, ts1_el_t_read)
})

test_that("file with labels with label option after written correctly",  {

  file <- "csv/ts1_lbls_after.csv"
  write_ts_csv(ts1_lbls, file = file)
  ts1_read <- read_ts_csv(file, labels = "after")
  expect_identical(ts1_lbls, ts1_read)

  file <- "csv/ts1_lbls_before_t.csv"
  write_ts_csv(ts1_lbls, file = file, rowwise = FALSE)
  ts1_t_read <- read_ts_csv(file, labels = "after")
  expect_identical(ts1_lbls, ts1_t_read)
})

test_that("file with labels with label option before written correctly",  {

  file <- "csv/ts1_lbls_before.csv"
  write_ts_csv(ts1_lbls, file = file, labels = "before")
  ts1_read <- read_ts_csv(file, labels = "before")
  expect_identical(ts1_lbls, ts1_read)

  file <- "csv/ts1_lbls_before_t.csv"
  write_ts_csv(ts1_lbls, file = file, rowwise = FALSE, labels = "after")
  ts1_t_read <- read_ts_csv(file, labels = "after")
  expect_identical(ts1_lbls, ts1_t_read)
})

test_that("file with labels with label option no written correctly",  {

  file <- "csv/ts1_lbls_no.csv"
  write_ts_csv(ts1_lbls, file = file, labels = "no")
  ts1_read <- read_ts_csv(file)
  expect_identical(ts1, ts1_read)

  file <- "csv/ts1_lbls_no_t.csv"
  write_ts_csv(ts1_lbls, file = file, rowwise = FALSE, labels = "no")
  ts1_t_read <- read_ts_csv(file)
  expect_identical(ts1, ts1_read)
})

test_that("univariate timeseries", {
  a_mat <- ts1_lbls[, "a", drop = FALSE]
  write_ts_csv(a_mat, file = "csv/ts1_unimat.csv", rowwise = FALSE)
  a <- ts1_lbls[, "a"]
  write_ts_csv(a, file = "csv/ts1_univec.csv")
  expect_identical(a_mat, read_ts_csv("csv/ts1_unimat.csv", labels = "after"))
  expect_identical(a_mat, read_ts_csv("csv/ts1_univec.csv", labels = "after"))
})

test_that("no column names", {
  tmp <- ts1_lbls
  colnames(tmp) <- NULL
  write_ts_csv(tmp, file = "csv/ts1_no_colnames.csv")
  expected_result <- tmp
  colnames(expected_result) <- c("series1", "series2")
  expect_identical(expected_result, read_ts_csv("csv/ts1_no_colnames.csv",
                                                labels = "after"))
})


test_that("period_format", {

  file <- "csv/ts1_period_format.csv"
  write_ts_csv(ts1_lbls, file, labels = "before", period_format = "%Y-%m-%d")

  period_fun <- function(x) {
    x <- as.Date(x, format = "%Y-%m-%d")
    return(period(x, frequency = 4))
  }

  ts_read <- read_ts_csv(file, period_fun = period_fun, labels = "before")
  expect_identical(ts1_lbls, ts_read)

  file <- "csv/ts1_period_format_t.csv"
  write_ts_csv(ts1_lbls, file, labels = "before", rowwise = FALSE,
               period_format = "%Y-%m-%d")

  ts_read <- read_ts_csv(file, period_fun = period_fun, labels = "before")

  expect_identical(ts1_lbls, ts_read)

  # incorrect period_fun

  period_fun_err1 <- function(x) {
    return("xxx")
  }

  expect_error(read_ts_csv(file, period_fun = period_fun_err1),
               paste("Function period_fun should return an object with the",
                     "same length as its input value."))

  period_fun_err2 <- function(x) {
    return(rep(2, length(x)))
  }

  emsg <- "Period_fun should return a character or period vector."
  expect_error(read_ts_csv(file, period_fun = period_fun_err2),
               emsg)
})





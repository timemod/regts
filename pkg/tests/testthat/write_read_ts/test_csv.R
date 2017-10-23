library(regts)
library(testthat)

context("write_read_ts_csv")

if (!dir.exists("csv")) {
  stop("directory csv does not exist")
}

prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5.25, 6), period =  prd)
b <- 10 * a
ts1 <- cbind(a, b)

ts1_lbls <- ts1
ts_labels(ts1_lbls) <- paste(c("Timeseries"), colnames(ts1))

test_that("ts with labels written correctly",  {

  file <- "csv/ts1.csv"
  write_ts_csv(ts1, file = file, labels = "after")
  ts1_read <- read_ts_csv(file)
  expect_identical(ts1, ts1_read)

  file <- "csv/ts1_t.csv"
  write_ts_csv(ts1, file = file, rowwise = FALSE, sep = ";", dec = ",")
  ts1_t_read <- read_ts_csv(file, dec = ",")
  expect_identical(ts1, ts1_t_read)
})

test_that("file with labels with label option after written correctly",  {

  file <- "csv/ts1_lbls_after.csv"
  write_ts_csv(ts1_lbls, file = file)
  ts1_read <- read_ts_csv(file, labels = "after")
  expect_identical(ts1_lbls, ts1_read)

  file <- "csv/ts1_lbls_before_t.csv"
  write_ts_csv(ts1_lbls, file = file, rowwise = FALSE)
  ts1_t_read <- read_ts_csv(file, labels = "before")
  expect_identical(ts1_lbls, ts1_t_read)
})

test_that("file with labels with label option before written correctly",  {

  file <- "csv/ts1_lbls_before.csv"
  write_ts_csv(ts1_lbls, file = file, labels = "before")
  ts1_read <- read_ts_csv(file, labels = "before")
  expect_identical(ts1_lbls, ts1_read)

  file <- "csv/ts1_lbls_before_t.csv"
  msg = "For columnwise timeseries labels option \"before\" is not allowed"
  expect_error(write_ts_csv(ts1_lbls, file = file, rowwise = FALSE,
                            labels = "before"), msg = msg)
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






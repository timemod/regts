library(regts)
library(testthat)


if (!dir.exists("xlsx")) {
  # this could is needed if we run the test with Travis CI,
  # even though xlsx/.gitignore is part of the repo.
  dir.create("xlsx")
}

prd <- period_range("2010/2015")
a <- regts(c(1, NA, NA, 5.25, 6), period =  prd)
b <- 10 * a
ts2 <- cbind(a, b)

ts2_lbls <- ts2
ts_labels(ts2_lbls) <- paste(c("Timeseries"), colnames(ts2))

test_that("ts without labels written correctly",  {

  file <- "xlsx/ts2.xlsx"

  # make sure that the file is simply overwritten
  writeLines(c("Hello","World"), con = file)

  write_ts_xlsx(ts2, file, sheet_name = "ts2", labels = "after")

  comments <- c("This is a transposed timeseries", "")
  write_ts_xlsx(ts2, file, sheet_name = "ts2_t",  rowwise = FALSE,
                comments = comments, append = TRUE)

  wb <- openxlsx::loadWorkbook(file)
  comments <- data.frame(col1 = "Timeseries ts2",
                         col2 = "(times 2)")
  write_ts_sheet(ts2 * 2, wb, sheet_name = "ts2_times_2",
                 comments = comments, rowwise = TRUE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  ts2_read <- read_ts_xlsx(file, sheet = "ts2")
  expect_identical(ts2, ts2_read)

  ts2_t_read <- read_ts_xlsx(file, sheet = "ts2_t", skiprow = 2)
  expect_identical(ts2, ts2_t_read)

  ts2_times_2_read <- read_ts_xlsx(file, sheet = "ts2_times_2", skiprow = 1)
  expect_identical(ts2 * 2, ts2_times_2_read)
})

test_that("ts with labels written correctly",  {

  file <- "xlsx/ts2_lbls.xlsx"
  if (file.exists(file)) {
    ok <- file.remove(file)
  }

  write_ts_xlsx(ts2_lbls, file, sheet_name = "ts2",
                append = TRUE, number_format = "00.00")
  write_ts_xlsx(ts2_lbls, file, sheet_name = "ts2_t",  rowwise = FALSE,
                append = TRUE, number_format = "#.000")

  ts2_read <- read_ts_xlsx(file, sheet = "ts2", labels = "after")

  expect_identical(ts2_lbls, ts2_read)

  ts2_t_read <- read_ts_xlsx(file, sheet = "ts2_t", labels = "after")

  expect_identical(ts2_lbls, ts2_t_read)

  # check that all sheet names still exists
  wb <- openxlsx::loadWorkbook(file)
  sheetnames <- names(wb)
  expect_identical(sheetnames, c("ts2", "ts2_t"))
})


library(regts)
library(testthat)

context("write_read_ts_xlsx")

prd <- period_range("2010Q2/2011Q2")
a <- regts(c(1, NA, NA, 5.25, 6), period =  prd)
b <- 10 * a
ts1 <- cbind(a, b)

ts1_lbls <- ts1
ts_labels(ts1_lbls) <- paste(c("Timeseries"), colnames(ts1))

test_that("ts without labels written correctly",  {

  file <- "xlsx/ts1.xlsx"

  # make sure that the file is simply overwritten
  writeLines(c("Hello","World"), con = file)

  write_ts_xlsx(ts1, file, sheet_name = "ts1", labels = "after")
  write_ts_xlsx(ts1, file, sheet_name = "ts1_t",  rowwise = FALSE,
                append = TRUE)

  wb <- loadWorkbook(file)
  sheet <- createSheet(wb, "ts1_times_2")
  write_ts_sheet(ts1 * 2, sheet,  rowwise = TRUE)
  saveWorkbook(wb, file)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1")
  expect_identical(ts1, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t")
  expect_identical(ts1, ts1_t_read)

  ts1_times_2_read <- read_ts_xlsx(file, sheet = "ts1_times_2")
  expect_identical(ts1 * 2, ts1_times_2_read)
})

test_that("ts with labels written correctly",  {

  file <- "xlsx/ts1_lbls.xlsx"
  file.copy("xlsx_org/ts1_lbls.xlsx", file)

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1", append = TRUE)
  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1_t",  rowwise = FALSE,
                append = TRUE)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")
  expect_identical(ts1_lbls, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "after")
  expect_identical(ts1_lbls, ts1_t_read)

  # check that all sheet names still exists
  wb <- loadWorkbook(file)
  sheetnames <- names(getSheets(wb))
  expect_identical(sheetnames, c("sheet_before", "ts1", "ts1_t", "sheet_after"))

  df1 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_before"))
  expect_equal(df1, data.frame(x = 10, y = 20))

  df2 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_after"))
  expect_equal(df2, data.frame(x = 1, y = 2))
})



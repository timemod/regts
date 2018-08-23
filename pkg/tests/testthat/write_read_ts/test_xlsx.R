library(regts)
library(testthat)

context("write_read_ts_xlsx")

if (!dir.exists("xlsx")) {
  # this could is needed if we run the test with Travis CI,
  # even though xlsx/.gitignore is part of the repo.
  dir.create("xlsx")
}

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

  comments <- c("This is a transposed timeseries", "")
  write_ts_xlsx(ts1, file, sheet_name = "ts1_t",  rowwise = FALSE,
                comments = comments, append = TRUE)

  wb <- loadWorkbook(file)
  sheet <- createSheet(wb, "ts1_times_2")
  comments <- data.frame(col1 = "Timeseries ts1",
                         col2 = "(times 2)")
  write_ts_sheet(ts1 * 2, sheet,  comments = comments, rowwise = TRUE)
  saveWorkbook(wb, file)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1")
  expect_identical(ts1, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", skiprow = 2)
  expect_identical(ts1, ts1_t_read)

  ts1_times_2_read <- read_ts_xlsx(file, sheet = "ts1_times_2", skiprow = 1)
  expect_identical(ts1 * 2, ts1_times_2_read)
})

test_that("ts with labels written correctly",  {

  file <- "xlsx/ts1_lbls.xlsx"
  file.copy("xlsx_org/ts1_lbls.xlsx", file, overwrite = TRUE)

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1", append = TRUE,
                number_format = "00.00")
  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1_t",  rowwise = FALSE,
                append = TRUE, number_format = "#.000")

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")

  expect_identical(ts1_lbls, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "before")

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

test_that("univariate timeseries", {
  a_mat <- ts1_lbls[, "a", drop = FALSE]
  write_ts_xlsx(a_mat, file = "xlsx/ts1_unimat.xlsx", rowwise = FALSE)
  a <- ts1_lbls[, "a"]
  write_ts_xlsx(a, file = "xlsx/ts1_univec.xlsx")
  expect_identical(a_mat, read_ts_xlsx("xlsx/ts1_unimat.xlsx", labels = "before"))
  expect_identical(a_mat, read_ts_xlsx("xlsx/ts1_univec.xlsx", labels = "after"))
})

test_that("period_as_date", {
  file <- "xlsx/ts1_date.xlsx"
  write_ts_xlsx(ts1_lbls, file, labels = "after", period_as_date = TRUE)

  # check if the file really contains date objects
  first_row <- readxl::read_xlsx(file, n_max = 1, col_types = "list",
                                  col_names = FALSE)
  periods <- first_row[-c(1,2)]
  names(periods) <- NULL
  is_posixt <- sapply(periods, FUN = function(x) {inherits(x[[1]], "POSIXt")},
                      USE.NAMES = FALSE)
  expect_identical(is_posixt, rep(TRUE, 5))

  # try to read the file with read_ts_xlsx
  ts1_date_read <- read_ts_xlsx(file, labels = "after", frequency = 4)

  expect_identical(ts1_lbls, ts1_date_read)

  file <- "xlsx/ts1_date_t.xlsx"
  write_ts_xlsx(ts1_lbls, file, labels = "before", rowwise = FALSE,
                period_as_date = TRUE)

  # check if the file really contains date objects
  df <- readxl::read_xlsx(file, skip = 2)
  expect_identical(class(df[[1]]), c("POSIXct", "POSIXt"))

  ts1_date_t_read <- read_ts_xlsx(file, labels = "before", frequency = 4)

  expect_identical(ts1_lbls, ts1_date_t_read)


})



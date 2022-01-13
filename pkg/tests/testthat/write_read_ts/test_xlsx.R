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

  wb <- openxlsx::loadWorkbook(file)
  # the next statement is redundant, but lets check it
  openxlsx::addWorksheet(wb, "ts1_times_2")
  comments <- data.frame(col1 = "Timeseries ts1",
                         col2 = "(times 2)")
  write_ts_sheet(ts1 * 2, wb, sheet_name = "ts1_times_2",
                 comments = comments, rowwise = TRUE)
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1")
  expect_identical(ts1, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", skiprow = 2)
  expect_identical(ts1, ts1_t_read)

  ts1_times_2_read <- read_ts_xlsx(file, sheet = "ts1_times_2", skiprow = 1)
  expect_identical(ts1 * 2, ts1_times_2_read)
})

test_that("ts with empty labels written correctly",  {
  ts1_el <- ts1
  ts_labels(ts1_el) <- rep("", 2)

  file <- "xlsx/ts1_el.xlsx"
  write_ts_xlsx(ts1_el, file = file)
  ts1_el_read <- read_ts_xlsx(file, labels = "no")
  expect_identical(ts1, ts1_el_read)

  file <- "xlsx/ts1_el_t.xlsx"
  write_ts_xlsx(ts1_el, file = file, rowwise = FALSE)
  ts1_el_t_read <- read_ts_xlsx(file)
  expect_identical(ts1, ts1_el_t_read)
})


test_that("ts with labels written correctly (1)",  {

  file <- "xlsx/ts1_lbls.xlsx"
  file.copy("xlsx_org/ts1_lbls.xlsx", file, overwrite = TRUE)

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1", append = TRUE,
                number_format = "00.00")

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1_t",  rowwise = FALSE,
                append = TRUE, number_format = "#.000")

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")

  expect_identical(ts1_lbls, ts1_read)

  expect_error(
    dum <- read_ts_xlsx(file, sheet = "ts1", labels = "rig"))

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "after")

  expect_identical(ts1_lbls, ts1_t_read)

  # check that all sheet names still exists
  wb <- openxlsx::loadWorkbook(file)
  sheetnames <- names(wb)

  expect_identical(sheetnames, c("sheet_before", "ts1", "ts1_t",
                                 "sheet_after"))

  df1 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_before"))
  expect_equal(df1, data.frame(x = 10, y = 20))

  df2 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_after"))
  expect_equal(df2, data.frame(x = 1, y = 2))
})


test_that("ts with labels written correctly (2)",  {

  file <- "xlsx/ts1_lbls_2.xlsx"
  file.copy("xlsx_org/ts1_lbls.xlsx", file, overwrite = TRUE)

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1_t",  rowwise = FALSE,
                append = TRUE, number_format = "#.000")

  write_ts_xlsx(ts1_lbls, file, sheet_name = "extra", append = TRUE,
                number_format = "00.00")

  write_ts_xlsx(ts1_lbls, file, sheet_name = "ts1", append = TRUE,
                number_format = "00.00")

  expect_warning(
    expect_error(
      write_ts_xlsx(ts1_lbls, file = "x/y/dummy.xlsx",
                    sheet_name = "ts1",
                    number_format = "00.00"),
      regexp = "Failed to save workbook to file 'x/y/dummy.xlsx'. Check warnings.",
      fixed = TRUE
    )
  )

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")

  expect_identical(ts1_lbls, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "after")

  expect_identical(ts1_lbls, ts1_t_read)

  # check that all sheet names still exists
  wb <- openxlsx::loadWorkbook(file)
  sheetnames <- names(wb)

  expect_identical(sheetnames, c("sheet_before", "ts1", "ts1_t",
                                 "sheet_after", "extra"))

  df1 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_before"))
  expect_equal(df1, data.frame(x = 10, y = 20))

  df2 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_after"))
  expect_equal(df2, data.frame(x = 1, y = 2))
})



test_that("ts with labels written correctly (3)",  {

  file <- "xlsx/ts1_lbls_3.xlsx"

  wb <- openxlsx::loadWorkbook("xlsx_org/ts1_lbls.xlsx")

  write_ts_sheet(ts1_lbls, wb, sheet_name = "ts1",
                 number_format = "00.00")

  openxlsx::addWorksheet(wb, "dummy")

  write_ts_sheet(ts1_lbls, wb, sheet_name = "ts1_t",  rowwise = FALSE,
                 number_format = "#.000")

  openxlsx::saveWorkbook(wb, file, overwrite  = TRUE)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")

  expect_identical(ts1_lbls, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "after")

  expect_identical(ts1_lbls, ts1_t_read)

  # check that all sheet names still exists
  wb <- openxlsx::loadWorkbook(file)
  sheetnames <- names(wb)

  expect_identical(sheetnames, c("sheet_before", "ts1", "ts1_t",
                                 "sheet_after", "dummy"))

  df1 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_before"))
  expect_equal(df1, data.frame(x = 10, y = 20))

  df2 <- as.data.frame(readxl::read_excel(file, sheet = "sheet_after"))
  expect_equal(df2, data.frame(x = 1, y = 2))
})



test_that("ts with labels written correctly (4)",  {

  file <- "xlsx/ts1_lbls_4.xlsx"

  wb <- openxlsx::loadWorkbook("xlsx_org/ts1_lbls.xlsx")

  write_ts_sheet(ts1_lbls, wb, sheet_name = "ts1_t",  rowwise = FALSE,
                 number_format = "#.000")

  openxlsx::addWorksheet(wb, "dummy")

  write_ts_sheet(ts1_lbls, wb, sheet_name = "ts1",
                 number_format = "00.00")


  openxlsx::saveWorkbook(wb, file, overwrite  = TRUE)

  ts1_read <- read_ts_xlsx(file, sheet = "ts1", labels = "after")

  expect_identical(ts1_lbls, ts1_read)

  ts1_t_read <- read_ts_xlsx(file, sheet = "ts1_t", labels = "after")

  expect_identical(ts1_lbls, ts1_t_read)

  # check that all sheet names still exists
  wb <- openxlsx::loadWorkbook(file)
  sheetnames <- names(wb)

  expect_identical(sheetnames, c("sheet_before", "ts1", "ts1_t",
                                 "sheet_after", "dummy"))

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
  expect_identical(a_mat, read_ts_xlsx("xlsx/ts1_unimat.xlsx", labels = "after"))
  expect_identical(a_mat, read_ts_xlsx("xlsx/ts1_univec.xlsx", labels = "after"))
})

test_that("period_as_date", {
  file <- "xlsx/ts1_date.xlsx"
  write_ts_xlsx(ts1_lbls, file, labels = "after", period_as_date = TRUE)

  # check if the file really contains date objects
  first_row <- readxl::read_xlsx(file, n_max = 1, col_types = "list",
                                  col_names = FALSE,
                                 .name_repair = function(x) {x})
  periods <- first_row[-c(1,2)]
  is_posixt <- sapply(periods, FUN = function(x) {inherits(x[[1]], "POSIXt")},
                      USE.NAMES = FALSE)
  names(is_posixt) <- NULL
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

test_that("errors", {
  file <- "xxlsx/ts1_date.xlsx"

  msg1 <- "cannot create file 'xxlsx/ts1_date.xlsx', reason 'No such file or directory'"
  msg2 <-  "Failed to save workbook to file 'xxlsx/ts1_date.xlsx'\\. Check warnings\\."

  minWidth_old <- options("openxlsx.minWidth")[[1]]
  options("openxlsx.minWidth" = 111)

  if (packageVersion("openxlsx") > "4.2.4") {
    # different error handling since openxslx 4.2.5
    expect_error(
      expect_warning(
        write_ts_xlsx(ts1_lbls, file, labels = "after", period_as_date = TRUE),
        msg1),
      msg2
     )
  } else {
    expect_error(
      write_ts_xlsx(ts1_lbls, file, labels = "after", period_as_date = TRUE),
      msg1)
  }

  expect_equal(options("openxlsx.minWidth")[[1]], 111)

  options("openxlsx.minWidth" = minWidth_old)
})

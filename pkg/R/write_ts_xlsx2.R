#' @export
write_ts_xlsx2 <- function(x, file, sheet_name = "Sheet1",
                          rowwise = TRUE, append = FALSE,
                          labels = c("after", "before", "no"), comments,
                          number_format, period_as_date = FALSE) {

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  if (!file.exists(file)) {
    append <- FALSE
  }

  if (append) {
    wb <- openxlsx::loadWorkbook(file)
  } else {
    if (file.exists(file)) {
      unlink(file)
    }
    wb <- openxlsx::createWorkbook()
  }

  if (append) {
    sheet_names <- openxlsx::getSheetNames()
    if (sheet_name %in% sheet_names) {
      openxlsx::removeWorksheet(wb, sheet_name)
    }
  }
  openxlsx::addWorksheet(wb, sheetName = sheet_name, gridLines = TRUE)

  write_ts_sheet2_(x, wb, sheet_name, rowwise = rowwise,
                  labels = labels, labels_missing = missing(labels), comments,
                  number_format, period_as_date = period_as_date)

  openxlsx::saveWorkbook(wb, file)

  return(invisible(NULL))
}

#' @export
write_ts_sheet2 <- function(x, wb, sheet,  rowwise = TRUE,
                           labels = c("after", "before", "no"),
                           comments, number_format, period_as_date = FALSE) {

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  write_ts_sheet2_(x, wb, sheet, rowwise = rowwise, labels = labels,
                  labels_missing = missing(labels), comments, number_format,
                  period_as_date = period_as_date)

}

# internal function to write a timeseries object to a sheet of an Excel workbook
write_ts_sheet2_ <- function(x, wb, sheet_name, rowwise, labels, labels_missing,
                            comments, number_format, period_as_date) {

  # check for comments. The comments are actually written before the
  # autoSizeColumns() command has been executed.
  if (missing(comments)) {
    n_comment_rows <- 0
  } else {
    comments <- as.data.frame(comments)
    n_comment_rows <- nrow(comments)
  }

  dataframes  <- ts2df_(x, rowwise, labels, labels_missing, "regts",
                        period_as_date)
  data <- dataframes$data
  column_headers <- dataframes$column_headers
  has_labels <- dataframes$has_labels

  if (rowwise) {
    n_text_cols <- 1 + as.integer(has_labels)
  } else {
    n_text_cols <- 1
  }
  n_text_rows <- nrow(column_headers)
  row_split <- n_text_rows + 1 + n_comment_rows
  col_split <- n_text_cols + 1

  # convert strings representing years to numeric
  if (rowwise && frequency(x) == 1) {
    col_sel <- - seq_len(n_text_cols)
    column_headers[, col_sel] <-
      as.data.frame(lapply(column_headers[, col_sel], FUN = as.numeric))
  }

  # Write the column headers. Use right alignment for the column headers
  # of data columns, except if period_as_date has been used.
  openxlsx::writeData(wb, sheet_name, column_headers, colNames = FALSE,
                      rowNames = FALSE, startRow = n_comment_rows + 1)
  if (!period_as_date) {
    style <- openxlsx::createStyle(halign = "right")
    openxlsx::addStyle(wb, sheet_name, style = style,
                       rows = n_comment_rows + 1,
                       cols = seq_along(column_headers),
                       gridExpand = TRUE)
  }


  # now write the data part


  if (!rowwise && frequency(x) == 1) {
    # convert strings representing years to numeric
    data[1] <- as.numeric(data[[1]])
  }

  start_row <- n_text_rows + n_comment_rows + 1
  openxlsx::writeData(wb, sheet_name, data, colNames = FALSE, rowNames = FALSE,
               startRow = start_row)
  if (!missing(number_format)) {
    style <- openxlsx::createStyle(numFmt = number_format)
    openxlsx::addStyle(wb, sheet_name, style = style,
             rows = start_row - 1+ seq_len(nrow(data)), cols = 2 : ncol(data),
             gridExpand = TRUE)
  }


  if (!missing(comments)) {
     openxlsx::writeData(wb, sheet_name, comments, colNames = FALSE,
                         rowNames = FALSE)
  }

  openxlsx::freezePane(wb, sheet_name, firstActiveRow = row_split,
             firstActiveCol = col_split)


  return(invisible(NULL))
}

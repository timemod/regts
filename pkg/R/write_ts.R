#' Write timeseries to a csv file
#'
#' This function writes timeseries to a csv file.
#' The csv file is actually written by function \code{\link[data.table]{fwrite}}
#' of package \code{data.table}.
#' @param x a \code{\link{ts}} or \code{\link{regts}} object
#' @param file a \code{regts} object
#' @param rowwise a logical value: should the timeseries be written rowwise?
#' @param sep The separator between columns. Default is ",".
#' @param dec The decimal separator, by default ".". Cannot be the same as sep.
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present.
#' @param period_format The period format. By default the
#' \code{regts} format (e.g. \code{"2010Q2"}, see \code{\link{period}}) is used.
#' Alternatively, it is possible to specify a format employed by base R function
#' \code{\link[base]{strptime}}, e.g. \code{"\%Y-\%m-\%d"}.
#' @importFrom data.table fwrite
#' @examples
#' # create two timeseries objects
#' ts1 <- regts(matrix(rnorm(50), ncol =  2), names = c("a", "b"),
#'              labels = c("Timeseries a", "Timeseries b"), start = "2017Q2")
#'
#' # write timeseries to csv
#' write_ts_csv(ts1, file = "ts1.csv", labels = "after")
#'
#' # write timeseries columnwise to csv, using a specified period_format
#' write_ts_csv(ts1, file = "ts1_2.csv", rowwise = FALSE, period_format = "%Y-%m-%d")
#
#' \dontshow{
#'    unlink("ts1.csv")
#'    unlink("ts1_2.csv")
#' }
#' @seealso \code{\link{read_ts_csv}} and \code{\link{write_ts_xlsx}}
#' @export
write_ts_csv <- function(x, file, rowwise = TRUE, sep = ",", dec = ".",
                        labels = c("after", "before", "no"),
                        period_format = "regts") {

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  dataframes  <- ts2df_(x, rowwise, labels, labels_missing, period_format,
                        FALSE)
  data <- dataframes$data
  column_headers <- dataframes$column_headers
  has_labels <- dataframes$has_labels

  fwrite(column_headers, file, sep = sep, dec  = dec,
         col.names = FALSE)
  fwrite(data, file, sep = sep, dec  = dec, append = TRUE, col.names = FALSE)

  return(invisible(NULL))
}


#' Functions for writing timeseries to an xlsx file
#'
#' These functions can be used to write timeseries to a sheet of an
#' xlsx file. \code{write_ts_xlsx} creates or opens an Excel workbook
#' (depending on argument \code{append}) and writes the timeseries to
#' a sheet with a specified name.
#' \code{write_ts_sheet} writes timeseries to a sheet of a workbook
#' created with \code{\link[openxlsx]{createWorkbook}} (see the example
#' below).
#'
#' The functions employ package
#' \code{\link[openxlsx:openxlsx-package]{openxlsx}}
#' package for writing the Excel file.
#'
#' If you want to write multiple timeseries objects to different
#' sheets, you can use \code{write_ts_xlsx} with argument
#' \code{append = TRUE}. Alternatively,
#' you can create a \code{Workbook} object with
#' function \code{\link[openxlsx]{createWorkbook}} of package
#' \code{openxlsx} and then add a sheet with \code{write_ts_sheet}.
#' The latter approach is more efficient.
#' When the workbook is written to a file with function
#'  \code{\link[openxlsx]{saveWorkbook}}, is is often useful to
#' set the minimum column width option of package \code{openxlsx},
#' as is shown is the example below.
#'
#' @param x a \code{\link{ts}} or \code{\link{regts}} object
#' @param file the filename of the output file
#' @param wb a \code{Workbook} object created with function
#' \code{\link[openxlsx]{createWorkbook}} or \code{\link[openxlsx]{loadWorkbook}}
#' @param sheet_name the sheet name
#' @param append If \code{FALSE} (the default), then the original file,
#' if it exists, is replaced with the new file. All original data is lost.
#' If \code{TRUE}, then only data on the sheet with
#' the specified sheet name is erased and replaced with new data.  If the sheet
#' does not yet exist, then a new sheet is created and appended to the
#' original file.
#' @param rowwise a logical value: should the timeseries be written rowwise?
#' @param labels should labels be written, and if so before or after
#' the names? By default, labels are written after the names if present
#' @param number_format a character value specifying the number format.
#' For example, \code{"#.00"} corresponds to two decimal spaces.
#' For details see the description of the function
#' \code{\link[openxlsx]{createStyle}}
#' in the \code{\link[openxlsx:openxlsx-package]{openxlsx}} package.
#' @param period_as_date A logical (default \code{FALSE}).
#' If \code{TRUE} the periods are written as date values to the Excel file.
#' By default the periods are written as characters using the standard
#' \code{regts} format (e.g. \code{"2010Q2"}, see \code{\link{period}}).
#' @param comments a character vector or data frame. The comments
#' are written to the beginning of the sheet, before the timeseries data is
#' written.

#' @name write_ts_xlsx/write_ts_sheet
#' @examples
#' # create two timeseries objects
#' ts1 <- regts(matrix(rnorm(50), ncol =  2), names = c("a", "b"),
#'              labels = c("Timeseries a", "Timeseries b"), start = "2017Q2")
#'
#' # write timeseries ts1 to an Excel file
#' write_ts_xlsx(ts1, file = "ts1.xlsx", sheet_name = "ts1", labels = "after")
#'
#' # write two sheets using write_ts_sheet
#' library(openxlsx)
#' wb <- createWorkbook()
#' write_ts_sheet(ts1, wb, "ts1", labels = "after")
#' write_ts_sheet(ts1 * 100, wb, "ts1_times_100", labels = "after")

#' # Set the minimum column width. saveWorkbook will adjust
#' # the column widths for the sheets written by write_ts_xlsx,
#' # Setting a minimum column width prevents that some columns are very
#' # narrow.
#' options("openxlsx.minWidth" = 8.43)

#' saveWorkbook(wb, "timeseries.xlsx", overwrite = TRUE)
#'
#' # write a timeseries with comments
#' comments <- c("Timeseries ts1 is created on the Central Bureau of Policy Analysis",
#'               "using a random number generator")
#' write_ts_xlsx(ts1, file = "ts_comments.xlsx", sheet_name = "ts1",
#'               comments = comments)
#' \dontshow{
#'    unlink("ts1.xlsx")
#'    unlink("timeseries.xlsx")
#'    unlink("ts_comments.xlsx")
#' }
#' @seealso \code{\link{read_ts_xlsx}} and \code{\link{write_ts_csv}}
NULL

#' @describeIn write_ts_xlsx-slash-write_ts_sheet writes timeseries to an Excel
#' workbook
#' @importFrom openxlsx createWorkbook
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx saveWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx removeWorksheet
#' @importFrom openxlsx worksheetOrder
#' @importFrom openxlsx worksheetOrder<-
#' @importFrom openxlsx createStyle
#' @importFrom openxlsx addStyle
#' @importFrom openxlsx setColWidths
#' @importFrom openxlsx freezePane
#' @export
write_ts_xlsx <- function(x, file, sheet_name = "Sheet1",
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
    wb <- loadWorkbook(file)
  } else {
    if (file.exists(file)) {
      unlink(file)
    }
    wb <- createWorkbook()
  }

  if (append) {
    sheet_names <- names(wb)
    sheet_exists <- sheet_name %in% sheet_names
    if (sheet_exists) {
      sheet_names_old <- sheet_names
      removeWorksheet(wb, sheet_name)
    }
  }
  addWorksheet(wb, sheetName = sheet_name, gridLines = TRUE)


  write_ts_sheet_(x, wb, sheet_name, rowwise = rowwise,
                   labels = labels, labels_missing = missing(labels), comments,
                   number_format, period_as_date = period_as_date)

  if (append && sheet_exists) {
    # if the sheet already existed, then keep the original ordering
    order <- match(sheet_names_old, names(wb))
    worksheetOrder(wb) <- order
  }

  minWidth_old <- options("openxlsx.minWidth")[[1]]
  options("openxlsx.minWidth" = 8.43)

  saveWorkbook(wb, file, overwrite = TRUE)

  options("openxlsx.minWidth" = minWidth_old)

  return(invisible(NULL))
}

#' @describeIn write_ts_xlsx-slash-write_ts_sheet writes a timeseries to a
#' \code{Workbook} object
#' @export
write_ts_sheet <- function(x, wb, sheet_name = "Sheet1", rowwise = TRUE,
                          labels = c("after", "before", "no"),
                          comments, number_format, period_as_date = FALSE) {

  sheet_exists <- sheet_name %in% names(wb)

  if (sheet_exists) {
    sheetnames_old <- names(wb)[worksheetOrder(wb)]
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  write_ts_sheet_(x, wb, sheet_name, rowwise = rowwise, labels = labels,
                   labels_missing = missing(labels), comments, number_format,
                   period_as_date = period_as_date)

  if (sheet_exists) {
    # if the sheet already existed, then keep the original ordering
    order <- match(sheetnames_old, names(wb))
    worksheetOrder(wb) <- order
  }
}

# internal function to write a timeseries object to a sheet of an Excel workbook
write_ts_sheet_ <- function(x, wb, sheet, rowwise, labels, labels_missing,
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

  # Write the column headers.
  writeData(wb, sheet, column_headers, colNames = FALSE, rowNames = FALSE,
            startRow = n_comment_rows + 1)

  # Set style of the column headers of data columns.
  # Use right alignment, except if period_as_date has been used.
  if (!(rowwise && period_as_date)) {
    style <- createStyle(halign = "right")
    cols <- seq(n_text_cols + 1, ncol(column_headers))
    rows <- seq(n_comment_rows + 1, n_comment_rows + nrow(column_headers))
    addStyle(wb, sheet, style = style, rows = rows, cols = cols,
             gridExpand = TRUE)
  }

  # now write the data part

  if (!rowwise && frequency(x) == 1) {
    # convert strings representing years to numeric
    data[1] <- as.numeric(data[[1]])
  }

  start_row <- n_text_rows + n_comment_rows + 1
  writeData(wb, sheet, data, colNames = FALSE, rowNames = FALSE,
                      startRow = start_row)
  if (!missing(number_format)) {
    style <- createStyle(numFmt = number_format)
    addStyle(wb, sheet, style = style,
                       rows = start_row - 1+ seq_len(nrow(data)), cols = 2 : ncol(data),
                       gridExpand = TRUE)
  }


  setColWidths(wb, sheet, 1:ncol(data), widths = "auto")


  if (!missing(comments)) {
    writeData(wb, sheet, comments, colNames = FALSE,
                        rowNames = FALSE)
  }


  freezePane(wb, sheet, firstActiveRow = row_split,
                       firstActiveCol = col_split)


  return(invisible(NULL))
}

# Internal function that converts a timeseries to a data frames that can be
# written to a csv or excel file. The function returns a list with
# three elements:
#   data          : The data part of the timeseries (excluding column headers).
#                   For rowwise timeseries data may include labels.
#   column_headers: The column header, a data frame with one or two rows.
#                   For columnwise timeseries with labels the first row
#                   contains the label and the second row the variable names.
#                   Otherwise column_headers has 1 row with periods (rowwise
#                   timeseries) or names (columnwise timeseries)
#   has_labels:     TRUE if the timeseries will be written with labels.
#   period_format   period format: regts or for example %Y-%m-%d
#   period_as_date  write period as Dates.
#
ts2df_ <- function(x, rowwise, label_option, labels_missing, period_format,
                   period_as_date) {

  if (!is.ts(x)) {
    stop(paste("Argument x is not a timeseries object but a ", class(x)))
  }

  if (is.null(colnames(x))) {
    colnames(x) <- paste0("series", 1:ncol(x))
  }

  # collect labels
  if (labels_missing || label_option != "no") {
    lbls <- ts_labels(x)
    if (labels_missing) {
      if (is.null(lbls)) {
        label_option <- "no"
      } else if (rowwise) {
        label_option <- "after"
      } else {
        label_option <- "before"
      }
    } else {
      if (is.null(lbls)) {
        lbls <- rep("", NCOL(x))
      }
    }
    has_labels = !is.null(lbls)
  } else {
    has_labels = FALSE
  }

  # remove the labels, we don't need them any more
  ts_labels(x) <- NULL

  period_as_date <- period_format != "regts" || period_as_date
  data <- as.data.frame(x, row_names = FALSE, period_as_date = period_as_date)
  if (period_format != "regts") {
    data$period <- format(data$period, period_format)
  }

  if (rowwise) {
    periods <- data$period
    data$period <- NULL
    data <- transpose_df(data)
    names <- rownames(data)
    if (label_option == "no") {
      data <- cbind(name = names, data, stringsAsFactors = FALSE)
      col_headers <- c("name")
    } else if (label_option == "after") {
      data <- cbind(name = names, label = lbls, data, stringsAsFactors = FALSE)
      col_headers <- c("name", "label")
    } else if (label_option == "before") {
      data <- cbind(label = lbls, name = names, data, stringsAsFactors = FALSE)
      col_headers <- c("label", "name")
    }
    n_rowheaders <- ncol(data) - length(periods)
    column_headers <- as.data.frame(c(as.list(colnames(data)[1:n_rowheaders]),
                                      as.list(periods)),
                                    stringsAsFactors = FALSE)
    colnames(column_headers) <- NULL
  } else {
    # columnwise timeseries
    column_headers <- as.data.frame(t(colnames(data)), stringsAsFactors = FALSE)
    if (label_option == "before") {
      column_headers <- rbind(c("label", lbls), column_headers,
                                stringsAsFactors = FALSE)
    } else if (label_option == "after") {
        stop("For columnwise timeseries labels option \"after\" is not allowed")
    }
  }

  rownames(data) <- NULL
  colnames(data) <- NULL
  return(list(data = data,  column_headers = column_headers,
              has_labels = has_labels))
}

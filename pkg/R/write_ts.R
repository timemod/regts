#' Write timeseries to a csv file
#'
#' This function writes timeseries to a csv file.
#' The csv file is actually written by function \code{\link[data.table]{fwrite}}
#' of package \code{data.table}.
#' @param x a \code{ts} or \code{regts} object
#' @param file a \code{regts} object
#' @param rowwise a logical value: should the timeseries we written rowwise?
#' @param sep The separator between columns. Default is ",".
#' @param dec The decimal separator, by default ".". Cannot be the same as sep.
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present.
#' @importFrom data.table fwrite
#' @export
write_ts_csv <- function(x, file, rowwise = TRUE, sep = ",", dec = ".",
                        labels = c("after", "before", "no")) {

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  dataframes  <- ts2df_(x, rowwise, labels, labels_missing)
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
#' \code{write_ts_sheet} writes timeseries to a \code{\link[xlsx]{Sheet}}
#' object.
#'
#' The functions employ function \code{\link[xlsx]{addDataFrame}}
#' from the \code{\link[xlsx]{xlsx}} package for writing the Excel file.
#'
#' If you want to write multiple timeseries objects to different
#' sheets, you can use \code{write_ts_xlsx} with argument
#' \code{append = TRUE}, or \code{write_ts_sheet}. The latter approach
#' is more efficient.
#'
#' @param x a \code{ts} or \code{regts} object
#' @param file the filename of the output file
#' @param sheet_name the sheet name
#' @param sheet a \code{\link[xlsx]{Sheet}} object (see the documentation
#' of package \code{xlsx})
#' @param append If \code{FALSE} (the default), then the original file,
#' if it exists, is replaced with the new file. All original data is lost.
#' If \code{TRUE}, then only data on the sheet with
#' the specified sheet name is erased and replaced with new data.  If the sheet
#' does not yet exist, then a new sheet is created and appended to the
#' original file.
#' @param rowwise a logical value: should the timeseries we written rowwise?
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present
#' @param number_format a character value specifying the number format.
#' For example, \code{"#.00"} corresponds to two decimal spaces.
#' For details see the description of the function \code{\link[xlsx]{DataFormat}}
#' in the \code{\link[xlsx]{xlsx}} package.
#' @param comments a character vector or data frame. The comments
#' are written to the beginning of the sheet, before the timeseries data is
#' written.
#' @importFrom xlsx loadWorkbook
#' @importFrom xlsx getSheets
#' @importFrom xlsx removeRow
#' @importFrom xlsx createSheet
#' @importFrom xlsx createWorkbook
#' @importFrom xlsx CellStyle
#' @importFrom xlsx Alignment
#' @importFrom xlsx addDataFrame
#' @importFrom xlsx saveWorkbook
#' @importFrom xlsx createFreezePane
#' @importFrom xlsx autoSizeColumn
#' @importFrom xlsx removeRow
#' @importFrom xlsx DataFormat
#'
#' @describeIn write_ts_xlsx Write timeseries to an Excel workbook
#' @examples
#' # create two timeseries objects
#' ts1 <- regts(matrix(rnorm(50), ncol =  2), names = c("a", "b"),
#'              labels = c("Timeseries a", "Timeseries b"),
#'              start = "2017Q2")
#'
#' # write timeseries ts1 to an Excel file
#' write_ts_xlsx(ts1, file = "ts1.xlsx", sheet_name = "ts1", labels = "after")
#'
#' # write two sheets using write_ts_sheet
#' wb <- createWorkbook()
#' sheet <- createSheet(wb, "ts1")
#' write_ts_sheet(ts1, sheet, labels = "after")
#' sheet <- createSheet(wb, "ts1_times_100")
#' write_ts_sheet(ts1 * 100, sheet = sheet, labels = "after")
#' saveWorkbook(wb, "timeseries.xlsx")
#' #
#' # write a timeseries with comments
#' comments <- c("Timeseries ts1 is created on the Central Bureau of Policy Analysis",
#'               "using a random number generator")
#'  write_ts_xlsx(ts1, file = "ts_comments.xlsx", sheet_name = "ts1",
#'                comments = comments)
#' \dontshow{
#'    unlink("ts1.xlsx")
#'    unlink("timeseries.xlsx")
#'    unlink("ts_comments.xlsx")
#' }
#' @export
write_ts_xlsx <- function(x, file, sheet_name = "Sheet1",
                          rowwise = TRUE, append = FALSE,
                          labels = c("after", "before", "no"), comments,
                          number_format) {

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

  nsheet <- wb$getNumberOfSheets()
  if (nsheet > 0) {
    sheets <- getSheets(wb)
  }
  if (nsheet > 0 && sheet_name %in% names(sheets)) {
    sheet <- sheets[[sheet_name]]
    removeRow(sheet)
  } else {
    sheet <- createSheet(wb, sheetName = sheet_name)
  }

  write_ts_sheet_(x, sheet, rowwise = rowwise,
                  labels = labels, labels_missing = missing(labels), comments,
                  number_format)

  saveWorkbook(wb, file)

  return(invisible(NULL))
}

#' @describeIn write_ts_xlsx Writes a timeseries to a a \code{\link[xlsx]{Sheet}} object
#' @export
write_ts_sheet <- function(x, sheet,  rowwise = TRUE,
                           labels = c("after", "before", "no"),
                           comments, number_format) {

  write_ts_sheet_(x, sheet, rowwise = rowwise, labels = labels,
                  labels_missing = missing(labels), comments, number_format)

}

# internal function to write a timeseries object to a sheet of an Excel workbook
write_ts_sheet_ <- function(x, sheet, rowwise, labels, labels_missing,
                            comments, number_format) {

  # check for comments. The comments are actually written before the
  # autoSizeColumns() command has been executed.
  if (missing(comments)) {
    n_comment_rows <- 0
  } else {
    comments <- as.data.frame(comments)
    n_comment_rows <- nrow(comments)
  }

  dataframes  <- ts2df_(x, rowwise, labels, labels_missing)
  data <- dataframes$data
  column_headers <- dataframes$column_headers
  has_labels <- dataframes$has_labels

  if (rowwise) {
    n_text_rows <- 1
    n_text_cols <- 1 + as.integer(has_labels)
    row_split <- 2
    col_split <- 1 + n_text_cols
  } else {
    n_text_rows <- 1 + as.integer(has_labels)
    n_text_cols <- 1
    row_split <- 2 + as.integer(has_labels)
    col_split <- 2
  }
  row_split <- row_split + n_comment_rows


  # Write the column headers. Use right alignment for the numeric columns
  wb <- sheet$getWorkbook()
  right_align_style <- CellStyle(wb, alignment = Alignment(horizontal =
                                                             "ALIGN_RIGHT"))
  col_style <- rep(list(right_align_style), ncol(column_headers) - n_text_cols)
  names(col_style) <- seq(n_text_cols + 1, ncol(column_headers))
  addDataFrame(column_headers, sheet, col.names = FALSE, row.names = FALSE,
               colStyle = col_style, startRow = n_comment_rows + 1)

  # now write the data part

  if (missing(number_format)) {
    col_style <- NULL
  } else {
    cs <- CellStyle(wb, dataFormat = DataFormat(number_format))
    ndata <- ncol(data)
    col_style <- rep(list(cs), ndata)
    names(col_style) <- seq(n_text_cols + 1, n_text_cols + ndata)
  }

  addDataFrame(data, sheet, col.names = FALSE, row.names = FALSE,
               startRow = n_text_rows + n_comment_rows + 1,
               colStyle = col_style)

  autoSizeColumn(sheet, seq_len(ncol(data)))

  if (!missing(comments)) {
    addDataFrame(comments, sheet, col.names = FALSE, row.names = FALSE)
  }

  createFreezePane(sheet, rowSplit = row_split, colSplit = col_split)


  return(invisible(NULL))
}


# Internal function that converts a timeseries to a data frame that can be
# written to a csv or excel file.
ts2df_ <- function(x, rowwise, label_option, labels_missing) {

  if (!is.ts(x)) {
    stop(paste("Argument x is not a timeseries object but a ", class(x)))
  }

  # collect labels
  if (labels_missing || label_option != "no") {
    lbls <- ts_labels(x)
    if (labels_missing) {
      if (is.null(lbls)) {
        label_option <- "no"
      } else {
        label_option <- "after"
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

  data <- as.data.frame(x)

  if (rowwise) {
    data <- transpose_df(data)
    names <- rownames(data)
    if (label_option == "no") {
      data <- cbind(name = names, data, stringsAsFactors = FALSE)
    } else if (label_option == "after") {
      data <- cbind(name = names, label = lbls, data, stringsAsFactors = FALSE)
    } else if (label_option == "before") {
      data <- cbind(label = lbls, name = names, data, stringsAsFactors = FALSE)
    }
    column_headers <- as.data.frame(t(colnames(data)), stringsAsFactors = FALSE)
  } else {
     # columnwise timeseries
    data <- cbind(period = rownames(data), data, stringsAsFactors = FALSE)
    column_headers <- as.data.frame(t(colnames(data)), stringsAsFactors = FALSE)
    if (label_option == "after") {
      column_headers <- rbind(c("label", lbls), column_headers,
                                stringsAsFactors = FALSE)
    } else if (label_option == "before") {
        stop("For columnwise timeseries labels option \"before\" is not allowed")
    }
  }

  rownames(data) <- NULL
  colnames(data) <- NULL
  return(list(data = data,  column_headers = column_headers,
              has_labels = has_labels))
}



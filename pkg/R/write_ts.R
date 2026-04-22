#' Write timeseries to a csv file
#'
#' Write timeseries to a csv file, using [data.table::fwrite].
#'
#' @param x A [ts] or [regts] object.
#' @param file A \code{regts} object
#' @param rowwise A logical value: should the timeseries be written rowwise?
#' @param sep The separator between columns. Default is ",".
#' @param dec The decimal separator, by default ".". Cannot be the same as sep.
#' @param labels Should labels be written? If so, should it be
#' done before the names or after the names? By default,
#' labels are written after the names if present.
#' @param period_format The period format. By default the
#' \code{regts} format (e.g. \code{"2010Q2"}, see \code{\link{period}}) is used.
#' Alternatively, it is possible to specify a format employed by base R function
#' \code{\link[base]{strptime}}, e.g. \code{"\%Y-\%m-\%d"}.
#' @param verbose A logical (default `FALSE`). If `TRUE`, the function
#' prints the filename, the number of timeseries written, the period range, and
#' the elapsed time.
#' @importFrom data.table fwrite
#' @examples
#' # create a timeseries object
#' ts1 <- regts(matrix(rnorm(50), ncol =  2), names = c("a", "b"),
#'              labels = c("Timeseries a", "Timeseries b"), start = "2017Q2")
#'
#' # write timeseries to csv
#' write_ts_csv(ts1, file = "ts1.csv", labels = "after")
#'
#' # write timeseries columnwise to csv, using a specified period_format
#' write_ts_csv(ts1, file = "ts1_2.csv", rowwise = FALSE,
#'              period_format = "%Y-%m-%d")
#
#' \dontshow{
#'    unlink("ts1.csv")
#'    unlink("ts1_2.csv")
#' }
#' @seealso [read_ts_csv] and [write_ts_xlsx].
#' @export
write_ts_csv <- function(x, file, rowwise = TRUE, sep = ",", dec = ".",
                         labels = c("after", "before", "no"),
                         period_format = "regts", verbose = FALSE) {

  if (verbose) {
    message(sprintf("\nWriting timeseries to file %s ...", file))
    t_start <- Sys.time()
  }

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  dataframes  <- ts2df_(x, rowwise, labels, missing(labels),
                        period_format, FALSE)
  data <- dataframes$data
  column_headers <- dataframes$column_headers

  fwrite(column_headers, file, sep = sep, dec  = dec, col.names = FALSE)
  fwrite(data, file, sep = sep, dec  = dec, append = TRUE, col.names = FALSE)

  if (verbose) {
    t_end <- Sys.time()
    secs <- t_end - t_start
    message(sprintf(paste("%d timeseries written, period range %s, %.2f sec.",
                          "elapsed.\n"),
                    ncol(x), get_period_range(x), secs))
  }

  return(invisible(NULL))
}


#' Write timeseries to an xlsx file.
#'
#' `write_ts_xlsx` creates or opens an Excel workbook
#' (depending on argument `append`) and writes the timeseries to
#' a sheet with a specified name.
#' `write_ts_sheet` writes timeseries to a sheet of a workbook
#' object returned by function [openxlsx::createWorkbook]
#' or [openxlsx::loadWorkbook] of package [openxlsx].
#'
#' The functions employ package [openxlsx] package for writing the Excel file.
#'
#' If you want to write multiple timeseries objects to different
#' sheets, you can use \code{write_ts_xlsx} with argument
#' \code{append = TRUE}. Alternatively,
#' you can create a workbook object with
#' function [openxlsx::createWorkbook] of package
#' [openxlsx] and then add a sheet with \code{write_ts_sheet}.
#' The latter approach is more efficient.
#' When the workbook is written to a file with function
#' [openxlsx::saveWorkbook], it is often useful to
#' set the minimum and maximum scolumn width option for package [openxlsx],
#' as shown in the example below.
#'
#' @section Warning:
#' When using `write_ts_xlsx` with `append = TRUE`,
#' formulas on existing sheets are **not** reevaluated. The same applies when
#' `write_ts_sheet` is used to add a sheet to an existing workbook.
#' Open the file in Excel and press `F9` to recalculate all formulas manually.
#'
#' @param x A \code{\link{ts}} or \code{\link{regts}} object
#' @param file The filename of the output file
#' @param wb A \code{Workbook} object created with function
#' [openxlsx::createWorkbook] or [openxlsx::loadWorkbook].
#' @param sheet_name The sheet name
#' @param append If \code{FALSE} (the default), then the original file,
#' if it exists, is replaced with the new file. All original data is lost.
#' If \code{TRUE}, then only data on the sheet with
#' the specified sheet name is erased and replaced with new data.  If the sheet
#' does not yet exist, then a new sheet is created and appended to the
#' original file.
#' @param rowwise A logical value: should the timeseries be written rowwise?
#' @param labels Should labels be written, and if so before or after
#' the names? By default, labels are written after the names if present
#' @param number_format A character value specifying the number format.
#' For example, \code{"#.00"} corresponds to two decimal spaces.
#' For details see the description of the function
#' [openxlsx::createStyle].
#' @param period_as_date A logical (default \code{FALSE}).
#' If \code{TRUE} the periods are written as date values to the Excel file.
#' By default the periods are written as characters using the standard
#' \code{regts} format (e.g. \code{"2010Q2"}, see \code{\link{period}}).
#' @param comments A character vector or data frame. The comments
#' are written to the beginning of the sheet, before the timeseries data is
#' written.
#' @param max_col_width Integer (default 50). The column widths are
#' adjusted automatically, but are never larger than the specified value.
#' @param verbose A logical (default `FALSE`). If `TRUE`, the function
#' prints the file and sheet name, the number of timeseries written, the period
#' range, and the elapsed time.
#' @name write_ts_xlsx
#' @examples
#' # create a timeseries object
#' ts1 <- regts(matrix(rnorm(50), ncol =  2), names = c("a", "b"),
#'              labels = c("Timeseries a", "Timeseries b"), start = "2017Q2")
#'
#' # write timeseries ts1 to an Excel file
#' write_ts_xlsx(ts1, file = "ts1.xlsx", sheet_name = "ts1", labels = "after",
#'               verbose = TRUE)
#'
#' # write two sheets using write_ts_sheet
#' library(openxlsx)
#' wb <- createWorkbook()
#' write_ts_sheet(ts1, wb, "ts1", labels = "after")
#' write_ts_sheet(ts1 * 100, wb, "ts1_times_100", labels = "after",
#'                verbose = TRUE)
#'
#' # Set the minimum and maximum column width, to prevent very narrow or wide
#' # columns. saveWorkbook will adjust the widths of the columns written by
#' # write_ts_sheet.
#' options(openxlsx.minWidth = 8.43, openxlsx.maxWidth = 60)
#'
#' # Save the workbook with openxlsx::saveWorkbook.
#' ok <- saveWorkbook(wb, "timeseries.xlsx", overwrite = TRUE,
#'                    returnValue = TRUE)
#' if (!ok) {
#'   stop("Failed to save workbook to file '", output_file,
#'        "'. Check warnings.")
#' }
#'
#' # write a timeseries with comments
#' comments <- c("Timeseries ts1 is created at the CPB",
#'               "using a random number generator")
#' write_ts_xlsx(ts1, file = "ts_comments.xlsx", sheet_name = "ts1",
#'               comments = comments)
#' \dontshow{
#'    unlink("ts1.xlsx")
#'    unlink("timeseries.xlsx")
#'    unlink("ts_comments.xlsx")
#' }
#' @seealso [read_ts_xlsx] and [write_ts_csv].
NULL

#' @describeIn write_ts_xlsx writes timeseries to an Excel workbook file.
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
#' @importFrom utils packageVersion
#' @export
write_ts_xlsx <- function(x, file, sheet_name = "Sheet1",
                          rowwise = TRUE, append = FALSE,
                          labels = c("after", "before", "no"), comments,
                          number_format, period_as_date = FALSE,
                          max_col_width = 60,
                          verbose = FALSE) {

  if (verbose) {
    message(sprintf("\nWriting timeseries to sheet %s of file %s ...",
                    sheet_name, file))
    t_start <- Sys.time()
  }

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

  write_ts_sheet_(
    x,
    wb,
    sheet = sheet_name,
    rowwise = rowwise,
    labels = labels,
    labels_missing = missing(labels),
    comments = comments,
    number_format = number_format,
    period_as_date = period_as_date
  )

  if (append && sheet_exists) {
    # if the sheet already existed, then keep the original ordering
    order <- match(sheet_names_old, names(wb))
    worksheetOrder(wb) <- order
  }

  # Set the minimum and maximum column width and restore them on exit.
  # Use a fixed minimum column width of 8.43.
  original_opts <- options("openxlsx.minWidth", "openxlsx.maxWidth")
  on.exit(options(original_opts), add = TRUE)
  options(openxlsx.minWidth = 8.43)
  if (!is.na(max_col_width)) options(openxlsx.maxWidth = max_col_width)

  result <- saveWorkbook(wb, file, overwrite = TRUE, returnValue = TRUE)
  if (!isTRUE(result)) {
    stop("Failed to save workbook to file '", file, "'. Check warnings.")
  }

  if (verbose) {
    t_end <- Sys.time()
    secs <- t_end - t_start
    message(sprintf(paste("%d timeseries written, period range %s, %.2f sec.",
                          "elapsed.\n"),
                    ncol(x), get_period_range(x), secs))
  }

  return(invisible(NULL))
}

#' @describeIn write_ts_xlsx writes a timeseries to a workbook object
#' returned by [openxlsx::createWorkbook] or [openxlsx::loadWorkbook].
#' @export
write_ts_sheet <- function(x, wb, sheet_name = "Sheet1", rowwise = TRUE,
                           labels = c("after", "before", "no"), comments,
                           number_format, period_as_date = FALSE,
                           verbose = FALSE) {

  if (verbose) {
    message(sprintf(
      "\nWriting timeseries to sheet %s ...\n",
      sheet_name
    ))
    t_start <- Sys.time()
  }

  sheet_exists <- sheet_name %in% names(wb)

  if (sheet_exists) {
    sheetnames_old <- names(wb)[worksheetOrder(wb)]
    removeWorksheet(wb, sheet_name)
  }
  addWorksheet(wb, sheet_name)

  if (!is.matrix(x)) {
    x <- univec2unimat(x, deparse(substitute(x)))
  }

  write_ts_sheet_(
    x,
    wb,
    sheet = sheet_name,
    rowwise = rowwise,
    labels = labels,
    labels_missing = missing(labels),
    comments = comments,
    number_format = number_format,
    period_as_date = period_as_date
  )

  if (sheet_exists) {
    # if the sheet already existed, then keep the original ordering
    order <- match(sheetnames_old, names(wb))
    worksheetOrder(wb) <- order
  }

  if (verbose) {
    t_end <- Sys.time()
    secs <- t_end - t_start
    message(sprintf(paste("%d timeseries written, period range %s, %.2f sec.",
                          "elapsed.\n"),
                    ncol(x), get_period_range(x), secs))
  }

  invisible()
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

  dataframes <- ts2df_(x, rowwise, labels, labels_missing, "regts",
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


  # convert strings representing years to numeric
  if (rowwise && frequency(x) == 1) {
    col_sel <- - seq_len(n_text_cols)
    column_headers[, col_sel] <-
      as.data.frame(lapply(column_headers[, col_sel], FUN = as.numeric))
  }

  # write comments
  if (!missing(comments)) {
    writeData(wb, sheet, comments, colNames = FALSE, rowNames = FALSE)
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
    rows <- start_row : (start_row + nrow(data) - 1)
    cols <- (n_text_cols + 1) : ncol(data)
    addStyle(wb, sheet, style = style, rows = rows, cols = cols,
             gridExpand = TRUE)
  }

  setColWidths(wb, sheet, seq_len(ncol(data)), widths = "auto")

  row_split <- n_text_rows + n_comment_rows + 1
  col_split <- n_text_cols + 1
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
ts2df_ <- function(x, rowwise, labels = c("after", "before", "no"),
                   labels_missing, period_format, period_as_date) {

  if (!is.ts(x)) {
    stop(paste("Argument x is not a timeseries object but a ", class(x)))
  }

  labels <- match.arg(labels)

  if (is.null(colnames(x)) && ncol(x) > 0) {
    colnames(x) <- paste0("series", seq_len(ncol(x)))
  }

  # collect labels
  if (labels != "no") {
    lbls <- ts_labels(x)
    if (labels_missing && (is.null(lbls) || !any(nzchar(trimws(lbls))))) {
      # if argument labels has not been specified and if there are no labels,
      # then set labels to "no".
      labels <- "no"
    } else if (is.null(lbls)) {
      lbls <- rep("", NCOL(x))
    }
  }
  has_labels <- labels != "no"

  # remove the labels, we don't need them any more
  ts_labels(x) <- NULL

  period_as_date <- period_format != "regts" || period_as_date
  data <- as_data_frame(x, period_as_date = period_as_date)
  if (period_format != "regts") {
    data$period <- format(data$period, period_format)
  }

  if (rowwise) {
    periods <- data$period
    data$period <- NULL
    data <- transpose_df(data)
    names <- rownames(data)
    if (labels == "no") {
      data <- cbind(name = names, data, stringsAsFactors = FALSE)
    } else if (labels == "after") {
      data <- cbind(name = names, label = lbls, data, stringsAsFactors = FALSE)
    } else if (labels == "before") {
      data <- cbind(label = lbls, name = names, data, stringsAsFactors = FALSE)
    }
    n_rowheaders <- ncol(data) - length(periods)
    column_headers <- as.data.frame(c(as.list(colnames(data)[1:n_rowheaders]),
                                      as.list(periods)),
                                    stringsAsFactors = FALSE)
    colnames(column_headers) <- NULL
  } else {
    # columnwise timeseries
    column_headers <- as.data.frame(t(colnames(data)),
                                    stringsAsFactors = FALSE)
    column_headers[1] <- "name"
    if (labels == "before") {
      column_headers <- rbind(c("label", lbls), column_headers,
                              stringsAsFactors = FALSE)
    } else if (labels == "after") {
      column_headers <- rbind(column_headers, c("label", lbls),
                              stringsAsFactors = FALSE)
    }
  }

  rownames(data) <- NULL
  colnames(data) <- NULL
  return(list(data = data,  column_headers = column_headers,
              has_labels = has_labels))
}

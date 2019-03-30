#' Read timeseries from a sheet of an  xls(x) file
#'
#' This function attempts to read timeseries from an xls(x) file.
#' The xls(x) file is actually read by function \code{\link[readxl]{read_excel}}
#' of package \code{readxl}.
#' The timeseries can be stored both rowwise or columnwise on the sheet.
#' The function tries to find period cells on the sheet. Period cells are
#' cells containing
#' \itemize{
#' \item a text with the format recognized by function \code{\link{period}},
#' for example \code{"2010Q2"}, \code{"2010.2Q"},
#' \code{"2010m2"}, \code{"2011"} or \code{"2011-1"},
#' \item an integer value (e.g. \code{2018}), which is considered as a year,
#' \item a date
#' }
#' Use argument \code{period_fun} if the period cells contain a text with a
#' format not recognized by function \code{period}.
#'
#' In many cases, this function will read timeseries correctly.
#' However, \emph{you should always carefully check the results of this
#' function}. If the function fails or if the result is not
#' what you want, then you have to read the data into a data frame
#' (for example by using function \code{read_excel} of package \code{readxl}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{\link{regts}} by using function
#' \code{\link{as.regts}}.
#'
#' If argument \code{rowwise} has not been specified, then
#' function \code{read_ts_xlsx} tries to guess if the timeseries are stored
#' rowwise or columnwise based on the positions of the period cells.
#'
#' \strong{rowwise timeseries}
#'
#' For rowwise timeseries, the function searches for the first
#' row with periods.  All rows before the period row are ignored.
#' Columns without a valid period in the period row are also ignored.
#' The first non-empty column in the sheet should contain the timeseries names
#' (or labels if argument \code{labels = "before"}, see the discussion below).
#' Otherwise, use argument \code{skipcol} to specify the number of columns
#' to skip.
#'
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=260}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
#' There may be more than one column before the columns with timeseries values
#' (data columns). In that case one column should contain the variable names.
#' The other columns before the first data column are used to create
#' timeseries labels (see \code{\link{ts_labels}}). If argument
#' \code{labels = "after"} (default), then the first
#' column contains the variable names. If \code{labels = "no"} the first column
#' also contains variable names but the other columns before the first data
#' column are ignored. If argument \code{labels = "before"}, then the variable
#' names should be in the last column before the first data column.
#'
#'\strong{columnwise timeseries}
#'
#' For columnwise timeseries, the first non-empty row that has been read (see
#' argument \code{range} or \code{skiprow}) should contain the variable names
#' (or labels if argument \code{labels = "before"}, see the discussion below).
#' The periods can be in any column on the sheet.
#' Rows without a valid period in the period column are ignored.
#' All columns to the left of the period column are also ignored.
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=240}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' There may be more than one row before the rows with timeseries values
#' (data rows). In that case one row should contain the variable names.
#' The other rows before the first data row are used to create
#' timeseries labels (see \code{\link{ts_labels}}).
#' If argument  \code{labels = "after"} (default), then the first
#' row contains the variable names. If \code{labels = "no"} the first row
#' also contains variable names but the other rows before the first data
#' row are ignored. If argument \code{labels = "before"}, then the variable
#' names should be in the last row before the first data row.
#'
#' By default, the function skips all leading empty rows and columns,
#' just as \code{read_excel}. This behaviour can be overruled by specifying
#' arguments \code{range}, \code{skiprow} or \code{skipcol}.
#'
#' Sometimes it helps to supply information about the structure of
#' the data on the sheet. Specify option  \code{rowwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' With \code{name_fun} a function can be applied to names of the timeseries,
#' e.g. \code{\link{tolower}}.
#'
#' @param filename  a string with the filename.
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). Ignored if the sheet is
#' specified via range. If neither argument specifies the sheet,
#' defaults to the first sheet.
#' @param  range	A cell range to read from, as described in
#' \code{\link[readxl]{cell-specification}}. Includes typical Excel ranges
#' like \code{"B3:D87"}, possibly including the
#' sheet name like \code{"Budget!B2:G14"}, and more.
#' Strictly, even if the range forces the inclusion of leading or trailing
#' empty rows or columns.
#' Takes precedence over \code{skiprow}, \code{skipcol} and \code{sheet}.
#' @param skiprow the number of rows to skip, including leading empty rows.
#' Ignored if \code{range} is given. By default, all leading empty rows are
#' skipped.
#' @param skipcol the number of columns to skip, including empty columns.
#' Ignored if \code{range} is given. By default, all leading empty columns are
#' skipped.
#' @param rowwise a logical value: are the timeseries stored rowwise?
#' If not specified, then \code{read_ts_xlsx} tries to figure out itself if
#' the timeseries are stored rowwise or columnwise.
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains period texts without
#' frequency indicator (for example "2011-1").
#' @param labels label option. See Details.
#' @param na_string Character vector of strings to use for missing values.
#' By default, \code{read_ts_xlsx} treats blank cells as missing data.
#' @param name_fun function to apply to the names of the timeseries.
#' @param period_fun function applied to period texts. This should be a function
#' that converts a character vector of length 1 to a character vector of length 1.
#' Use this argument if the period texts do not have a standard format (see Description).
#' @param strict A logical. If \code{TRUE} (the default) all periods between the
#' start and the end period must be present.
#' Otherwise the timeseries are filled with \code{NA} for the missing periods.
#'

#' @return a \code{regts} object
#'
#' @examples
#' \dontrun{
#' read_ts_xlsx("series.xlsx", skipcol = 2, na_string = c("", "NA"))
#' read_ts_xlsx("data.xlsx", sheet = "Budget", labels = "after",
#'              name_fun = tolower)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom cellranger cell_limits
#' @importFrom cellranger as.cell_limits
#' @seealso \code{\link{write_ts_xlsx}} and \code{\link{read_ts_csv}}
#' @export
read_ts_xlsx <- function(filename, sheet = NULL, range = NULL,
                         skiprow = 0, skipcol = 0, rowwise, frequency = NA,
                         labels = c("after", "before", "no"),
                         na_string = "", name_fun, period_fun, strict = TRUE) {

  # NOTE: read excel skips leading empty rows if the the first row in range
  # has not been specified (if the first row is NA). We do not want that,
  # therefore always start from the first row
  if (missing(range)) {
    range <- cell_limits()
    range$ul[1] <- skiprow + 1
    range$ul[2] <- skipcol + 1
  } else {
    range <- as.cell_limits(range)
    if (is.na(range$ul[1])) range$ul[1] <- 1
    if (is.na(range$ul[2])) range$ul[2] <- 1
  }

  na_string <- union(na_string, "")

  labels <- match.arg(labels)

  if (!missing(name_fun) && !is.function(name_fun)) {
      stop("argument name_fun is not a function")
  }
  if (!missing(period_fun) && !is.function(period_fun)) {
    stop("argument period_fun is not a function")
  }

  # the number of lines used to inspect the file
  N <- 25
  range_1 <- range
  range_1$lr[1] <- range_1$ul[1] + N - 1

  tbl_1 <- read_excel(filename, sheet, range = range_1, col_names = FALSE,
                    col_types = "list", na = na_string,
                    .name_repair = "minimal")

  # TODO: sheetname is not correct if the sheetname is specified in
  # argument range.
  sheetname <- if (is.null(sheet)) "1" else as.character(sheet)

  if (nrow(tbl_1) == 0 || ncol(tbl_1) == 0) {
    stop(sprintf("Sheet %s of file %s is empty\n", sheetname, filename))
  }

  tbl_layout <- get_tbl_layout(tbl_1, frequency, rowwise, labels,
                               xlsx = TRUE, period_fun = period_fun)

  #printobj(tbl_layout)

  if (is.null(tbl_layout)) {
    stop(sprintf("No periods found on Sheet %s of file %s\n", sheetname,
                 filename))
  }

  if (tbl_layout$rowwise) {
    return(read_ts_rowwise_xlsx(filename, sheet, range, na_string, frequency,
                                labels, name_fun, period_fun, tbl_layout,
                                strict))
  } else {
    return(read_ts_columnwise_xlsx(filename, sheet, range, na_string,
                                   frequency, name_fun, period_fun,
                                   tbl_layout, strict))
  }

  return(ret)
}

# Internal function to read timeseries rowwise from a tibble, used by
# read_ts_xlsx
read_ts_rowwise_xlsx <- function(filename, sheet, range, na_string,
                                 frequency, labels, name_fun, period_fun,
                                 tbl_layout, strict) {
  #
  # read data
  #

  range$ul[1] <- range$ul[1] + tbl_layout$period_row
  range$lr[2] <- range$ul[2] + tbl_layout$last_data_col - 1

  #printobj(range)

  col_types <- rep("skip", tbl_layout$last_data_col)
  col_types[tbl_layout$is_data_col[1:tbl_layout$last_data_col]] <- "numeric"
  col_types[1:(tbl_layout$first_data_col - 1)] <- "text"
  #printobj(col_types)

  # read data
  data_tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                         col_types = col_types, na = na_string,
                         .name_repair = "minimal")

  # View(data_tbl)

  # find first  non-empty column. NOTE: do not use tbl_layout$first_col_nr,
  # because that colum number if only based on the first 25 rows.
  # TODO: make a function get_first_non_empty_col in read_utils.
  not_all_na <- sapply(data_tbl, FUN = function(x) {!all(is.na(x))})
  first_col_nr <- Position(identity, not_all_na)

  name_info <- get_name_info_rowwise(tbl_layout, first_col_nr, data_tbl,
                                      labels, name_fun)

  # convert data columns to a numeric matrix
  rowsel <- name_info$row_has_name
  colsel <- tbl_layout$first_data_col : ncol(data_tbl)
  mat <- t(as.matrix(data_tbl[rowsel , colsel]))
  colnames(mat) <- name_info$names

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- matrix2regts_(mat, periods = tbl_layout$periods, fun = period,
                       numeric = FALSE, frequency = frequency, strict = strict)

  if (!is.null(name_info$lbls)) {
    ts_labels(ret) <- name_info$lbls
  }

  return(ret)
}

# Internal function to read timeseries columnwise from a tibble, used in
# read_ts_xlsx.
read_ts_columnwise_xlsx <- function(filename, sheet, range, na_string,
                                    frequency, name_fun, period_fun,
                                    tbl_layout, strict) {

  if (!any(tbl_layout$is_data_col)) return(NULL) # TODO: error message?

  #
  # read data
  #

  # determine range of data to read
  last_data_col <- Position(identity, tbl_layout$is_data_col, right = TRUE)
  range$ul[1] <- range$ul[1] + tbl_layout$period_row - 1
  range$ul[2] <- range$ul[2] + tbl_layout$period_col - 1
  range$lr[2] <- range$ul[2] + last_data_col - tbl_layout$period_col

  # prepare column types
  ncol_to_read <- last_data_col - tbl_layout$period_col + 1
  col_types <- rep("skip", ncol_to_read)
  col_types[1] <- "list"
  col_types[tbl_layout$is_data_col[tbl_layout$period_col:last_data_col]] <- "numeric"
  #printobj(col_types)
  #printobj(range)

  # read data
  data_tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                         col_types = col_types, na = na_string,
                         .name_repair = "minimal")

  # select rows with valid periods
  row_sel <- is_period_tbl(data_tbl[[1]], frequency, TRUE, period_fun)
  data_tbl <- data_tbl[row_sel, ]

  # TODO: this code is not efficient:
  # 1) The period has been parsed before in is_period_tbl.
  # 2) If period_fun has been specified, then it it called twice,
  periods <- get_periods_tbl(data_tbl[[1]], frequency, TRUE, period_fun)


  mat <- as.matrix(data_tbl[-1])
  colnames(mat) <- tbl_layout$names

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- matrix2regts_(mat, periods, fun = period, numeric = FALSE,
                       frequency = frequency, strict = strict)

  if (!is.null(tbl_layout$lbls)) {
    ts_labels(ret) <- tbl_layout$lbls
  }

  return(ret)
}

convert_data_tbl <- function(tbl, transpose, colnames) {
  # This function converts the part of the tibble that should contain data
  # values to a numerical matrix.

  # First check if the tbl contains POSIXt or Date objects.
  # These cannot be handled correctly, so we should give an error.
  check_col <- function(x) {
    x <- sapply(x, FUN = function(x) {inherits(x, "POSIXt") |
                                      inherits(x, "Date")})
    return(any(x))
  }

  problem_cols <- sapply(tbl, FUN = check_col)
  if (any(problem_cols)) {
    stop(paste("Found Date values in cells were numerical values are expected"))
  }

  # convert the tbl to a numeric matrix, employing C++ function list_tbl_2_mat
  mat <- list_tbl_2_mat(tbl)

  if (transpose) mat <- t(mat)

  colnames(mat) <- colnames
  return(mat)
}

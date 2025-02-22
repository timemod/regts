#' Read timeseries from a sheet of an  xls(x) file
#'
#' This function reads timeseries from a sheet of an xls(x) file,
#' employing function \code{\link[readxl]{read_excel}} of
#' package \code{readxl}. \code{read_ts_xlsx} searches for period cells and
#' automatically determines how the timeseries are stored
#' (rowwise or columnwise) and which columns contain the numerical values of
#' the timeseries. Period cells are cells containing
#' \itemize{
#' \item a text with the format recognized by function \code{\link{period}},
#' for example \code{"2010Q2"}, \code{"2010.2Q"},
#' \code{"2010m2"}, \code{"2011"} or \code{"2011-1"},
#' \item an integer value (e.g. \code{2018}), which is considered as a year,
#' \item a date, which  is assumed to specify a month unless argument
#' \code{frequency} has been specified.
#' }
#' Use argument \code{period_fun} if the period cells contain a text with a
#' format not recognized by function \code{period}.
#'
#' \code{read_ts_xlsx} reads the timeseries data in two steps. In the first
#' step, the first 25 rows are read to inspect the
#' structure of the data on the sheet: are the timeseries stored rowwise or
#' columnwise, which row or column contains the period cells and which columns
#' contain the numerical data of the timeseries.
#' Using this information, the
#' complete sheet is read and the timeseries are constructed.
#'
#' In many cases, this function will read timeseries correctly.
#' If the function fails or if the result is not what you want,
#' it might help to specify arguments \code{rowwise}, \code{frequency},
#' \code{period_fun}, \code{range}, \code{skipcol} or \code{skiprow}.
#' Specify option  \code{rowwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the
#' timeseries.
#' Arguments \code{range}, \code{skipcol} and \code{skiprow} can be used to read
#' only a part of the file.
#'
#' If that does not help, then you can read the data into a data frame
#' (for example by using function \code{read_excel} of package \code{readxl}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{\link{regts}} by using function
#' \code{\link{as.regts}}.
#'
#' \code{read_ts_xlsx} skips all empty rows and columns. Use arguments
#' \code{skipcol} and \code{skiprow} to skip additional leading rows
#' and columns. Argument \code{range} can  be used to read only a part
#' of the sheet.
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
#' With argument \code{name_fun} a function can be applied to names of the
#' timeseries, e.g. \code{\link{tolower}}.
#'
#' \strong{columnwise timeseries}
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
#' @param filename  a string with the filename.
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). Ignored if the sheet is
#' specified via range. If neither argument specifies the sheet,
#' defaults to the first sheet.
#' @param  range	A cell range to read from, as described in
#' \code{\link[readxl]{cell-specification}}. Includes typical Excel ranges
#' like \code{"B3:D87"}, possibly including the
#' sheet name like \code{"Budget!B2:G14"}, and more.
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
#' that converts a character vector to another character vector or a
#' \code{period} vector with the same length. Use this argument if the period
#' texts do not have a
#' standard format (see Description).
#' @param strict A logical. If \code{TRUE} (the default) all periods between the
#' start and the end period must be present.
#' Otherwise the timeseries are filled with \code{NA} for the missing periods.
#' @param warn_num_text A logical. If \code{TRUE} (the default) a warning is
#' issued if a cell contains a number as text (e.g. \code{"2012.2"}) when
#' a numeric value is expected. The text is always converted to a numeric value
#' assuming the decimal separator \code{"."}.
#' @param warn_dupl A logical. If \code{TRUE} (the default), a warning is
#' issued if there are duplicate column names in the returned timeseries object.
#' @param verbose A logical (default `FALSE`). If `TRUE`, the function
#' prints the name of the file and sheet, the number of timeseries read,
#' the period range, and the elapsed time.
#' @return a \code{regts} object
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
#' @importFrom testthat capture_warnings
#' @importFrom cellranger num_to_letter
#' @importFrom stringr str_match
#' @seealso \code{\link{write_ts_xlsx}} and \code{\link{read_ts_csv}}
#' @export
read_ts_xlsx <- function(filename, sheet = NULL, range = NULL,
                         skiprow = 0, skipcol = 0, rowwise, frequency = NA,
                         labels = c("after", "before", "no"),
                         na_string = "", name_fun, period_fun, strict = TRUE,
                         warn_num_text = TRUE, warn_dupl = TRUE,
                         verbose = FALSE) {

  na_string <- union(na_string, "")

  labels <- match.arg(labels)

  if (!missing(name_fun) && !is.function(name_fun)) {
    stop("argument name_fun is not a function")
  }
  if (!missing(period_fun) && !is.function(period_fun)) {
    stop("argument period_fun is not a function")
  }

  range <- check_read_range(range, skiprow = skiprow, skipcol = skipcol)

  # Create sheetname for (error) messages
  sheetname <- if (!is.na(range$sheet))  {
    range$sheet
  } else if (!is.null(sheet)) {
    as.character(sheet)
  } else {
    "1"
  }

  if (verbose) {
    cat(sprintf("\nReading timeseries from sheet %s of file %s ...\n",
                sheetname, filename))
    t_start <- Sys.time()
  }

  layout <- get_sheet_layout(filename, sheet, sheetname = sheetname,
                             range = range, frequency = frequency,
                             rowwise = rowwise, labels = labels,
                             na_string = na_string, period_fun = period_fun,
                             name_fun = name_fun)

  if (layout$rowwise) {
    ret <- read_ts_rowwise_xlsx(filename, sheet, sheetname, range, na_string,
                                frequency, labels, name_fun, layout, strict,
                                warn_num_text)
  } else {
    ret <- read_ts_columnwise_xlsx(filename, sheet, sheetname, range, na_string,
                                   frequency, period_fun, layout, strict,
                                   warn_num_text)
  }

  # Check for duplicate names
  if (warn_dupl) test_duplicates(ret, filename, sheetname)

  if (verbose) {
    t_end <- Sys.time()
    secs <- t_end - t_start
    cat(sprintf("%d timeseries read, period range %s, %.2f sec. elapsed.\n\n",
                ncol(ret), get_period_range(ret), secs))
  }

  return(ret)
}

check_read_range <- function(range, skiprow, skipcol) {
  if (is.null(range)) {
    range <- cell_limits()
    range$ul[1] <- skiprow + 1
    range$ul[2] <- skipcol + 1
  } else {
    range <- as.cell_limits(range)
    # Function read_excel skips leading empty rows and columns if the first
    # row or column in range is NA (i.e. not-specified). This is not desired
    # here, because otherwise we do no longer know to which part of the sheet
    # the data read corresponds.
    if (is.na(range$ul[1])) range$ul[1] <- 1
    if (is.na(range$ul[2])) range$ul[2] <- 1
  }
  return(range)
}

get_sheet_layout <- function(filename, sheet, sheetname, range, frequency,
                             rowwise, labels, na_string, period_fun,
                             name_fun) {

  # n_inspect is the number of rows to read to determine the layout of the
  # sheet.
  n_inspect <- 25

  range_inspect <- range
  range_inspect$lr[1] <- min(range_inspect$ul[1] + n_inspect - 1,
                             range_inspect$lr[1], na.rm = TRUE)

  tbl_inspect <- read_excel(filename, sheet, range = range_inspect,
                            col_names = FALSE, col_types = "list",
                            na = na_string, .name_repair = "minimal")

  if (nrow(tbl_inspect) == 0 || ncol(tbl_inspect) == 0) {
    stop(sprintf("Sheet %s of file %s is empty\n", sheetname, filename))
  }

  layout <- inspect_tibble(tbl_inspect, frequency, rowwise, labels,
                           xlsx = TRUE, period_fun = period_fun,
                           name_fun = name_fun)

  if (is.null(layout)) {
    stop(sprintf("No periods found on sheet %s of file %s\n", sheetname,
                 filename))
  }
  return(layout)
}

# Internal function to read timeseries rowwise from a tibble, used by
# read_ts_xlsx
read_ts_rowwise_xlsx <- function(filename, sheet, sheetname, range, na_string,
                                 frequency, labels, name_fun, layout, strict,
                                 warn_num_text) {

  if (is.list(layout$periods)) {
    # there are periods with different frequencies
    row_nr <- range$ul[1] + layout$period_row - 1
    col1 <- num_to_letter(range$ul[2] + layout$first_data_col - 1)
    col2 <- num_to_letter(range$ul[2] + layout$last_data_col - 1)
    cells <- paste0(c(col1, col2), row_nr)
    stop(sprintf(paste0("The row %s:%s of sheet %s of file %s contains\n",
                        "periods with different frequencies."),
                 cells[1], cells[2], sheetname, filename))
  }

  #
  # read data
  #

  range$ul[1] <- range$ul[1] + layout$period_row
  range$lr[2] <- range$ul[2] + layout$last_data_col - 1

  col_types <- rep("skip", layout$last_data_col)
  col_types[layout$is_data_col[1:layout$last_data_col]] <- "numeric"
  col_types[1:(layout$first_data_col - 1)] <- "text"

  # read data
  warnings <- capture_warnings({
    data_tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                           col_types = col_types, na = na_string,
                           .name_repair = "minimal")
  })

  # When all cells in the specified range are empty, then read_excel returns
  # a tibble with dimension 0 x 0 (0 rows and 0 columns). Therefore this case
  # should be handled separately.
  has_data <- nrow(data_tbl) > 0

  if (has_data) {

    name_info <- get_name_info_rowwise(layout, data_tbl, labels, name_fun)

    # convert data columns to a numeric matrix
    rowsel <- name_info$row_has_name
    colsel <- layout$first_data_col : ncol(data_tbl)
    mat <- t(data_tbl[rowsel, colsel])
    colnames(mat) <- name_info$names

    # print warnings
    for (warning in select_read_excel_warnings(warnings, name_info$row_has_name,
                                               range$ul[1], warn_num_text)) {
      warning(warning)
    }

  } else {
    mat <- matrix(NA_real_,
                  nrow = layout$last_data_col - layout$first_data_col + 1,
                  ncol = 0)
  }

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- matrix2regts_(mat, layout$periods, numeric = FALSE, strict = strict)

  if (has_data && !is.null(name_info$lbls)) {
    ts_labels(ret) <- name_info$lbls
  }

  return(ret)
}

# Internal function to read timeseries columnwise from a tibble, used in
# read_ts_xlsx.
read_ts_columnwise_xlsx <- function(filename, sheet, sheetname, range,
                                    na_string, frequency, period_fun, layout,
                                    strict, warn_num_text) {
  #
  # read data
  #

  # determine range of data to read

  last_col_to_read <- if (is.na(layout$last_data_col)) {
    layout$period_col
  } else {
    layout$last_data_col
  }

  range$ul[1] <- range$ul[1] + layout$first_data_row - 1
  range$ul[2] <- range$ul[2] + layout$period_col - 1
  range$lr[2] <- range$ul[2] + last_col_to_read - layout$period_col

  # prepare column types

  ncol_to_read <- last_col_to_read - layout$period_col + 1
  col_types <- rep("skip", ncol_to_read)
  col_types[1] <- "list"
  if (!is.na(layout$last_data_col)) {
    colsel <- layout$period_col:layout$last_data_col
    col_types[layout$is_data_col[colsel]] <- "numeric"
  }

  warnings <- capture_warnings({
    data_tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                           col_types = col_types, na = na_string,
                           .name_repair = "minimal")
  })

  # select rows with valid periods
  row_sel <- is_period_data(data_tbl[[1]], frequency, TRUE, period_fun)
  data_tbl <- data_tbl[row_sel, ]

  for (warning in select_read_excel_warnings(warnings, row_sel, range$ul[1],
                                             warn_num_text)) {
    warning(warning)
  }

  periods <- get_periods_data(data_tbl[[1]], frequency, TRUE, period_fun)

  if (is.list(periods)) {
    # periods with different frequencies
    col <- num_to_letter(range$ul[2])
    cells <- paste0(col, c(range$ul[1], range$ul[1] + length(row_sel) - 1))
    stop(sprintf(paste0("The column %s:%s of sheet %s of file %s contains\n",
                        "periods with different frequencies."),
                 cells[1], cells[2], sheetname, filename))
  }

  mat <- as.matrix(data_tbl[-1])

  if (ncol(mat) == 0) {
    # mat is a logical matrix, convert to numeric
    mat[] <- as.numeric(mat)
  } else {
    colnames(mat) <- layout$names
  }


  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- matrix2regts_(mat, periods, numeric = FALSE, strict = strict)

  if (!is.null(layout$lbls)) {
    ts_labels(ret) <- layout$lbls
  }

  return(ret)
}

select_read_excel_warnings <- function(warnings, row_sel, first_row_nr,
                                       warn_num_text)  {

  # select warnings issued by read_excel.

  # Warning about expecting a numerical value in a cell should only be given
  # for rows that are actually read.
  row_nrs <- which(row_sel) + first_row_nr - 1

  pattern <- "^((Expecting numeric)|(Coercing text to numeric)) in [A-Z]+(\\d+)"
  warning_row_nrs <- as.numeric(str_match(warnings, pattern)[, 5])
  sel <- is.na(warning_row_nrs) | warning_row_nrs %in% row_nrs
  if (!warn_num_text) {
    # remove warnings  about "Coercing text to numeric")
    sel <- sel & !grepl("^Coercing text to numeric", warnings)
  }
  return(warnings[sel])
}

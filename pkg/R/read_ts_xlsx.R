#' Reads timeseries from a sheet of an  xls(x) file
#'
#' This function attempts to read timeseries from an xls(x) file.
#' The xls(x) file is actually read by function \code{\link[readxl]{read_excel}}
#' of package \code{readxl}.
#' The timeseries can be stored both rowwise or columnwise on the sheet.
#' The function tries to find valid period texts on the sheet.
#' Valid period texts should have the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#' An integer value is considered as a period wih frequency year.
#' In many cases, this function will read timeseries correctly.
#' However, \emph{you should always carefully check the results of this
#' function}. If the result is not
#' what you want, then you have to read the data into a data frame
#' (for example by using function \code{read_excel} of package \code{readxl}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{regts} by using function \code{as.regts}.
#'
#' If argument \code{columnwise} has not been specified, then
#' function \code{read_ts_xlsx} searches for any valid period text in the first
#' row after the skipped rows on the sheet. If a valid period was found, then
#' \code{read_ts} assumes that the timeseries are stored rowwise. Otherwise it
#' assumes that the timeseries are stored columnwise.
#'
#'\strong{columnwise timeseries}
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=240}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' For columnwise timeseries, the first row that read (see
#' argument \code{range} or \code{skiprow}) should contain the variable names.
#' The periods can be in any column on the sheet.
#' All columns to the left of the time column are ignored.
#' There may be one or more rows between the column names and the rows
#' where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' rows will be used to create timeseries labels.
#  If \code{labels = "before"},
#' the last row before the data is supposed to contain
#' the variable names. Now the row before the variable name columns
#' should contain label information. If argument \code{use_colnames = TRUE},
#' then the label option \code{"before"} is not allowed for columnwise
#' timeseries, since in that case the column names are the timeseries names.
#'
#' \strong{rowwise timeseries}
#'
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=260}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
#' For rowwise timeseries, the first row that is read (see
#' argument \code{range} and \code{skiprow}) should contain the periods.
#' Columns for which the corresponding period is not a valid period
#' are ignored. The timeseries names should be in the
#' first column of the sheet. Otherwise, use argument \code{skipcol}
#' to specify the number of columns to skip.
#' There may be one or more columns between the column with variable names
#' and the columns where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' columns will be used to create timeseries labels. If \code{labels = "before"},
#' the last column before the data is supposed to contain
#' the variable names. The columns before the variable name column
#' now should contain label information.
#'
#' Sometimes it helps to supply information about the structure of
#' the data on the sheet. Specify option  \code{columnwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' @param filename  a string with the filename
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). Ignored if the sheet is
#' specified via range. If neither argument specifies the sheet,
#' defaults to the first sheet
#' @param  range	A cell range to read from, as described in
#' \code{\link[readxl]{cell-specification}}. Includes typical Excel ranges
#' like "B3:D87", possibly including the
#' sheet name like "Budget!B2:G14", and more. Interpreted strictly, even
#' if the range forces the inclusion of leading or trailing empty rows or
#' columns. Takes precedence over skiprow, skipcol, n_max and sheet.
#' @param columnwise a logical value: are the timeseries stored columnwise?
#' If not specified, then \code{read_ts} tries to figure out itself if
#' the timeseries are stored columnwise or rowwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains period texts without
#' frequency indicator (for example "2011-1")
#' @param labels label option. See details.
#' @param na_string Character vector of strings to use for missing values.
#' By default, \code{read_ts_xlsx} treats blank cells as missing data.
#' @param skiprow the number of rows to skip. Ignored in \code{range} is
#' given.
#' @param skipcol the number of columns to skip. Ignored if \code{range} is
#' given.
#' @param n_max Maximum number of data rows to read. Trailing empty rows are
#' automatically skipped, so this is an upper bound on the number of rows in
#' the returned tibble. Ignored if range is given.
#' @return a \code{regts} object
#' @importFrom readxl read_excel
#' @importFrom cellranger cell_limits
#' @export
read_ts_xlsx <- function(filename, sheet = NULL, range = NULL,
                         columnwise, frequency = NA,
                         labels = c("no", "after", "before"),
                         na_string = c(""), skiprow = 0, skipcol = 0,
                         n_max = Inf) {

  if (missing(range)) {
    if (is.infinite(n_max)) {
      n_max <- NA_integer_
    }
    range <- cell_limits(ul = c(skiprow + 1,     skipcol + 1),
                         lr = c(skiprow + n_max, NA))
  }

  if (missing(columnwise) || !columnwise) {
    # Read the first line of the Excel sheet to determine if the
    # first row contains a period. read_excel skips all columns
    # without any value, thus we cannot compute yet the
    # position of the first period column, as in read_ts_csv
    ul <- range$ul
    lr <- range$lr
    lr[1] <- ul[1]
    range_first_row <- cell_limits(ul = ul, lr = lr)
    first_row <- read_excel(filename, sheet, range = range_first_row,
                            col_names = FALSE)
    # the next statement is necessary. Why?
    first_row <- as.data.frame(first_row)

    is_period <- is_period_text(get_strings(first_row), frequency)
    first_prd_col <- Position(function(x) {x}, is_period)
    if (missing(columnwise)) {
      columnwise <- is.na(first_prd_col) | first_prd_col <= range$ul[2]
    }
  }


  # read the data data frame. For rowwise timeseries, the time index is put in
  # the header
  if (columnwise) {
    df <- read_excel(filename, sheet, range = range, col_names = FALSE)
  } else {
    nper <- length(is_period) - first_prd_col + 1
    col_types <- c(rep("text", first_prd_col - 1), rep("numeric", nper))
    # do not read more columns than the length of col_types
    ul <- range$ul
    lr <- range$lr
    lr[2] <- ul[2] + length(col_types) - 1
    range_tmp <- cell_limits(ul = ul, lr = lr)
    df <- read_excel(filename, sheet, range = range_tmp, col_names = TRUE,
                     col_types = col_types, na = na_string)
  }

  # the next statement is necessary. Why?
  df <- as.data.frame(df)

  if (columnwise) {
    return(read_ts(df, columnwise = columnwise, frequency = frequency,
                   labels = labels))
  } else {
    # use numeric = FALSE, because we already know that the timeseries
    # are numeric (see code above)
    return(read_ts_rowwise(df, frequency = frequency, labels = labels,
                           numeric = FALSE))
  }
}

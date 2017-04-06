#' Reads timeseries from a csv file
#'
#' This function attempts to read timeseries from a csv file.
#' The csv file is actually read by function \code{\link[data.table]{fread}}
#' of package \code{data.table}.
#' The timeseries can be stored both rowwise or columnwise.
#' The function tries to find valid period texts.
#' Valid period texts should have the format recognized by function
#' \code{\link{regperiod}}, for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#' An integer value is considered as a period wih frequency year.
#' In many cases, this function will read timeseries correctly.
#' However, \emph{you should always carefully check the results of this
#' function}. If the result is not
#' what you want, then you have read the data into a data frame
#' (for example by using function \code{read.csv} or the function
#' \code{fread} of package \code{data.table}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{regts} by using funtion \code{as.regts}.
#'
#' If argument \code{columnwise} has not been specified, then
#' function \code{read_ts_xlsx} searches for any valid period text in the first
#' row after the skipped rows. If a valid period was found, then
#' \code{read_ts} assumes that the timeseries are stored rowwise. Otherwise it
#' assumes that the timeseries are store columnwise.
#'
#'\strong{columnwise timeseries}
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=200}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' For columnwise timeseries, the first row that is not skipped (see
#' argument \code{skiprow}) should contain the variable names.
#' The periods can be in any column.
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
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=200}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
#' For rowwise timeseries, the first row that is not skipped (see
#' argument \code{skiprow}) should contain the periods.
#' Columns for which the corresponding period is not a valid period
#' are ignored. The timeseries names should be in the first column.
#' Otherwise use argument \code{skipcol} to specify the number of
#' columns to skip.
#' There may be one or more columns between the column with variable names
#' and the columns where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' columns will be used to create timeseries labels. If \code{labels = "before"},
#' the last column before the data is supposed to contain
#' the variable names. The columns before the variable name column
#' now should contain label information.
#'
#' Sometimes it helps to supply information about the structure of
#' the data in the file. Specify option  \code{columnwise} is you know
#' that the timeseries ares stored rowwise or columnwise. Specify
#' argument \code{frequency} is you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' Sometimes it helps to supply information about the structure of
#' the data on the csv file. Specify option  \code{columnwise} is you know
#' that the timeseries ares stored rowwise or columnwise. Specify
#' argument \code{frequency} is you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' @param filename  a string with the filename
#' @param columnwise a logical value: are the timeseries stored columnwise?
#' If not specified, then \code{read_ts} tries to figure out itself if
#' the timeseries are stored columnwise or rowwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains a period texts without
#' frequency indicator (for example "2011-1")
#' @param skiprow the number of rows to skip
#' @param skipcol the number of columns to skip
#' @param labels label option. See details.
#' @param ... arguments passed to function \code{\link[data.table]{fread}} of
#' package \code{read.table}
#' @return a \code{regts} object
#' @importFrom data.table fread
#' @export
read_ts_csv <- function(filename, columnwise, frequency = NA,
                        skiprow, skipcol,
                        labels = c("no", "after", "before"),
                        ...) {

  if (!missing(skiprow)) {
    skip <- skiprow
  } else {
    skip <- 0
  }

  if (missing(columnwise)) {
    first_line <- fread(filename, nrows = 1, skip = skip, header = FALSE,
                        data.table = FALSE, ...)
    columnwise <- !any(is_period_text(get_strings(first_line), frequency))
  }

  # read the data data frame. For rowwise timeseries, the time index is put in
  # the header
  df <- fread(filename, skip = skip, header = !columnwise, data.table = FALSE,
              ...)

  if (!missing(skipcol) && skipcol > 0) {
    df <- df[ , -(1:skipcol), drop = FALSE]
  }

  if (columnwise) {
    return(read_ts(df, columnwise = columnwise, frequency = frequency,
            labels = labels))
  } else {
    return(read_ts_rowwise(df, frequency = frequency, labels = labels))
  }
}

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
#' For columnwise timeseries, the first row that is not skipped (see
#' argument \code{skiprow}) should contain the variable names.
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
#' For rowwise timeseries, the first row that is not skipped (see
#' argument \code{skiprow}) should contain the periods.
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
#' @param columnwise a logical value: are the timeseries stored columnwise?
#' If not specified, then \code{read_ts} tries to figure out itself if
#' the timeseries are stored columnwise or rowwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains period texts without
#' frequency indicator (for example "2011-1")
#' @param skiprow the number of rows to skip
#' @param skipcol the number of columns to skip
#' @param labels label option. See details.
#' @param ... arguments passed to function \code{\link[readxl]{read_excel}}
#' of package \code{readxl}
#' @return a \code{regts} object
#' @importFrom readxl read_excel
#' @export
read_ts_xlsx <- function(filename, columnwise, frequency = NA,
                        skiprow, skipcol,
                        labels = c("no", "after", "before"),
                        ...) {

  if (!missing(skiprow)) {
    skip <- skiprow
  } else {
    skip <- 0
  }

  df <- read_excel(filename, skip = skip, col_names = FALSE, ...)

  # read_excel sometimes creates a weird structure: convert to standard
  # data frame
  df <- as.data.frame(df)

  if (!missing(skipcol) && skipcol > 0) {
    df <- df[ , -(1:skipcol), drop = FALSE]
  }

  return(read_ts(df, columnwise = columnwise, frequency = frequency,
                 labels = labels))
}

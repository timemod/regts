#' Reads timeseries from a data frame
#'
#' This function attempts to read timeseries from a data frame.
#' The timeseries can be stored both rowwise and columnwise in the data frame.
#' The function tries to find valid period texts in the data frame.
#' Valid period texts should have the format recognized by function
#' \code{\link{regperiod}}, for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#' An integer value is considered as a period wih frequency year.
#' In many cases, this function will read timeseries correctly
#' and this will save you a lot of work. However, \emph{you should always
#' carefully check the results of this function}. If the result is not
#' what you want, then you have to clean and preprocess the data
#' frame so that is has a more standard format (see Details).
#'
#' If the column names contain any valid period, then \code{read_ts} assumes
#' that the timeseries are stored rowwise. Otherwise it assumes that the
#' timeseries are store columnwise.
#'
#' For \strong{columnwise} timeseries, the column names should contains the timeseries
#' names. The periods can be in the row names or in any column of the data frame.
#' All columns to the left of the time column are ignored.
#' There may be one or more rows between the column names and the rows
#' where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' rows will be used to create timeseries labels. For columnwise timeseries,
#' the label option \code{"before"} is not allowed.
#'
#' For \strong{rowwise} timeseries, the column name should contain the periods.
#' Columns for which the corresponding column name is not a valid period
#' are ignored. The timeseries names can be in the row names or in
#' the first column of the data frame.
#' There may be one or more columns between the column with variable names
#' and the columns where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' columns will be used to create timeseries labels. If \code{labels = "before"},
#' the last column before the columns where the data start is supposed to contain
#' the variable names and the columns before the variable name columns
#' contain label information.
#'
#' Sometimes it helps to supply information about the structure of
#' the data frame. Specify option  \code{columnwise} is you know
#' that the timeseries ares stored rowwise or columnwise. Specify
#' argument \code{frequency} is you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' @param df a \code{data.frame}
#' @param columnwise a logical value: are the timeseries stored columnwise?
#' If not specified, then \code{read_ts} tries to figure out itself if
#' the timeseries are stored columnwise or rowwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains a period texts without
#' frequency indicator (for example "2011-1")
#' @param labels label option. See details.
#' @return a \code{regts} object
#' @export
read_ts <- function(df, columnwise, frequency = NA,
                    labels = c("no", "after", "before")) {

  labels <- match.arg(labels)

  # TODO: what about stringAsFactors?

  if (missing(columnwise)) {
    columnwise <- !any(is_period_text(colnames(df), frequency))
  }

  if (columnwise) {
    return(read_ts_columnwise(df, frequency, labels))
  } else {
    return(read_ts_rowwise(df, frequency, labels))
  }
}

# returns true if x is a positive integer
is_posint <- function(x) {
  return(grepl("^\\d+$", as.character(x)))
}

# Standard columnwise format:
#  - variable names in column names
#  - optionally labels in first row (TO DO!)
#  - time axis in rownames, or any other column.
#    columns before the time column are skipped
read_ts_columnwise <- function(df, frequency, labels) {

  if (labels == "before") {
    stop(paste("For columnwise timeseries, the label option 'before'",
               "is not allowed"))
  }

  period_info <- find_period_column(df, frequency)
  time_column <- period_info$col_nr
  is_period <- period_info$is_period
  first_data_row <- Position(function(x) {x}, is_period)
  label_rows <- seq_len(first_data_row - 1)

  # remove column without names to the left of the time_columns
  keep_cols <- nchar(colnames(df)) > 0
  if (time_column > 0) {
    keep_cols[1:time_column] <- TRUE
  }
  df <- df[, keep_cols, drop = FALSE]

  # Ff colr_nr > 1, then remove all previous columns
  # NOTE: we have to do this before rows without period are removed,
  # because the latter procedure will also remove empty columns
  if (time_column > 1) {
    df <- df[, -(1:time_column-1), drop = FALSE]
    time_column <- 1
  }


  if (labels == "after") {
    lbl_data <- df[label_rows, , drop = FALSE]
    if (time_column > 0) {
      lbl_data <- lbl_data[, -time_column, drop = FALSE]
    }
    if (length(label_rows) == 1) {
      labels <- as.character(lbl_data)
    } else {
      labels <- as.data.frame(t(lbl_data))
      l <- lapply(labels, as.character)
      labels <- do.call(paste, l)
      labels <- trimws(labels)
    }
  }

  # remove rows without period
  df <- df[is_period, , drop = FALSE]

  ret <- as.regts(df, time_column = time_column, frequency = frequency)
  if (labels != "no" && any(nchar(labels) > 0)){
    ts_labels(ret) <- labels
  }
  return(ret)
}

# Find a period column for a standard (columnwise) data frame.
# Returns a vector which two elements:
#  * the first element is the period column: 0 if the period is in the row names,
#     >= 1 if in a column of the data frame and -1 if not found
#  * the second element is a logical vector: TRUE if the correponding
#    element of the period column is a period
find_period_column <- function(df, frequency) {

  # The standard row names of a data frame are row numbers.
  # Unfortunately, we cannot distinguish between standard row names
  # or year indicators (if we know the frequency we can do more)
  standard_rownames <- all(is_posint(rownames(df)))

  if (!standard_rownames) {
    # check if the row names contain periods
    is_period <- is_period_text(rownames(df), frequency)
    if (any(is_period)) {
      row_nr <- Position(function(x) {x}, is_period)
      return(list(col_nr = 0, is_period = is_period))
    }
  }

  # try to find the first column with period.
  for (i in 1:ncol(df)) {
    is_period <- is_period_text(df[, i], frequency)
    if (any(is_period)) {
      col_index <- i
      row_nr <- Position(function(x) {x}, is_period)
      return(list(col_nr = i, is_period = is_period))
    }
  }

  # We have not found a time column yet. If the row names are
  # integers, then we assume that they are years
  # TODO: do not do this if frequency > 1 has been specified)
  if (standard_rownames) {
    return(list(col_nr = 0, is_period = rep(TRUE, nrow(df))))
  }
}

# Rowwise format:
#  - time indicators in colnames
#  - variable names in rownames or first column,
#    optionally labels in the second column
#  - optionally labels in first column, variable names second column
#    (argument labels_first)
read_ts_rowwise <- function(df, frequency, labels) {

  is_period <- is_period_text(colnames(df), frequency)

  first_data_col <- Position(function(x) {x}, is_period)

  standard_rownames <- all(is_posint(rownames(df)))

  if (standard_rownames) {

    # the row names do not contain variable names
    if (labels == "before") {
      colname_column <- first_data_col - 1
      label_columns <- seq_len(colname_column - 1)
    } else {
      colname_column <- 1
      if (first_data_col > colname_column + 1) {
        label_columns <- (colname_column + 1) : (first_data_col - 1)
      } else {
        label_columns <- integer(0)
      }
    }

    # remove rows without variable names
    df <- df[nchar(df[, colname_column]) > 0, , drop = FALSE]

  } else {

    colname_column <- 0

    # remove empty row names
    if (colname_column == 0) {
      df <- df[nchar(rownames(df)) > 0, , drop = FALSE]
    }

    # variable names in the row names
    if (labels == "before") {
      stop(paste("Label option 'before' is not allowed",
                 "if the row names are not numbered"))
    }

    label_columns <- seq_len(first_data_col - 1)
  }

  if (labels != "no") {
    labels <- df[, label_columns, drop = FALSE]
    l <- lapply(labels, as.character)
    labels <- do.call(paste, l)
    labels <- trimws(labels)
  }

  # remove all columns without period except for the colname column
  keep_cols <- is_period
  if (colname_column >= 1) {
    keep_cols[colname_column] <- TRUE
  }
  df <- df[ ,  keep_cols, drop = FALSE]

  if (colname_column > 1) {
    colname_column <- 1
  }

  if (colname_column >= 1) {
    df <- transpose_df(df, colname_column = colname_column)
  } else {
    df <- transpose_df(df)
  }

  # remove empty column names
  df <- df[ , nchar(colnames(df)) > 0, drop = FALSE]

  ret <- as.regts(df, frequency = frequency)
  if (labels != "no" && any(nchar(labels) > 0)){
    ts_labels(ret) <- labels
  }
  return(ret)
}

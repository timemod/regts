#' Read timeseries from a csv file
#'
#' This function attempts to read timeseries from a csv file.
#' The csv file is actually read by function \code{\link[data.table]{fread}}
#' of package \code{data.table}.
#' The timeseries can be stored both rowwise or columnwise.
#' The function tries to find valid period texts.
#' Valid period texts should have the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"}, \code{"2010.2Q"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#'
#' In many cases, this function will read timeseries correctly.
#' However, \emph{you should always carefully check the results of this
#' function}. If the function fails or if the result is not
#' what you want, then you have to read the data into a data frame
#' (for example by using function \code{read.csv} or the function
#' \code{fread} of package \code{data.table}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{\link{regts}} by using function
#' \code{\link{as.regts}}.
#'
#'
#' If argument \code{rowwise} has not been specified, then
#' function \code{read_ts_xlsx} tries to guess if the timeseries are stored
#' rowwise or columnwise based on the positions of the fields with period texts.
#'
#' \strong{rowwise timeseries}
#'
#' For rowwise timeseries, the function searches for the first
#' row with periods.  All rows before the period row are ignored.
#' Columns for which the corresponding period is not a valid period
#' are ignored. The timeseries names should be in the first non-empty column.
#' Otherwise use argument \code{skipcol} to specify the number of
#' columns to skip.
#'
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=200}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
#' There may be one or more columns between the column with variable names
#' and the columns where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' columns will be used to create timeseries labels. If \code{labels = "before"},
#' the last column before the data is supposed to contain
#' the variable names. The columns before the variable name column
#' now should contain label information.
#'
#'\strong{columnwise timeseries}
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=200}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' For columnwise timeseries, the first non-empty row that is not skipped (see
#' argument \code{skiprow}) should contain the variable names.
#' The periods can be in any column.
#' All columns to the left of the period column are ignored.
#' There may be one or more rows between the column names and the rows
#' where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' rows will be used to create timeseries labels.
#' If \code{labels = "before"},
#' the last row before the data is supposed to contain
#' the variable names. Now the row before the variable name columns
#' should contain label information. If argument \code{use_colnames = TRUE},
#' then the label option \code{"before"} is not allowed for columnwise
#' timeseries, since in that case the column names are the timeseries names.
#'
#' Sometimes it helps to supply information about the structure of
#' the data on the csv file. Specify argument \code{rowwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' With \code{name_fun} a function can be applied to names of the timeseries,
#' e.g. \code{\link{tolower}}.
#'
#' \strong{automatic row skip}
#'
#' If \code{skiprow = 0}, then the first rows with less columns than
#' the rest of the file are automatically skipped. These rows are assumed
#' to be comment rows. This procedure is described in detail
#' in the documentation of function
#' \code{\link[data.table]{fread}} of the \code{data.table} package.
#' Briefly, \code{fread} first determines the number of columns
#' and then searches for the first data row based on this number of columns.
#' All rows before this data row are skipped.
#'
#' If argument \code{fill} is \code{TRUE}, then all rows have the same
#' number of columns, and automatic row skipping is therefore
#' disabled.
#'
#' @param filename  a string with the filename
#' @param rowwise a logical value: are the timeseries stored rowwise?
#' If not specified, then \code{read_ts_csv} tries to figure out itself if
#' the timeseries are stored rowwise or columnwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains a period texts without
#' frequency indicator (for example "2011-1")
#' @param skiprow the number of rows to skip.
#' If 0 (default) and if argument \code{fill} is \code{FALSE},
#' then comment rows are automatically skipped.
#' See Details.
#' @param skipcol the number of columns to skip
#' @param labels label option. See details
#' @param sep the separator between columns. If not specified, then
#' the separator is determined automatically by inspecting the
#' first 30 lines of the csv file (see the details of function
#' \code{\link[data.table]{fread}}).
#' @param fill logical (default is \code{FALSE}). If \code{TRUE} then in case
#' the rows have unequal length, blank fields are implicitly filled
#' with \code{NA}.
#' @param dec the decimal separator as in \code{base::read.csv}.
#' If not "." (default) then usually ",".
#' @param na_string Character vector of strings to use for missing values.
#' By default, \code{read_ts_csv} treats blank cells as missing data.
#' @param name_fun function to apply to the names of the timeseries, e.g.
#' \code{tolower}.
#' @return a \code{regts} object
#'
#' @examples
#' \dontrun{
#' read_ts_csv("series.csv", sep = ";", dec = ",")
#' read_ts_csv("data.csv", labels = "after", name_fun = tolower)
#' }
#'
#' @importFrom data.table fread
#' @importFrom tibble as.tibble
#' @export
read_ts_csv <- function(filename, rowwise, frequency = NA,
                        skiprow = 0, skipcol = 0,
                        labels = c("no", "after", "before"),
                        sep = "auto", fill = FALSE,
                        dec = if (sep != ".") "." else ",",
                        na_string = "", name_fun) {

  if (!missing(skiprow)) {
    skip <- skiprow
  } else {
    skip <- 0
  }

  na_string <- union(na_string, "")

  df <- fread(filename, skip = skip, header = FALSE, data.table = FALSE,
              sep = sep, dec = dec, fill = fill, colClasses = "character",
              na.strings = na_string)

  if (!missing(skipcol) && skipcol > 0) {
    df <- df[ , -(1:skipcol), drop = FALSE]
  }

  if (nrow(df) == 0 && ncol(df) == 0) {
    stop(sprintf("File %s is empty\n", filename))
  }

  tbl <- as.tibble(df)

  not_all_na <- sapply(tbl, FUN = function(x) {!all(is.na(x))})
  tbl <- tbl[ , not_all_na, drop = FALSE]

  period_info <- find_periods(tbl, frequency, rowwise)

  if (is.null(period_info)) {
    stop(sprintf("No periods found in file %s\n", filename))
  }

  if (period_info$rowwise) {
    ret <- read_ts_rowwise(tbl, frequency = frequency, labels = labels,
                           dec = dec, name_fun = name_fun,
                           period_info = period_info)
  } else {
    ret <- read_ts_columnwise(tbl, frequency = frequency, labels = labels,
                             dec = dec, name_fun = name_fun,
                             period_info = period_info)
  }

  # apply function to columnnames if given
  if (!missing(name_fun)) {
    if (!is.function(name_fun)) {
      stop("argument name_fun is not a function")
    }
    colnames(ret) <- name_fun(colnames(ret))
  }

  return(ret)

}

# internal function to read timeseries rowwise from a data frame with
# the time index in the column header.
# is numeric = TRUE, then the timeseries are converted to numeric
read_ts_rowwise <- function(tbl, frequency, labels = c("no", "after", "before"),
                            name_fun, dec = ".", period_info) {

  labels <- match.arg(labels)

  # remove all rows before the period row
  if (period_info$row_nr > 1) {
    tbl <- tbl[-(1:(period_info$row_nr - 1)), ]
  }

  is_period <- period_info$is_period
  first_prd_col <- period_info$col_nr

  # ignore possible period in the first colum
  first_prd_col <- max(first_prd_col, 2)

  name_col <- if (labels == "before") first_prd_col - 1 else 1

  if (labels == "before") {
    name_col <- first_prd_col - 1
    label_cols <- seq_len(name_col - 1)
  } else {
    name_col <- 1
    if (first_prd_col >= 3) {
      label_cols <- 2 : (first_prd_col - 1)
    } else {
      label_cols <- numeric(0)
    }
  }

  # keep only period columns, the name column and the label columns
  col_sel <- is_period
  col_sel[c(name_col, label_cols)] <- TRUE
  tbl <- tbl[ , col_sel]

  data_cols <- (max(c(name_col, label_cols)) + 1) : ncol(tbl)

  periods <- tibble_2_char(tbl[1, data_cols])

  names <- tibble_2_char(tbl[-1, name_col], replace_na = FALSE)
  name_sel <- !is.na(names)
  names <- names[name_sel]

  # remove rows without names, including the row with the period
  tbl <- tbl[c(FALSE, name_sel), ]

  # convert all data columns to numerical columns, taking the decimal separator
  # into account
  mat <- numeric_matrix(tbl[, data_cols], dec = dec)
  mat <- t(mat)
  rownames(mat) <- periods
  colnames(mat) <- names


  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that df is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && length(label_cols) > 0) {
    lbl_data <- tbl[, label_cols]
    lbl_data[] <- lapply(lbl_data, FUN = function(x)
    {ifelse(is.na(x),  "", as.character(x))})
    if (length(label_cols) == 1) {
      lbls <- unlist(lbl_data, use.names = FALSE)
    } else {
      lbls <- do.call(paste, lbl_data)
      lbls <- trimws(lbls, which = "right")
    }
    if (any(lbls != "")) {
      ts_labels(ret) <- lbls
    }
  }

  return(ret)
}

# Internal function to read timeseries columnwise from a dataframe.
# This function is used in function read_ts_csv and read_ts_xlsx
read_ts_columnwise <- function(tbl, frequency = NA,
                               labels = c("no", "after", "before"),
                               name_fun, dec =  ".", period_info) {

  labels <- match.arg(labels)

  time_column <- period_info$col_nr
  is_period <- period_info$is_period
  first_data_row <- period_info$row_nr

  # ignore possible period in the first row
  first_data_row <- max(first_data_row, 2)

  # compute the row with variable names. 0 means: column names
  # and the label rows
  if (labels != "before") {
    name_row <- 1
    if (first_data_row > 2) {
      label_rows <- 2:(first_data_row -1)
    } else {
      label_rows <- integer(0)
    }
  } else {  # labels == before
    name_row <- first_data_row - 1
    if (first_data_row > 2) {
      label_rows <- 1:(first_data_row - 2)
    } else {
      label_rows <- integer(0)
    }
  }

  if (length(label_rows) == 0) {
    labels <- "no"
  }

  # drop all columns to the left of time_column
  if (time_column > 1) {
    tbl <- tbl[, -(1:(time_column - 1))]
  }

  # get variable names
  ts_names <- unlist(tbl[name_row, -1], use.names = FALSE)
  name_sel <- which(!is.na(ts_names))
  ts_names <- ts_names[name_sel]

  # remove columns without names
  keep_cols <- c(1, name_sel + 1)
  tbl <- tbl[, keep_cols]

  # labels
  if (labels != "no" && length(label_rows) > 0) {
    lbl_data <- tbl[label_rows, -1]
    lbl_data[] <- lapply(lbl_data, FUN = function(x)
    {ifelse(is.na(x),  "", as.character(x))})
    if (length(label_rows) == 1) {
      lbls <- unlist(lbl_data, use.names = FALSE)
    } else {
      lbl_mat <- as.matrix(lbl_data)
      lbls <- apply(lbl_mat, MARGIN = 2, FUN = paste, collapse = " ")
      lbls <- trimws(lbls, which = "right")
    }
  }

  # remove all rows without period (the names and labels have already been
  # stored in variables ts_names and lbls)
  tbl <- tbl[is_period, ]

  # convert data columns to a numeric matrix, employing C++ function
  # list_tbl_2_mat.
  mat <- numeric_matrix(tbl[-1], dec = dec)
  rownames(mat) <- tibble_2_char(tbl[[1]])
  colnames(mat) <- ts_names

  # set numeric = FALSE, because we already know that df is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && any(lbls != "")) {
    ts_labels(ret) <- lbls
  }

  return(ret)
}

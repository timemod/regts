#' Read timeseries from a csv file
#'
#' This function attempts to read timeseries from a csv file.
#' The csv file is actually read by function \code{\link[data.table]{fread}}
#' of package \code{data.table}.
#' The timeseries can be stored both rowwise or columnwise.
#' The function tries to find valid period texts.
#' Valid period texts should have the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#'
#' An integer value is considered as a period wih frequency year.
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
#' If argument \code{rowwise} has not been specified, then
#' function \code{\link{read_ts_csv}} searches for any valid period text in the
#' first row after the skipped rows. If a valid period was found, then
#' \code{read_ts_csv} assumes that the timeseries are stored rowwise. Otherwise it
#' assumes that the timeseries are stored columnwise.
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
#' Sometimes it helps to supply information about the structure of
#' the data on the csv file. Specify option  \code{rowwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the timeseries.
#' Argument \code{frequency} is mandatory if a general period format
#' such as  \code{"2011-1"} has been used.
#'
#' With \code{name_fun} a function can be applied to names of the timeseries,
#' e.g. \code{\link{tolower}}
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

#' @param name_fun function to apply to the names of the timese, tries
#' @return a \code{regts} object
#'
#' @examples
#' \dontrun{
#' read_ts_csv("series.csv", sep = ";", dec = ",")
#' read_ts_csv("data.csv", labels = "after", name_fun = tolower)
#' }
#'
#' @importFrom data.table fread
#' @export
read_ts_csv <- function(filename, rowwise, frequency = NA,
                        skiprow = 0, skipcol = 0,
                        labels = c("no", "after", "before"),
                        sep = "auto", fill = FALSE,
                        dec = if (sep != ".") "." else ",",
                        name_fun) {

  if (!missing(skiprow)) {
    skip <- skiprow
  } else {
    skip <- 0
  }

  if (missing(rowwise) || rowwise) {
    first_line <- fread(filename, nrows = 1, skip = skip, header = FALSE,
                        data.table = FALSE, sep = sep, dec = dec, fill = fill)
    is_period <- is_period_text(get_strings(first_line), frequency)
    first_prd_col <- Position(function(x) {x}, is_period)
    if (missing(rowwise)) {
      rowwise <- !is.na(first_prd_col)
    }
  }

  # read the data data frame. For rowwise timeseries, the time index is put in
  # the header
  if (rowwise) {
    nper <- length(is_period) - first_prd_col + 1
    colClasses <- c(rep("character", first_prd_col - 1), rep("numeric", nper))
    df <- fread(filename, skip = skip, header = TRUE, data.table = FALSE,
                colClasses = colClasses, sep = sep, dec = dec,
                fill = fill)
  } else {
    # we use argument header = FALSE, because otherwise fread generates
    # dummy column names VX (X is number) for all columns with an empty
    # header.
    df <- fread(filename, skip = skip, header = FALSE, data.table = FALSE,
                sep = sep, dec = dec, fill = fill)
  }

  if (!missing(skipcol) && skipcol > 0) {
    df <- df[ , -(1:skipcol), drop = FALSE]
  }

  if (rowwise) {
    # use numeric = FALSE, because we already know that the timeseries
    # are numeric (see code above)
    return(read_ts_rowwise(df, frequency = frequency, labels = labels,
                           dec = dec, name_fun = name_fun))
  } else {
    return(read_ts_columnwise(df, frequency = frequency, labels = labels,
                             dec = dec, name_fun = name_fun))
  }
}

# internal function to read timeseries rowwise from a data frame with
# the time index in the column header.
# is numeric = TRUE, then the timeseries are converted to numeric
read_ts_rowwise <- function(df, frequency, labels = c("no", "after", "before"),
                            name_fun, dec = ".") {

  labels <- match.arg(labels)

  # Sometimes, in Exinteger numbers are stored internally as
  # for example "2010.0". The corresponding column name then becomes "2010.0".
  # This is the case for the Excel files written by Isis with
  # the "nice" method. Therefore we have to remove the redundant .0 in this situation.
  if (is.na(frequency) || frequency == 1) {
    cnames <- colnames(df)
    sel <- grep("^\\d{1,4}\\.0+$", cnames)
    cnames[sel] <- gsub("\\.0+$", "", cnames[sel])
    colnames(df) <- cnames
  }

  is_period <- is_period_text(get_strings(colnames(df)), frequency)
  first_prd_col <- Position(function(x) {x}, is_period)
  if (is.na(first_prd_col)) {
    stop("No periods found when reading rowwise timeseries")
  }
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

  # remove columns to be ignored
  col_sel <- is_period
  col_sel[1:(first_prd_col - 1)] <- TRUE
  if (labels == "no") {
    # remove label colums
    col_sel[label_cols] <- FALSE
    label_cols <- numeric(0)
  }
  df <- df[ , col_sel, drop = FALSE]


  data_cols <- (max(c(name_col, label_cols)) + 1) : ncol(df)

  # convert all data columns to numerical columns, taking the decimal separator
  # into account
  mat <- numeric_matrix(df[, data_cols, drop = FALSE], dec = dec)
  names <- df[, name_col]
  mat <- t(mat)
  colnames(mat) <- names

  # remove columns with empty names
  mat <- mat[ , which(!(get_strings(colnames(mat)) == "")), drop = FALSE]

  # convert the matrix to a regts, using numeric = FALSE, because we already
  # know that df is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && length(label_cols) > 0) {
    lbls <- df[, label_cols, drop = FALSE]
    l <- lapply(lbls[names%in% colnames(ret), , drop = FALSE], get_strings)
    l$names <- NULL
    lbls <- do.call(paste, l)
    lbls <- trimws(lbls)
    if (any(labels != "")) {
      ts_labels(ret) <- lbls
    }
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

# Internal function to read timeseries columnwise from a dataframe.
# This function is used in function read_ts_csv and read_ts_xlsx
read_ts_columnwise <- function(df, frequency = NA,
                               labels = c("no", "after", "before"),
                               name_fun, dec =  ".") {

  labels <- match.arg(labels)

  # remove all columns with only NAs
  all_na <- sapply(df, FUN = function(x) {!all(is.na(x))})
  df <- df[ , all_na, drop = FALSE]

  period_info <- find_period_column(df, frequency)
  time_column <- period_info$col_nr
  is_period <- period_info$is_period
  first_data_row <- Position(function(x) {x}, is_period)
  if (is.na(first_data_row)) {
    stop("No periods found when reading columnwise timeseries")
  }

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

  # remove column withouts names to the right of the time_columns
  keep_cols <- get_strings(df[name_row, ]) != ""
  keep_cols[time_column] <- TRUE
  if (time_column > 1) {
    keep_cols[1:(time_column - 1)] <- FALSE
  }
  df <- df[, keep_cols, drop = FALSE]

  if (labels != "no") {
    lbl_data <- df[label_rows, , drop = FALSE]
    lbl_data <- lbl_data[ , -1, drop = FALSE]
    if (length(label_rows) == 1) {
      labels <- get_strings(lbl_data)
    } else {
      labels <- as.data.frame(t(lbl_data))
      l <- lapply(labels, get_strings)
      labels <- do.call(paste, l)
      labels <- trimws(labels)
    }
  }

  colnames(df) <- df[name_row, , drop = FALSE]

  # remove rows without period
  df <- df[is_period, , drop = FALSE]

  # put time column in the row names, then the conversion of
  # data frame to numeric is more efficient
  rownames(df) <- df[, 1]
  df <- df[, -1, drop = FALSE]

  # remove columns with empty names
  df <- df[ , which(!(get_strings(colnames(df)) == "")), drop = FALSE]

  datamat <- numeric_matrix(df, dec = dec)

  # set numeric = FALSE, because we already know that df is numeric
  ret <- as.regts(datamat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && any(labels != "")) {
    ts_labels(ret) <- labels
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

# internal function: find period column in tibble read by read_excel
find_period_column <- function(df, frequency) {

  for (i in 1:ncol(df)) {
    is_period <- is_period_text(get_strings(df[, i]), frequency)
    if (any(is_period)) {
      col_index <- i
      row_nr <- Position(function(x) {x}, is_period)
      return(list(col_nr = i, is_period = is_period))
    }
  }

  stop("No periods found for columnwise timeseries!")
}


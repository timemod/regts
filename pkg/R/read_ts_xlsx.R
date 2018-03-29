#' Read timeseries from a sheet of an  xls(x) file
#'
#' This function attempts to read timeseries from an xls(x) file.
#' The xls(x) file is actually read by function \code{\link[readxl]{read_excel}}
#' of package \code{readxl}.
#' The timeseries can be stored both rowwise or columnwise on the sheet.
#' The function tries to find period cells on the sheet.
#' Period cells should contain a text with the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"}, \code{"2010.2Q"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"},
#' Period cells may also contain an integer value (e.g. \code{2018}), which
#' is considered as a year.
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
#' The first non-empty column in the sheet should contain  the timeseries names.
#' Otherwise, use argument \code{skipcol} to specify the number of columns
#' to skip.
#'
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=260}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
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
#' For columnwise timeseries, the first non-empty row that has been read (see
#' argument \code{range} or \code{skiprow}) should contain the variable names.
#' The periods can be in any column on the sheet.
#' All columns to the left of the time column are ignored.

#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=240}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' There may be one or more rows between the column names and the rows
#' where the actual timeseries are stored.
#' If argument \code{labels = "after"}  then the texts in these
#' rows will be used to create timeseries labels.
#'  If \code{labels = "before"},
#' the last row before the data is supposed to contain
#' the variable names. Now the row before the variable name columns
#' should contain label information. If argument \code{use_colnames = TRUE},
#' then the label option \code{"before"} is not allowed for columnwise
#' timeseries, since in that case the column names are the timeseries names.
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
#' @param filename  a string with the filename
#' @param sheet Sheet to read. Either a string (the name of a sheet),
#' or an integer (the position of the sheet). Ignored if the sheet is
#' specified via range. If neither argument specifies the sheet,
#' defaults to the first sheet
#' @param  range	A cell range to read from, as described in
#' \code{\link[readxl]{cell-specification}}. Includes typical Excel ranges
#' like "B3:D87", possibly including the
#' sheet name like "Budget!B2:G14", and more.
#' Strictly, even if the range forces the inclusion of leading or trailing
#' empty rows or columns.
#' Takes precedence over skiprow, skipcol and sheet
#' @param skiprow the number of rows to skip, including leading empty rows.
#' Ignored if \code{range} is given. By default, all leading empty rows are
#' skipped.
#' @param skipcol the number of columns to skip, including empty columns.
#' Ignored if \code{range} is given. By default, all leading empty columns are
#' skipped.
#' @param rowwise a logical value: are the timeseries stored rowwise?
#' If not specified, then \code{read_ts_xlsx} tries to figure out itself if
#' the timeseries are stored rowwise or columnwise
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains period texts without
#' frequency indicator (for example "2011-1")
#' @param labels label option. See details.
#' @param na_string Character vector of strings to use for missing values.
#' By default, \code{read_ts_xlsx} treats blank cells as missing data.
#' @param name_fun function to apply to the names of the timeseries
#' @return a \code{\link{regts}} object
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
#' @export
read_ts_xlsx <- function(filename, sheet = NULL, range = NULL,
                         skiprow = NA, skipcol = NA, rowwise, frequency = NA,
                         labels = c("no", "after", "before"),
                         na_string = "", name_fun) {

  if (missing(range)) {
    range <- cell_limits()
    range$ul[1] <- skiprow + 1
    range$ul[2] <- skipcol + 1
  } else {
    range <- as.cell_limits(range)
  }

  na_string <- union(na_string, "")

  tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                    col_types = "list", na = na_string)

  sheetname <- if (is.null(sheet)) "1" else as.character(sheet)

  if (nrow(tbl) == 0 && ncol(tbl) == 0) {
    stop(sprintf("Sheet %s of file %s is empty\n", sheetname, filename))
  }

  # remove all columns with only NAs
  not_all_na <- sapply(tbl, FUN = function(x) {!all(is.na(x))})
  tbl <- tbl[ , not_all_na]

  period_info <- find_periods(tbl, frequency, rowwise)

  if (is.null(period_info)) {
    stop(sprintf("No periods found on Sheet %s of file %s\n", sheetname,
                 filename))
  }

  if (period_info$rowwise) {
    ret <- read_ts_tbl_rowwise(tbl, frequency = frequency, labels = labels,
                               period_info = period_info)
  } else {
    ret <- read_ts_tbl_columnwise(tbl, frequency = frequency, labels = labels,
                                  period_info = period_info)
  }


  # apply function to column names if given
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
read_ts_tbl_rowwise <- function(tbl, frequency,
                                labels = c("no", "after", "before"),
                                period_info) {

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
    label_cols <- seq_len(name_col - 1)
  } else if (labels == "after") {
    if (first_prd_col >= 3) {
      label_cols <- 2 : (first_prd_col - 1)
    } else {
      label_cols <- numeric(0)
    }
  } else {
    label_cols <- numeric(0)
  }

  # keep only period columns, the name column and the label columns
  col_sel <- is_period
  col_sel[c(name_col, label_cols)] <- TRUE
  tbl <- tbl[ , col_sel]

  data_cols <- (max(name_col, label_cols) + 1) : ncol(tbl)

  periods <- tibble_2_char(tbl[1, data_cols])

  names <- tibble_2_char(tbl[-1, name_col], replace_na = FALSE)
  name_sel <- !is.na(names)
  names <- names[name_sel]

  # remove rows without names, including the row with the period
  tbl <- tbl[c(FALSE, name_sel), ]

  # convert data columns to a numeric matrix, employing C++ function
  # list_tbl_2_mat.
  mat <- list_tbl_2_mat(tbl[, data_cols])

  mat <- t(mat)
  rownames(mat) <- periods
  colnames(mat) <- names

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
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

# Internal function to read timeseries columnwise from a tibble read in
# with readxl::read_excel.
read_ts_tbl_columnwise <- function(tbl, frequency = NA,
                                   labels = c("no", "after", "before"),
                                   period_info) {

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

  # keep the period column and the columnws with names
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
  mat <- list_tbl_2_mat(tbl[-1])
  rownames(mat) <- tibble_2_char(tbl[[1]])
  colnames(mat) <- ts_names

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && length(label_rows) > 0 && any(lbls != "")) {
      ts_labels(ret) <- lbls
  }

  return(ret)
}


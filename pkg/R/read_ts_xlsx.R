#' Read timeseries from a sheet of an  xls(x) file
#'
#' This function attempts to read timeseries from an xls(x) file.
#' The xls(x) file is actually read by function \code{\link[readxl]{read_excel}}
#' of package \code{readxl}.
#' The timeseries can be stored both rowwise or columnwise on the sheet.
#' The function tries to find valid period texts on the sheet.
#' Valid period texts should have the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#'
#' An integer value is considered as a period wih frequency year.
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
#' function \code{read_ts_xlsx} searches for any valid period text in the first
#' row read. If a valid period was found, then
#' \code{read_ts_xlsx} assumes that the timeseries are stored rowwise.
#' Otherwise it assumes that the timeseries are stored columnwise.
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
#'\strong{columnwise timeseries}
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=240}}
#' \if{latex}{\figure{xlsschemacolumnwise.jpg}{options: width=5in}}
#'
#' For columnwise timeseries, the first row that was read (see
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
#' e.g. \code{\link{tolower}}
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
                         na_string = c(""), name_fun) {

  if (missing(range)) {
    range <- cell_limits()
    range$ul[1] <- skiprow + 1
    range$ul[2] <- skipcol + 1
  } else {
    range <- as.cell_limits(range)
  }

  tbl <- read_excel(filename, sheet, range = range, col_names = FALSE,
                    col_types = "list", na = na_string)

  if (nrow(tbl) == 0 && ncol(tbl) == 0) {
    if (is.null(sheet)) {
      sheetname <- "1"
    } else {
      sheetname <- as.character(sheet)
    }
    stop(sprintf("Sheet %s of file %s is empty\n", sheetname, filename))
  }

  if (missing(rowwise)) {
    first_row <- sapply(tbl[1, ], FUN = as.character, USE.NAMES = FALSE)
    is_period <- is_period_text(get_strings(first_row), frequency)
    rowwise <- any(is_period)
  }

  if (rowwise) {
    ret <- read_ts_tbl_rowwise(tbl, frequency = frequency, labels = labels)
  } else {
    ret <- read_ts_tbl_columnwise(tbl, frequency = frequency, labels = labels)
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
                                labels = c("no", "after", "before")) {

  labels <- match.arg(labels)

  first_row <- sapply(tbl[1, ], FUN = function(x) {as.character(x[[1]])})

  # Sometimes, in Excel integer numbers are stored internally as
  # for example "2010.0". The corresponding column name then becomes "2010.0".
  # This is the case for the Excel files written by Isis with
  # the "nice" method. Therefore we have to remove the redundant .0 in this situation.
  if (is.na(frequency) || frequency == 1) {
    sel <- grep("^\\d{1,4}\\.0+$", first_row)
    first_row[sel] <- gsub("\\.0+$", "", first_row[sel])
  }

  is_period <- is_period_text(get_strings(first_row), frequency)
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
  tbl <- tbl[ , col_sel, drop = FALSE]

  data_cols <- (max(c(name_col, label_cols)) + 1) : ncol(tbl)

  periods <- sapply(tbl[1, data_cols], FUN = function(x) {as.character(x[[1]])})

  names <- sapply(tbl[-1, name_col][[1]], FUN = function(x) x[[1]],
                  USE.NAMES = FALSE)
  name_sel <- which(!is.na(names))
  names <- names[name_sel]
  keep_rows <- c(1, name_sel + 1)
  tbl <- tbl[keep_rows, ]

  # now create data matrix
  mat <- tbl2nummat(tbl[-1, data_cols])
  mat <- t(mat)
  rownames(mat) <- periods
  colnames(mat) <- names

  # convert the matrix to a regts, using numeric = FALSE, because we already
  # know that df is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && length(label_cols) > 0) {
    lbls <- tbl[-1, label_cols]
    # TODO: is there a better way to do this
    for (i in 1:ncol(lbls)) {
      lbls[[i]] <- lapply(lbls[[i]], FUN = function(x) {ifelse(is.na(x), "", x)})
    }
    lbls <- do.call(paste, lbls)
    lbls <- trimws(lbls)
    if (any(labels != "")) {
      ts_labels(ret) <- lbls
    }
  }

  return(ret)
}

# Internal function to read timeseries columnwise from a tibble read in
# with readxl::read_excel.
read_ts_tbl_columnwise <- function(tbl, frequency = NA,
                                   labels = c("no", "after", "before")) {

  labels <- match.arg(labels)


  # remove all columns with only NAs
  all_na <- sapply(tbl, FUN = function(x) {!all(is.na(x))})

  tbl <- tbl[ , all_na, drop = FALSE]

  period_info <- find_period_column_tbl(tbl, frequency)

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
  keep_cols <- get_strings(sapply(tbl[1, ], FUN = as.character,
                                  USE.NAMES = FALSE)) != ""

  keep_cols[time_column] <- TRUE
  if (time_column > 1) {
    keep_cols[1:(time_column - 1)] <- FALSE
  }

  tbl <- tbl[, keep_cols, drop = FALSE]

  ts_names <- sapply(tbl[name_row, ], FUN = function(x) {as.character(x[[1]])},
                     USE.NAMES = FALSE)

  keep_cols <- which(!is.na(ts_names))
  keep_cols <- c(1, setdiff(keep_cols, 1))

  # remove columns without names
  tbl <- tbl[, keep_cols, drop = FALSE]
  colnames(tbl) <- ts_names[keep_cols]

  if (labels != "no") {
    lbl_data <- tbl[label_rows, , drop = FALSE]
    lbl_data <- lbl_data[ , -1, drop = FALSE]
    lbl_data[] <- lapply(lbl_data,
                         FUN = function(x) {ifelse(is.na(x),  "", as.character(x))})
    if (length(label_rows) == 1) {
      lbls <- get_strings(sapply(lbl_data, FUN = function(x) {x[[1]]},
                                 USE.NAMES = FALSE))
    } else {
      lbls <- as.data.frame(t(lbl_data))
      l <- lapply(lbls, get_strings)
      lbls <- do.call(paste, l)

    }
    lbls <- trimws(lbls)
  }

  # remove rows without period (the names have already been stored in the
  # column names and the labels in variable lbls).
  tbl <- tbl[is_period, , drop = FALSE]

  # convert columns to numeric or character
  periods <- sapply(tbl[[1]], FUN = as.character)

  mat <- tbl2nummat(tbl[, -1])
  rownames(mat) <- periods

  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && any(lbls != "")) {
    ts_labels(ret) <- lbls
  }

  return(ret)
}

# internal function: find period column in tibble read by read_excel
find_period_column_tbl <- function(tbl, frequency) {

  for (i in 1:ncol(tbl)) {
    is_period <- is_period_text(get_strings(tbl[[i]]), frequency)
    if (any(is_period)) {
      col_index <- i
      row_nr <- Position(function(x) {x}, is_period)
      return(list(col_nr = i, is_period = is_period))
    }
  }

  stop("No periods found for columnwise timeseries!")
}

# internal function to convert a tible containing data only
# to a numeric matrix, giving warnings when some values could not be
tbl2nummat <- function(tbl) {

  # check for texts in  tbl. Use C++ function is_character_list
  # because this is much faster than
  # sapply(tbl, FUN = function(x) {sapply(x, is.character)}
  is_char <- sapply(tbl, FUN = is_character_list)
  if (any(is_char)) {
    texts <- as.character(as.data.frame(tbl)[is_char])
    ntexts <- length(texts)
    NTEXTS_MAX <- 10
    nmax <- min(NTEXTS_MAX, ntexts)
    texts <- paste0("\"", texts[1:nmax], "\"")
    if (ntexts <= NTEXTS_MAX) {
      warning(paste0("NAs introduced by coercion\n",
                         "The following texts could not be converted to numeric:\n",
                       paste0(texts, collapse = "\n")))
    } else {
      warning(paste0("NAs introduced by coercion.\n",
                     ntexts, " texts could not be converte to numeric.\n",
                     "The first ", nmax, " texts that gave problems are:\n",
                      paste0(texts, collapse = "\n")))
    }
  }

  suppressWarnings(tbl[] <- lapply(tbl, FUN = as.numeric))

  mat <- as.matrix(tbl)

  return(mat)
}


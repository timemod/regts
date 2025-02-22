#' Read timeseries from a csv file
#'
#' This function reads timeseries from a csv file,
#' employing function \code{\link[data.table]{fread}} of package
#' \code{data.table}.
#' The functions searches for period texts and automatically
#' determines how the timeseries are stored (rowwise or columnwise)
#' and which columns contain the numerical values of the timeseries.
#' Period texts should have the format recognized by function
#' \code{\link{period}}, for example \code{"2010Q2"}, \code{"2010.2Q"},
#' \code{"2010m2"}, \code{"2011"} or \code{"2011-1"}. Use argument
#' \code{period_fun} if the period texts have a different format.
#'
#' In many cases, this function will read timeseries correctly.
#' If the function fails or if the result is not what you want,
#' it might help to specify arguments \code{rowwise}, \code{frequency},
#' \code{period_fun}, \code{skipcol} or \code{skiprow}.
#' Specify option  \code{rowwise} if you know
#' that the timeseries are stored rowwise or columnwise. Specify
#' argument \code{frequency} if you already know the frequency of the
#' timeseries.
#' Arguments \code{skipcol} and \code{skiprow} can be used to read only a
#' part of the file.
#' If that does not help, then you can read the data into a data frame
#' (for example by using function \code{\link{read.csv}} or function
#' \code{\link[data.table]{fread}} of package \code{data.table}),
#' then convert the data frame to a standard columnwise data frame
#' and finally convert it to a \code{\link{regts}} by using function
#' \code{\link{as.regts}}.
#'
#' If argument \code{rowwise} has not been specified, then
#' function \code{read_ts_csv} tries to guess if the timeseries are stored
#' rowwise or columnwise based on the positions of the fields with period texts.
#'
#' \strong{rowwise timeseries}
#'
#' For rowwise timeseries, the function searches for the first
#' row with periods. All rows before the period row are ignored.
#' Columns without a valid period in the period row are also ignored.
#' The first non-empty column should contain the timeseries names
#' (or labels if argument \code{labels = "before"}, see the discussion below).
#' Otherwise use argument \code{skipcol} to specify the number of
#' columns to skip.
#'
#' \if{html}{\figure{xlsschemarowwise.jpg}{options: width=200}}
#' \if{latex}{\figure{xlsschemarowwise.jpg}{options: width=5in}}
#'
#' There may be more than one column before the columns with timeseries values
#' (data columns). In that case one column should contain the variable names.
#' The other columns before the first data column are used to create
#' timeseries labels (see \code{\link{ts_labels}}). If argument
#' \code{labels = "after"} (default), then the first column contains the
#' variable names. If \code{labels = "no"} the first column
#' also contains variable names but the other columns before the first data
#' column are ignored. If argument \code{labels = "before"}, then the variable
#' names should be in the last column before the first data column.
#'
#' With argument \code{name_fun} a function can be applied to names of the
#' timeseries, e.g. \code{\link{tolower}}.
#'
#' \strong{columnwise timeseries}
#'
#' For columnwise timeseries, the first non-empty row that is not skipped (see
#' argument \code{skiprow}) should contain the variable names
#' (or labels if argument \code{labels = "before"}, see the discussion below).
#' The periods can be in any column.
#' Rows without a valid period in the period column are ignored.
#' All columns to the left of the period column are also ignored.
#'
#' \if{html}{\figure{xlsschemacolumnwise.jpg}{options: width=200}}
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
#' @param filename  a string with the filename.
#' @param skiprow the number of rows to skip.
#' If 0 (default) and if argument \code{fill} is \code{FALSE},
#' then comment rows are automatically skipped.
#' See Details.
#' @param skipcol the number of columns to skip.
#' @param rowwise a logical value: are the timeseries stored rowwise?
#' If not specified, then \code{read_ts_csv} tries to figure out itself if
#' the timeseries are stored rowwise or columnwise.
#' @param frequency the frequency of the timeseries.
#' This argument is mandatory if the file contains a period texts without
#' frequency indicator (for example "2011-1").

#' @param labels label option. See Details.
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
#' @param name_fun function to apply to the names of the timeseries.
#' @param period_fun function applied to period texts. This should be a function
#' that converts a character vector to another character vector or a
#' \code{period} vector with the same length. Use this argument if the period
#' texts do not have a
#' standard format (see Description).
#' @param strict A logical. If \code{TRUE} (the default) all periods between the
#' start and the end period must be present.
#' Otherwise the timeseries are filled with \code{NA} for the missing periods.
#' @param warn_dupl A logical. If \code{TRUE} (the default), a warning is
#' issued if there are duplicate column names in the returned timeseries object.
#' @param verbose A logical (default `FALSE`). If `TRUE`, the function
#' prints the filename, the number of timeseries read, the period range, and
#' the elapsed time.
#' @return a \code{regts} object
#'
#' @examples
#' \dontrun{
#' read_ts_csv("series.csv", sep = ";", dec = ",")
#' read_ts_csv("data.csv", labels = "after", name_fun = tolower)
#' }
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @seealso \code{\link{write_ts_csv}} and \code{\link{read_ts_xlsx}}
#' @export
read_ts_csv <- function(filename, skiprow = 0, skipcol = 0,
                        rowwise, frequency = NA,
                        labels = c("after", "before", "no"),
                        sep = "auto", fill = FALSE,
                        dec = if (sep != ".") "." else ",",
                        na_string = "", name_fun, period_fun, strict = TRUE,
                        warn_dupl = TRUE, verbose = FALSE) {

  na_string <- union(na_string, "")
  labels <- match.arg(labels)

  if (!missing(name_fun) && !is.function(name_fun)) {
    stop("argument name_fun is not a function")
  }
  if (!missing(period_fun) && !is.function(period_fun)) {
    stop("argument period_fun is not a function")
  }

  if (verbose) {
    cat(sprintf("\nReading timeseries from file %s ...\n", filename))
    t_start <- Sys.time()
  }

  tbl <- read_csv_file(filename, skiprow = skiprow, skipcol = skipcol,
                       na_string = na_string, sep = sep, dec = dec,
                       fill = fill)

  layout <- inspect_tibble(tbl, frequency, rowwise, labels, xlsx = FALSE,
                           period_fun = period_fun, name_fun = name_fun)
  if (is.null(layout)) {
    stop(sprintf("No periods found in file %s\n", filename))
  }

  if (layout$rowwise) {
    ret <- read_ts_rowwise(tbl, frequency = frequency, labels = labels,
                           dec = dec, name_fun = name_fun, layout = layout,
                           strict = strict, filename = filename)
  } else {
    ret <- read_ts_columnwise(tbl, frequency = frequency, dec = dec,
                              period_fun = period_fun, layout = layout,
                              strict = strict, filename = filename,
                              skipcol = skipcol)
  }

  # check for duplicate names
  if (warn_dupl) test_duplicates(ret, filename)

  if (verbose) {
    t_end <- Sys.time()
    secs <- t_end - t_start
    cat(sprintf("%d timeseries read, period range %s, %.2f sec. elapsed.\n\n",
                ncol(ret), get_period_range(ret), secs))
  }

  return(ret)
}

read_csv_file <- function(filename, skiprow, skipcol, na_string, sep, dec,
                          fill) {

  df <- fread(filename, skip = skiprow, header = FALSE, data.table = FALSE,
              sep = sep, dec = dec, fill = fill, colClasses = "character",
              na.strings = na_string)

  if (!missing(skipcol) && skipcol > 0) {
    df <- df[, -(1:skipcol), drop = FALSE]
  }

  if (nrow(df) == 0 && ncol(df) == 0) {
    stop(sprintf("File %s is empty\n", filename))
  }

  tbl <- as_tibble(df)
  return(tbl)
}

# Internal function to read timeseries rowwise from a tibble, used by
# read_ts_csv.
read_ts_rowwise <- function(tbl, frequency, labels, dec, name_fun, layout,
                            strict, filename) {

  if (is.list(layout$periods)) {
    # There are periods with differnet frequencies.
    # The exact row number is unknown, because of the automatic row skipping.
    # Therefore the error message cannot be as specific as for read_ts_xlsx.
    stop(sprintf(paste0("Periods with different frequencies found in the ",
                        "%d'th non-skipped row in file file %s."),
                 layout$period_row, filename))
  }

  # obtain the data part of the tibble: the data below the period row.
  data_tbl <- tbl[-(1:layout$period_row), ]

  name_info <- get_name_info_rowwise(layout, data_tbl, labels, name_fun)

  rowsel <- name_info$row_has_name
  colsel <- layout$is_data_col
  mat <- df_to_numeric_matrix(data_tbl[rowsel, colsel], dec = dec)
  mat <- t(mat)
  colnames(mat) <- name_info$names

  # convert the matrix to a regts, using numeric = FALSE because we already
  # know that mat is numeric
  ret <- matrix2regts_(mat, layout$periods, numeric = FALSE, strict = strict)

  if (!is.null(name_info$lbls)) {
    ts_labels(ret) <- name_info$lbls
  }

  return(ret)
}

# Internal function to read timeseries columnwise from a tibble, used
# by function read_ts_csv.
read_ts_columnwise <- function(tbl, frequency, dec, period_fun, layout, strict,
                               filename, skipcol) {

  # remove all rows without period (the names and labels are already in
  # layout)
  tbl <- tbl[layout$is_data_row, ]

  periods <- get_periods_data(tbl[[layout$period_col]], frequency,
                              xlsx = FALSE, period_fun = period_fun)
  if (is.list(periods)) {
    # There are periods with differnet frequencies.
    # The exact row number is unknown, because of the automatic row skipping.
    # Therefore the error message cannot be as specific as for read_ts_xlsx.
    col <- num_to_letter(layout$period_col + skipcol)
    stop(sprintf(paste("Periods with different frequencies found in column %s",
                       "of file %s."), col, filename))
  }

  # convert data columns to a numeric matrix, employing function
  # df_to_numeric_matrix
  mat <- df_to_numeric_matrix(tbl[layout$is_data_col], dec = dec)
  colnames(mat) <- layout$names

  # set numeric = FALSE, because we already know that df is numeric
  ret <- matrix2regts_(mat, periods, numeric = FALSE, strict = strict)

  if (!is.null(layout$lbls)) {
    ts_labels(ret) <- layout$lbls
  }

  return(ret)
}

# Convert a character data frame to a numeric matrix.
# This function is similar to function numeric_matrix (see file regts.R), but
# more efficient because we already know that x only contains texts and that
# NA values are treated correctly.
df_to_numeric_matrix <- function(x, dec) {

  if (nrow(x) == 0 || ncol(x) == 0) {
    # no data available
    return(matrix(0.0, nrow = nrow(x), ncol = ncol(x)))
  }

  text_mat <- as.matrix(x)

  if (dec != ".") {
    # convert decimal separator
    text_mat_conv <- sub(dec, ".", text_mat, fixed = TRUE)
  } else {
    text_mat_conv <- text_mat
  }

  suppressWarnings({
    num_mat <- as.numeric(text_mat_conv)
    dim(num_mat) <- dim(text_mat)
  })

  error_sel <- is.na(num_mat) & !is.na(text_mat)
  if (any(error_sel)) {
    weird_texts <- unique(text_mat[error_sel])
    nweird <- length(weird_texts)
    nweird_max <- 10
    nmax <- min(nweird_max, nweird)
    weird_texts <- paste0("\"", weird_texts[1:nmax], "\"")

    if (nweird <= nweird_max) {
      warning(paste0("NAs introduced by coercion.\n",
                     "The following texts could not be converted to numeric:\n",
                     paste0(weird_texts, collapse = "\n")))
    } else {
      warning(paste0("NAs introduced by coercion.\n",
                     nweird, " texts could not be converted to numeric.\n",
                     "The first ", nweird_max,
                     " texts that gave problems are:\n",
                     paste0(weird_texts, collapse = "\n")))
    }
  }

  return(num_mat)
}

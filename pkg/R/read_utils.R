# converts a tibble to a character vector.
tibble_2_char <- function(tbl, replace_na = TRUE) {
  ret <- unlist(tbl, use.names = FALSE)
  ret <- as.character(ret)
  if (replace_na) {
    ret[is.na(ret)] <- ""
  }
  return(ret)
}


# return a logical vector, where each element indicates whether the
# corresponding element in tbl is a period
is_period_tbl <- function(tbl, frequency, xlsx, period_fun) {

  period_texts <- tibble_2_char(tbl)

  if (!missing(period_fun)) {
    period_texts <- sapply(period_texts, FUN = period_fun, USE.NAMES = FALSE)
  }

  is_per_text <- is_period_text(period_texts, frequency)
  if (xlsx && (!is.na(frequency) && 12 %% frequency == 0)) {
    # When read from an xlsx file the tbl may contain  POSIXt values.
    # POSIXt values are possible periods if the frequency is a divisor of 12.
    is_posixt <- sapply(tbl, FUN = function(x) {inherits(x[[1]], "POSIXt")},
                        USE.NAMES = FALSE)
    return(is_per_text | unname(is_posixt))
  } else {
    # csv
    return(is_per_text)
  }
}

# Returns the periods as either a character or Date vector
get_periods_tbl <- function(tbl, frequency, xlsx, period_fun) {
  if (xlsx &&  (!is.na(frequency) && 12 %% frequency == 0)) {
    # when read from xlsx the tbl may contain POSIXt data
    is_posixt <- sapply(tbl, FUN = function(x) {inherits(x[[1]], "POSIXt")})
    has_posixt <- any(is_posixt)
  } else {
    # csv file, no posixt values possible
    has_posixt <- FALSE
  }
  if (has_posixt) {
    # convert all periods to Dates, and return a Date vector
    period_fun_given <- !missing(period_fun)
    conv_date <- function(x) {
      x <- x[[1]]
      if (inherits(x, "POSIXt")) {
        return(as.Date(x))
      } else {
        # apply period_fun and convert to Date
        if (period_fun_given) x <- period_fun(x)
        return(as.Date(period(x, frequency)))
      }
    }
    date_list <- lapply(tbl, FUN = conv_date)
    # convert to vector of Dates (sapply does not work)
    return(do.call(c, date_list))
  } else {
    # return a character vector
    ret <- tibble_2_char(tbl, FALSE)
    if (!missing(period_fun)) {
      ret <- sapply(ret, FUN = period_fun, USE.NAMES = FALSE)
    }
    return(ret)
  }
}


# Internal function: finds the first row or column of the period data in a
# tibble read by read_ts_xlsx or read_ts_csv. The function also determines
# which column of the tibble contains timeseries data. Returns NULL if no
# period has been found.
#
# The function does not assume that all rows of the csv file or Excel sheet
# have been read. read_ts_xlsx first reads the first 25 rows, then calls
# get_tbl_layout to determine the position of the period data and
# the numerical data columns. The data are actually read in a second stage
# using this information. Currently, read_ts_csv reads all rows in one go,
# but get_tbl_layout does not use that information.
#
# For rowwise timeseries, the function returns the periods and for columnwise
# timeseries the names and labels.  The function does not return names and labels
# for rowwise timeseries, because the input tibble may not contain all rows
# (see the discussion above). For the same reason, the function does not return
# the periods for columnwise timeseries.
get_tbl_layout <- function(tbl, frequency, rowwise, labels, xlsx, period_fun,
                           name_fun) {

  first_row_nr <- get_first_non_empty_row(tbl)

  found <- FALSE

  if (!missing(rowwise) && !rowwise) {
    # columnwise

    for (col_nr in 1:ncol(tbl)) {
      is_period <- is_period_tbl(tbl[[col_nr]], frequency, xlsx, period_fun)
      if (any(is_period)) {
        last_col <- Position(function(x) {x}, is_period, right = TRUE)
        if (last_col > 1) {
          found <- TRUE
          break
        }
      }
    }

    if (found) row_nr <- Position(function(x) {x}, is_period)

  }  else {
    # rowwise or columnwise

    first_row_nr <- get_first_non_empty_row(tbl)
    if (is.na(first_row_nr)) return(NULL)

    # first search for a period rowwise
    for (row_nr in first_row_nr:nrow(tbl)) {
      is_period_row <- is_period_tbl(tbl[row_nr, ], frequency, xlsx, period_fun)
      if (any(is_period_row)) {
        col_nr <- Position(function(x) {x}, is_period_row)
        if (missing(rowwise)) {
          if (row_nr == first_row_nr) {
            rowwise <- TRUE
          } else {
            is_period_col <- is_period_tbl(tbl[[col_nr]], frequency, xlsx,
                                           period_fun)
            rowwise <- is_rowwise(row_nr, col_nr, is_period_row, is_period_col,
                                  tbl, frequency, xlsx)
          }
        }
        if (rowwise) {
          last_col <- Position(function(x) {x}, is_period_row, right = TRUE)
          if (last_col > 1) {
            found <- TRUE
            break
          }
        } else {
          last_row <- Position(function(x) {x}, is_period_col, right = TRUE)
          if (last_row > 1) {
            found <- TRUE
            break
          }
        }
      }
    }

    if (found) {
      is_period <- if(rowwise) is_period_row else is_period_col
    }
  }

  if (!found) return(NULL)

  if (rowwise) {

    first_data_col <- col_nr

    if (first_data_col == 1) {
      # a period on the first column for a rowwise timeseries should be ignored,
      # because the column to the left of the first data column should contain
      # variable names
      is_period[1] <- FALSE
      first_data_col <- Position(identity, is_period)
      if (is.na(first_data_col)) return(NULL)
    }

    last_data_col <- Position(identity, is_period, right = TRUE)

    periods <- get_periods_tbl(tbl[row_nr, is_period], frequency, xlsx,
                               period_fun)

    return(list(rowwise = TRUE, period_row = row_nr,
                first_data_col = first_data_col, last_data_col = last_data_col,
                is_data_col = is_period, periods = periods))

  } else {
    # columnwise timeseries

    first_data_row <- row_nr

    if (is_period[first_row_nr]) {
      # if the first period row is the first non-empty row, then
      # ignore that period, since the row should contain variable names
      is_period[first_row_nr] <- FALSE
      first_data_row <- Position(identity, is_period)
      if (is.na(first_data_row)) return(NULL)
    }

    period_col <- col_nr
    name_info <- get_name_info_colwise(first_data_row, first_row_nr,
                                       period_col, tbl, labels, name_fun)

    last_data_col <- Position(identity, name_info$col_has_name, right = TRUE)

    return(list(rowwise = FALSE, period_col = period_col,
                first_data_row = first_data_row, last_data_col = last_data_col,
                is_data_col = name_info$col_has_name, is_data_row = is_period,
                names = name_info$names, lbls = name_info$lbls))
  }
}

is_rowwise <- function(row_nr, col_nr, is_period_row, is_period_col,
                       tbl, frequency, xlsx) {

  if (col_nr == 1) {
    return(FALSE)
  }

  is_period_sum_row <- sum(is_period_row)
  is_period_sum_col <- sum(is_period_col)

  if (is_period_sum_row == 1 && is_period_sum_col == 1) {
    # There is a single period. Count the number of non-NA numerical
    # values in row.
    if (xlsx) {
      # for xlsx, a NA value will have type logical.
      num_period_row <- sapply(tbl[row_nr, ],
                             FUN = function(x) is.numeric(x[[1]]))
      num_period_col <- sapply(tbl[[col_nr]], FUN = is.numeric)
      return(sum(num_period_row) > sum(num_period_col))
    } else {
      period_row <- unlist(tbl[row_nr, is_period_row])
      num_period_row <- grepl(year_pattern, period_row)
      period_col <- unlist(tbl[is_period_col, col_nr])
      num_period_col <- grepl(year_pattern, period_col)
      return(sum(num_period_row) > sum(num_period_col))
    }
  } else if (is_period_sum_row == 1) {
    return(FALSE)
  } else if (is_period_sum_col == 1) {
    return(TRUE)
  }

  if (xlsx && (is.na(frequency) || frequency == 1)) {

    # For frequency 1, an integer numeric value is also considered as a
    # period. If the periods on the period row only
    # contain non-numeric period objects, then this suggest that we have a
    # rowwise timeseries.

    num_period_row <- sapply(tbl[row_nr, is_period_row],
                             FUN = function(x) is.numeric(x[[1]]))
    if (!any(num_period_row)) {
      # all periods on row row_nr are non-numerical:
      # assume rowwise timeseries
      return(TRUE)
    }

    num_period_col <- sapply(tbl[is_period_col, col_nr][[1]],
                             FUN = is.numeric)
    if (!any(num_period_col)) {
      # all periods on column col_nr are non-numerical:
      # assume columnwise timeseries
      return(FALSE)
    }

    # TODO: if all periods are numeric, then check the min and max value.
    # If on the first row the maximum is larger than 1900 and smaller than 3000,
    # then the timeseries is probably rowwise.

  } else if (!xlsx && is.na(frequency)) {

    # For csv files we cannot distinguish character values and numerical
    # values. However, if the frequency has not been specified and the
    # period row contains a period simple integer value, we use the same
    # approach.
    year_pattern <- "^-?\\d+$"
    period_row <- unlist(tbl[row_nr, is_period_row])
    num_period_row <- grepl(year_pattern, period_row)
    if (!any(num_period_row)) {
      # all periods on row row_nr are not simple integer numbers:
      # assume rowwise timeseries
      return(TRUE)
    }

    period_col <- unlist(tbl[is_period_col, col_nr])
    num_period_col <- grepl(year_pattern, period_col)
    if (!any(num_period_col)) {
      # all periods on column col_nr are non-numerical:
      # assume columnwise timeseries
      return(FALSE)
    }
  }


  # if there is no information available, then simply use the maximum
  # number of periods objects in the row or column.
  return(is_period_sum_row >= is_period_sum_col)
}




get_first_non_empty_row <- function(tbl) {
  for (row_nr in 1:nrow(tbl)) {
    if (any(!is.na(unlist(tbl[row_nr, ])))) {
      return(row_nr)
    }
  }
  return(NA)
}


get_first_non_empty_column <- function(tbl) {
  for (col_nr in 1:ncol(tbl)) {
    if (any(!is.na(tbl[[col_nr]]))) {
      return(col_nr)
    }
  }
  return(NA)
}

get_name_info_rowwise <- function(tbl_layout, data_tbl, labels, name_fun) {
  # get the names and labels for a rowwise tibble containing timeseries data.

  # find the first non-empty column
  first_col_nr <- get_first_non_empty_column(data_tbl)

  if (tbl_layout$first_data_col <= first_col_nr) {
    return(list(row_has_name = rep(FALSE, nrow(data_tbl)), names = character(0),
                lbls = character(0)))
  }

  # no labels if there is only one non-empty column before the first data column
  if (tbl_layout$first_data_col == first_col_nr + 1) labels = "no"

  name_col <- if (labels == "before") {
    tbl_layout$first_data_col - 1
  } else {
    first_col_nr
  }

  if (labels == "before") {
    label_cols <- first_col_nr : (name_col - 1)
  } else if (labels == "after") {
    label_cols <- (name_col + 1): (tbl_layout$first_data_col - 1)
  } else {
    label_cols <- numeric(0)
  }

  name_col_data <- data_tbl[[name_col]]
  row_has_name <- !is.na(name_col_data)

  # extract names
  names <- name_col_data[row_has_name]
  if (!missing(name_fun)) names <- name_fun(names)

  # extract labels
  if (labels != "no") {
    label_tbl <- data_tbl[row_has_name, label_cols]
    lbls <- get_labels_from_tbl(label_tbl, TRUE)
  } else {
    lbls <- NULL
  }

  return(list(row_has_name = row_has_name, names = names, lbls = lbls))
}


get_name_info_colwise <- function(first_data_row, first_row_nr, period_col, tbl,
                                  labels, name_fun) {

  # no labels if there is only one non-empty row before the first data row
  if (first_data_row == first_row_nr + 1) labels <- "no"

  # compute the row with variable names. 0 means: column names
  # and the label rows
  if (labels == "before") {
    name_row <- first_data_row - 1
    label_rows <- first_row_nr:(first_data_row - 2)
  } else {
    name_row <- first_row_nr
    if (first_data_row > first_row_nr + 1) {
      label_rows <- (first_row_nr + 1):(first_data_row -1)
    } else {
      label_rows <- integer(0)
    }
  }

  name_row_data <- tibble_2_char(tbl[name_row, ])

  col_has_name <- nzchar(name_row_data)
  col_has_name[1 : period_col] <- FALSE

  if (!any(col_has_name)) {
    names <- character(0)
    lbls <- character(0)
  } else {

    names <- name_row_data[col_has_name]

    # labels
    if (labels != "no") {
      label_tbl <- tbl[label_rows, col_has_name]
      lbls <- get_labels_from_tbl(label_tbl, FALSE)
    } else {
      lbls <- NULL
    }
  }
  return(list(col_has_name = col_has_name, names = names, lbls = lbls))
}

get_labels_from_tbl <- function(label_tbl, rowwise) {
  label_tbl[] <- lapply(label_tbl, FUN = function(x)
                        {ifelse(is.na(x),  "", as.character(x))})
  if (!rowwise) {
    label_tbl <- as.tibble(t(label_tbl))
  }
  if (ncol(label_tbl) == 1) {
    #lbls <- unlist(label_tbl, use.names = FALSE)
    lbls <- label_tbl[[1]]
  } else {
    lbls <- do.call(paste, label_tbl)
    # remove spurious spaces that are created when a label is "".
    lbls <- trimws(lbls, which = "right")
    lbls <- gsub(" {2,}", "", lbls)
  }
  if (!any(nzchar(lbls))) lbls <- NULL
  return(lbls)
}

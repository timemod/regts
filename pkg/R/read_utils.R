# converts a tibble to a character vector.
tibble_2_char <- function(tbl, replace_na = TRUE) {
  ret <- unlist(tbl, use.names = FALSE)
  ret <- as.character(ret)
  if (replace_na) {
    ret[is.na(ret)] <- ""
  }
  return(ret)
}


# Return a logical vector, where each element indicates whether the
# corresponding element in tbl is a period.
# INPUT:
#    data a list (xlsx) or character vector (csv) whose elements should be
#     checked
is_period_data <- function(data, frequency, xlsx, period_fun) {

   is_period_fun <- function(period_texts, frequency, period_fun) {
    # give a character vector with texts, this function returns a logical
    # vectors with TRUE and FALSE depending on the period
    if (!missing(period_fun)) {
      period_texts <- sapply(period_texts, FUN = period_fun, USE.NAMES = FALSE)
    }

    # call C++ function is_period_text
    return(is_period_text(period_texts, frequency))
  }

  if (xlsx) {

    is_text   <- sapply(data, FUN = is.character)
    is_num    <- sapply(data, FUN = is.numeric)

    is_period <- rep(FALSE, length(data))

    if (any(is_text)) {
      period_texts <- unlist(data[is_text], use.names = FALSE)
      #printobj(period_texts)
      is_period[is_text] <- is_period_fun(period_texts, frequency, period_fun)
    }

    if (any(is_num)) {
      num_values <- unlist(data[is_num], use.names = FALSE)
      is_period[is_num] <-  num_values %%  1 == 0
    }

    if (is.na(frequency) || 12 %% frequency == 0) {
      # When read from an xlsx file the tbl may contain  POSIXt values.
      # POSIXt values are possible periods if the frequency is a divisor of 12.
      is_posixt <- sapply(data, FUN =  function(x) {inherits(x[[1]], "POSIXt")})
      if (any(is_posixt)) {
        is_period[is_posixt] <- TRUE
      }
    }
    return(is_period)

  } else {

    # csv file
   return(is_period_fun(data, frequency, period_fun))
  }
}

# Returns the periods as either a character or Date vector from data
# read by read_ts_xlsx or read_ts_csv.
get_periods_data <- function(data, frequency, xlsx, period_fun) {

  if (xlsx) {

    #printobj(data)

    is_text   <- sapply(data, FUN = is.character)
    is_num    <- sapply(data, FUN = is.numeric)

    # first handle numerical and character data
    result <- rep(NA, length(data))

    if (any(is_text)) {
      period_texts <- unlist(data[is_text], use.names = FALSE)
      if (!missing(period_fun)) {
        period_texts <- sapply(period_texts, FUN = period_fun,
                               USE.NAMES = FALSE)
      }
      #printobj(period_texts)
      result[is_text] <- period_texts
    }

    if (any(is_num)) {
        num_values <- unlist(data[is_num], use.names = FALSE)
        if (!is.na(frequency)) {
          period_texts <- paste0(num_values, "/1")
        } else {
          period_texts <- as.character(num_values)
        }
        result[is_num] <- period_texts
    }

    if (is.na(frequency) || 12 %% frequency == 0) {

      # when read from xlsx the tbl may contain POSIXt data
      is_posixt <- sapply(data, FUN = function(x) {inherits(x[[1]], "POSIXt")})
      if (any(is_posixt)) {

        # first convert all texts to Date objects
        is_text <- is_text | is_num # all numerical values have already been
                                    # converted
        result_date <- rep(as.Date(NA), length(result))
        result_date[is_text] <- sapply(result[is_text], FUN = function(x) {
                                  as.Date(as.period(x, frequency))})

        # create a vector of POSIXT objects (unlist(data[is_posixt] does not
        # give correct results):
        posixt_vector <- do.call(c, data[is_posixt])
        result_date[is_posixt] <- as.Date(posixt_vector)
        return(result_date)
      }
    }

    return(result)

  } else {

    # csv file, all data are already character vectors
    periods <- data
    if (!missing(period_fun)) {
      periods <- sapply(periods, FUN = period_fun, USE.NAMES = FALSE)
    }
    return(periods)

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
      is_period <- is_period_data(tbl[[col_nr]], frequency, xlsx, period_fun)
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
      is_period_row <- is_period_data(get_row_tbl(tbl, row_nr, xlsx), frequency,
                                      xlsx, period_fun)
      if (any(is_period_row)) {
        col_nr <- Position(function(x) {x}, is_period_row)
        if (missing(rowwise)) {
          if (row_nr == first_row_nr) {
            rowwise <- TRUE
          } else {
            is_period_col <- is_period_data(tbl[[col_nr]], frequency, xlsx,
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

    periods <- get_periods_data(get_row_tbl(tbl, row_nr, xlsx)[is_period],
                                frequency, xlsx, period_fun)

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

  #
  # This function tries to determine if the timeseries are stored rowwise
  # or columnwise.
  #

  if (col_nr == 1) {
    return(FALSE)
  }

  is_period_sum_row <- sum(is_period_row)
  is_period_sum_col <- sum(is_period_col)

  if (is_period_sum_row == 1 && is_period_sum_col == 1) {
    # There is a single period. Count the number of non-NA numerical
    # values in the row to the right of the period, and in the cells below this
    # period.

    # create a list (xlsx) or character vector (csv) for the data next to /
    # below the period
    row_data <- lapply(tbl[row_nr, (col_nr + 1):ncol(tbl)],
                       FUN = function(x) x[[1]])
    if (!xlsx) row <- unlist(period_data, use.names = FALSE)
    col_data <- tbl[(row_nr + 1) : nrow(tbl), col_nr][[1]]

    if (xlsx) {
      f <- function(x) {is.numeric(x) && !is.na(x)}
      row_data_is_num <- sapply(row_data, FUN = f)
      col_data_is_num <- sapply(col_data, FUN = f)
      return(sum(row_data_is_num) <= sum(col_data_is_num))
    } else {
      # TODO: For csv files, we should take the decimal separator into account,
      # so the code is slightly more complicated than the code below
      # suppressWarnings({
      #  row_data_is_num <- !is.na(as.numeric(row_data))
      #  col_data_is_num <- !is.na(as.numeric(col_data))
      # })
    }


  } else if (is_period_sum_row == 1 &&
             col_nr == get_last_non_empty_column(tbl)) {
    return(TRUE)
  } else if (is_period_sum_col == 1 &&
             row_nr == get_last_non_empty_row(tbl)) {
    return(FALSE)
  }


  # An integer or fractional number (e.g. 2017 or 2017.25) can be either a
  # period or just the numerical data of a timeseries. On the other hand, a
  # text such as "2018q1" is unambigiously a period. We can use this
  # to determine if the timeseries are stored rowwise or columnwise.

  # For xlsx, we can also distinguish if a cell value is stored as
  # numerical or text value. For example, 2017 could be specified as a
  # numerical value 2017 or a string "2017". Here, we assume that all
  # text cells contain a period, while numerical cells may or may not
  # contain data.

  # For csv  files there is no distinction between numerical cells and text
  # cells: are cells contain a character. Therefore, it does not make sense
  # to use the information if the specified frequency is 1. If the specified
  # frequency == 1has been specified for csv files.

  # create a list (xlsx) or character vector (csv) for the data on the period
  # row

  row_periods <- lapply(tbl[row_nr, is_period_row], FUN = function(x) x[[1]])
  if (!xlsx) row_periods <- unlist(row_periods, use.names = FALSE)
  col_periods <- tbl[is_period_col, col_nr][[1]]

  is_num_period <- function(period_data) {
    if (xlsx) {
      return(sapply(period_data, FUN = is.numeric))
    } else {
      # TODO: what to do with fractionals (e.g. 2017.25)?
      # this approach is not yet correct.
      return(grepl("^-?\\d+$", period_data))
    }
  }

  is_num_period_row <- is_num_period(row_periods)
  is_num_period_col <- is_num_period(col_periods)

  if (is_period_sum_col > 1 && all(is_num_period_col[-1] &&
                                   any(!is_num_period_row))) {
    return(TRUE)
  } else if (is_period_sum_row > 1 && all(is_num_period_row[-1] &&
                                          any(!is_num_period_col))) {
    return(FALSE)
  }

  # TODO: if all periods are numeric, then check the min and max value.
  # If on the first row the number are between 1800 and 2300, then that
  # is probably a normal series. If that test fails we could check for periods
  # before 2023.

  # Fall back option: if every other test failed, use the total number of
  # rowwise/colwise periods.
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

get_last_non_empty_row <- function(tbl) {
  for (row_nr in nrow(tbl):1) {
    if (any(!is.na(unlist(tbl[row_nr, ])))) {
      return(row_nr)
    }
  }
  return(NA)
}

get_row_tbl <- function(tbl, row_nr, xlsx) {
  # returns a specific row of a tibble, as a list for xlsx and as a
  # character vector for csv.
  data <- lapply(tbl[row_nr, ], FUN = function(x) x[[1]])
  if (!xlsx) data <- unlist(data, use.names = FALSE)
  return(data)
}

get_first_non_empty_column <- function(tbl) {
  for (col_nr in 1:ncol(tbl)) {
    if (any(!is.na(tbl[[col_nr]]))) {
      return(col_nr)
    }
  }
  return(NA)
}

get_last_non_empty_column <- function(tbl) {
  for (col_nr in ncol(tbl):1) {
    if (any(!is.na(unlist(tbl[[col_nr]])))) {
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

# is_period_data: do elements in data contain a period?
# INPUT:
#   data  a list (xlsx) or character vector (csv)
# RETURN: a logical vector
is_period_data <- function(data, frequency, xlsx, period_fun) {

  is_period_fun <- function(periods, frequency, period_fun) {
    # Returns a logical vector with elements TRUE or FALSE if the corresponding
    # element in character vector period_texts is a valid period (for the
    # specified frequency).
    if (!missing(period_fun)) {
      periods <- apply_period_fun(periods, period_fun)
      if (!is.character(periods)) {
        # period_texts is a period vector or a list of period objects
        # with the different frequencies -> every element in data is a possible
        # period if the period is not NA.
        return(!is.na(periods))
      }
    }

    # call C++ function is_period_text
    return(is_period_text(periods, frequency))
  }

  if (xlsx) {

    is_period <- rep(FALSE, length(data))

    is_text <- sapply(data, FUN = is.character)
    if (any(is_text)) {
      period_texts <- unlist(data[is_text], use.names = FALSE)
      is_period[is_text] <- is_period_fun(period_texts, frequency, period_fun)
    }

    if ((is.na(frequency) || frequency == 1)) {
      # integer numerical values are possible periods for frequency 1
      is_num  <- sapply(data, FUN = is.numeric)
      if (any(is_num)) {
        num_values <- unlist(data[is_num], use.names = FALSE)
        is_period[is_num] <- num_values %% 1 == 0
      }
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

    # csv file, data is a character vector
    return(is_period_fun(data, frequency, period_fun))
  }
}

# get_periods_data: converts a list (xlsx) or character vector (csv) to
# a period vector or a period list.
# INPUT:
#   data:  a list (xlsx) or character vector (csv). Each element should
#          be an object which can be converted to a period.
# RETURN: a period vector if all periods in data have the same frequency,
#         otherwise a period list.
get_periods_data <- function(data, frequency, xlsx, period_fun) {

  if (xlsx) {

    is_text <- sapply(data, FUN = is.character)
    is_num  <- sapply(data, FUN = is.numeric)

    freq_text <- NA
    freq_num <- NA
    freq_posixt <- NA
    multi_freq_text <- FALSE

    text_present <- any(is_text)
    if (is.na(frequency) || frequency == 1) {
      # integer numerical values are possible periods if the frequency 1
      num_present <- any(is_num)
    } else {
      num_present <- FALSE
    }
    if (is.na(frequency) || 12 %% frequency == 0) {
      # POSIXt values are possible periods if the frequency is a divisor of 12.
      is_posixt <- sapply(data, FUN = function(x) {inherits(x[[1]], "POSIXt")})
      posixt_present <- any(is_posixt)
    } else {
      posixt_present <- FALSE
    }

    if (text_present) {
      text_values <- unlist(data[is_text], use.names = FALSE)
      if (!missing(period_fun)) {
        text_periods <- apply_period_fun(text_values, period_fun)
        if (is.character(text_periods)) {
          # Convert texts to periods with C++ function parse_period:
          text_periods <- parse_period(text_periods, frequency = frequency)
        }
      } else {
        text_periods <- parse_period(text_values, frequency = frequency)
      }
      if (all(is_text)) return(text_periods)
      multi_freq_text <- is.list(text_periods)
      if (!multi_freq_text) freq_text <- frequency(text_periods)
    }

    if (num_present) {
      num_values <- unlist(data[is_num], use.names = FALSE)
      num_periods <- period(num_values, frequency = frequency)
      if (all(is_num)) return(num_periods)
      # numerical periods always have frequency 1
      freq_num <- 1
    }

    if (posixt_present) {
      # When read from xlsx data may contain POSIXt values
      # unlist does not work here. Therefore use function c (concatenation):
      date_vector <- do.call(c, data[is_posixt])
      posixt_periods <- period(date_vector, frequency = frequency)
      if (all(is_posixt)) return(posixt_periods)
      freq_posixt <- frequency(posixt_periods)
    }

    if (is.na(frequency)) {
      # check if the frequencies are identical
      if (multi_freq_text) {
        all_freqs_equal <- FALSE
      } else {
        all_freqs <- c(freq_text, freq_num, freq_posixt)
        all_freqs <- unique(all_freqs[!is.na(all_freqs)])
        all_freqs_equal <- length(all_freqs) == 1
        freq_result <- all_freqs
      }
    } else {
      all_freqs_equal <- TRUE
      freq_result <- frequency
    }


    if (all_freqs_equal) {
      # all frequencies are equal: return a period vector
      result <- period(rep(NA, length(data)), frequency = freq_result)
      if (text_present) result[is_text] <- text_periods
      if (num_present) result[is_num] <- num_periods
      if (posixt_present) result[is_posixt] <- posixt_periods
    } else  {
      # different frequencies: return a list of period objects
      result <- list(rep(NULL, length(data)))
      if (text_present) result[is_text] <- as.list(text_periods)
      if (num_present) result[is_num] <- as.list(num_periods)
      if (posixt_present) result[is_posixt] <- as.list(posixt_periods)
    }

    return(result)

  } else {

    # csv file, all data are already character vectors
    periods <- data
    if (!missing(period_fun)) {
      periods <- apply_period_fun(periods, period_fun)
      if (is.character(periods)) {
        periods <- parse_period(periods, frequency = frequency)
      }
    } else {
      periods <- parse_period(periods, frequency = frequency)
    }
    return(periods)
  }
}

apply_period_fun <- function(texts, period_fun) {

  if (length(texts) == 0) return(character(0))

  periods <- period_fun(texts)

  if (length(periods) != length(texts)) {
    stop(paste("Function period_fun should return an object with the same",
               "length as its input value."))
  }

  if (is.character(periods) || is.period(periods)) {
    return(periods)
  } else if (is_period_list(periods)) {
    # the return value if a list of period objects. If they have the same
    # frequency, then convert to vector, otherwise return as list.
    return(simplify_plist(periods))
  }

  stop("Period_fun should return a character or period vector.")
}

# Internal function inspect_tibble: finds the first row or column of the periods
# of the tibble read by read_ts_xlsx or read_ts_csv. The function also determines
# which column of the tibble contains timeseries data. Returns NULL if no
# period has been found.
#
# The function does not assume that all rows of the csv file or Excel sheet
# have been read. read_ts_xlsx first reads the first 25 rows, then calls
# inspect_tibble to determine the position of the period data and
# the numerical data columns. The data are actually read in a second stage
# using this information. Currently, read_ts_csv reads all rows in one go,
# but inspect_tibble does not use that information.
#
# For rowwise timeseries, the function returns the periods and for columnwise
# timeseries the names and labels.  The function does not return names and labels
# for rowwise timeseries, because the input tibble may not contain all rows
# (see the discussion above). For the same reason, the function does not return
# the periods for columnwise timeseries.
inspect_tibble <- function(tbl, frequency, rowwise, labels, xlsx, period_fun,
                           name_fun) {

  first_row_nr <- get_first_non_empty_row(tbl)
  if (is.na(first_row_nr)) return(NULL)

  found <- FALSE
  rowwise_specified <- !missing(rowwise)
  rowwise_guessed <- FALSE

  if (rowwise_specified && !rowwise) {
    # columnwise

    for (col_nr in 1:ncol(tbl)) {
      is_period_col <- is_period_data(tbl[[col_nr]], frequency, xlsx,
                                      period_fun)
      if (!any(is_period_col)) next
      last_row <- Position(identity, is_period_col, right = TRUE)
      if (last_row > first_row_nr) {
        # If last_row == first_row_nr, there is a single period in the first
        # non-empty row. Since there is no room for possible variable names,
        # we have probably not found the real period column yet.
        found <- TRUE
        break
      }
    }

    if (found) row_nr <- Position(identity, is_period_col)

  }  else {

    # rowwise or columnwise

    # first search for a period rowwise
    for (row_nr in first_row_nr:nrow(tbl)) {
      row_data <- get_row_tbl(tbl, row_nr, xlsx)
      is_period_row <- is_period_data(row_data, frequency, xlsx, period_fun)
      if (!any(is_period_row)) next
      col_nr <- Position(identity, is_period_row)
      if (!rowwise_specified) {
        is_period_row_sum <- sum(is_period_row)
        if (row_nr == first_row_nr) {
          # If a period occurs in the first non-empty row, then we assume
          # for the time being rowwise.
          rowwise <- TRUE
        } else if (col_nr == 1) {
          # If a period occurs in the first column, then assume columnwise.
          rowwise <- FALSE
          is_period_col <- is_period_data(tbl[[col_nr]], frequency, xlsx,
                                          period_fun)
        } else {
          is_period_col <- is_period_data(tbl[[col_nr]], frequency, xlsx,
                                          period_fun)
          rowwise <- is_rowwise(row_nr, col_nr, is_period_row, is_period_col,
                                tbl, frequency, xlsx)
          if (!is.na(rowwise) && rowwise && col_nr == 1 &&
              is_period_row_sum > 1) {
            # If the first rowwise period occurs in the first column, then
            # that period is ignored, because that column should contain
            # variable names (see code below). Therefore check again if the
            # timeseries could be stored columnwise for the next period.
            is_period_row[1] <- FALSE
            col_nr <- Position(identity, is_period_row)
            is_period_col <- is_period_data(tbl[[col_nr]], frequency, xlsx,
                                            period_fun)
            rowwise <- is_rowwise(row_nr, col_nr, is_period_row, is_period_col,
                                  tbl, frequency, xlsx)
          }

          if (is.na(rowwise)) {
            # Fall back case" if every other test failed, use the total number of
            # rowwise / colwise periods.
            rowwise <- sum(is_period_row) >= sum(is_period_col)
            rowwise_guessed <- TRUE
          } else {
            rowwise_guessed <- FALSE
          }
        }
      }

      if (rowwise) {
        last_col <- Position(identity, is_period_row, right = TRUE)
        if (last_col > 1) {
          # If last_col == 1, there is a single period in the first column.
          # Since there is no room for possible variable names,
          # we have probably not found the real period row yet.
          found <- TRUE
          break
        }
      } else {
        last_row <- Position(identity, is_period_col, right = TRUE)
        if (last_row > 1) {
          found <- TRUE
          break
        }
      }
    }
  }

  if (!found) return(NULL)

  if (rowwise && col_nr == 1) {
    # period in first colum should be ignored, since this column should
    # contain variable names. Skip to the next column.
    is_period_row[1] <- FALSE
    col_nr <- Position(identity, is_period_row)
    if (is.na(col_nr)) return(NULL)
  }

  if (!rowwise && row_nr == first_row_nr) {
    # if the first period row is the first non-empty row, then
    # ignore that period, since the row should contain variable names
    is_period_col[first_row_nr] <- FALSE
    row_nr <- Position(identity, is_period_col)
    if (is.na(row_nr)) return(NULL)
  }


  if (rowwise_guessed) {
    text <- if (rowwise) "rowwise" else  "columnwise"
    warning(paste("Could not determine if timeseries are stored rowwise",
                  "or columnwise.\nAssuming" , text, "based on the",
                  "number of periods found.\nUse argument rowwise if",
                  "necessary."))
  }



  if (rowwise) {

    first_data_col <- col_nr

    last_data_col <- Position(identity, is_period_row, right = TRUE)

    period_data <- get_row_tbl(tbl[row_nr, is_period_row], 1, xlsx = xlsx)
    periods <- get_periods_data(period_data, frequency, xlsx, period_fun)

    is_data_col <- is_period_row
    is_data_col[1:(first_data_col -1)] <- FALSE

    return(list(rowwise = TRUE, period_row = row_nr,
                first_data_col = first_data_col, last_data_col = last_data_col,
                is_data_col = is_data_col, periods = periods))

  } else {
    # columnwise timeseries

    first_data_row <- row_nr
    period_col <- col_nr
    name_info <- get_name_info_colwise(first_data_row, first_row_nr,
                                       period_col, tbl, labels, name_fun, xlsx)

    last_data_col <- Position(identity, name_info$col_has_name, right = TRUE)

    is_data_row <- is_period_col
    is_data_row[1:(first_data_row - 1)] <- FALSE

    return(list(rowwise = FALSE, period_col = period_col,
                first_data_row = first_data_row, last_data_col = last_data_col,
                is_data_col = name_info$col_has_name,
                is_data_row = is_data_row, names = name_info$names,
                lbls = name_info$lbls))
  }
}

is_rowwise <- function(row_nr, col_nr, is_period_row, is_period_col,
                       tbl, frequency, xlsx) {

  #
  # This function tries to determine if the timeseries are stored rowwise
  # or columnwise. This function makes the assumption that the cell
  # (row_nr, col_nr) is definitively a period.
  #

  # possible periods above the current row should be ignored.
  is_period_col[1:(row_nr - 1)] <- FALSE

  is_period_row_sum <- sum(is_period_row)
  is_period_col_sum <- sum(is_period_col)

  #
  #  SINGLE PERIOD
  #

  if (is_period_row_sum == 1 && is_period_col_sum == 1) {
    # Single period. Count the number of numerical values in the cells to the
    # right of and below this period cell. In case of doubt assume rowwise.

    row_data <- lapply(tbl[row_nr, (col_nr + 1):ncol(tbl)],
                       FUN = function(x) x[[1]])
    col_data <- tbl[(row_nr + 1) : nrow(tbl), col_nr][[1]]

    if (xlsx) {
      row_data_n_num <- sum(sapply(row_data, FUN = is.numeric))
      col_data_n_num <- sum(sapply(col_data, FUN = is.numeric))
    } else {
      # For csv files, we should take the decimal separator into account
      # to check if the data are numerical.
      # For the time being, just compare the number of any non-NA values.
      row_data_n_num <- sum(!is.na(row_data))
      col_data_n_num <- sum(!is.na(col_data))
    }
    if (row_data_n_num != col_data_n_num) {
      return(row_data_n_num < col_data_n_num)
    } else {
      p_txt <- as.character(tbl[row_nr, col_nr][[1]])
      warning(sprintf(paste0("Could not determine if timeseries are stored",
                             " rowwise or columnwise.\nFound a single period ",
                             "%s. Assuming rowwise."), p_txt))
      return(TRUE)
    }

    return(sum(row_data_n_num) <= sum(col_data_n_num))
  }

  #
  # FREQUENCY > 1
  #

  if (!is.na(frequency) && frequency > 1 &&
      (is_period_row_sum == 1 || is_period_col_sum == 1)) {
    # If the frequency is larger than 1, there is no confusion between periods
    # and integer numerical data values. Therefore, if all periods are in a
    # single row then the timeseries are probably stored rowwise.
    # Something similar holds when all periods are in a single column.
    #
    # This is not the case if the frequency is 1. For example, for the row
    #      1   2   3
    # it is possible that all values in the row a periods. However, it could
    # be possible that the data are stored columnwise, with one period (1) and
    # two numerical values: 2 and 3 are then just data values.

    return(is_period_col_sum == 1)
  }

  #
  # FREQUENCY UNKNOWN or 1
  #

  # An integer number (e.g. 2017) can be either a period or just the numerical
  # data of a timeseries. On the other hand, a text such as "2018q1" is
  # unambigiously a period. We can use this to determine if the timeseries are
  # stored rowwise or columnwise.

  # For xlsx, we can also distinguish if a cell value is stored as
  # numerical or text value. For example, 2017 could be specified as a
  # numerical value 2017 or a string "2017". Here, we assume that all
  # text cells contain a period, while numerical cells may or may not
  # be a period.

  # For csv files there is no distinction between numerical cells and text
  # cells: are cells contain a text. However, even forfrequency 1 we
  # a text as "2010y" is definitively a text string.

  if (is.na(frequency) || frequency == 1) {

    # First step: create a list (xlsx) or character vector (csv) for the data
    # on the period row

    row_periods <- lapply(tbl[row_nr, is_period_row], FUN = function(x) x[[1]])
    col_periods <- tbl[is_period_col, col_nr][[1]]

    is_num_period <- function(period_data) {
      if (xlsx) {
        return(sapply(period_data, FUN = is.numeric))
      } else {
        return(grepl("^-?\\d+$", period_data))
      }
    }

    is_num_period_row <- is_num_period(row_periods)
    is_num_period_col <- is_num_period(col_periods)

    if (!any(is_num_period_row) && all(is_num_period_col[-1])) {

      return(TRUE)

    } else if (!any(is_num_period_col) && all(is_num_period_row[-1])) {

      return(FALSE)

    } else if (all(is_num_period_row) && all(is_num_period_col)) {
      # All periods are numeric.

      if (is_period_col_sum == 1 && row_nr == get_last_non_empty_row(tbl)) {
        # we have a row of integers, e.g. 2011 2012 2013 2015) {
        # If all periods are in a single row and if there are only empty
        # rows below that row, then the timeseries are probably stored
        # columnwise (where only the first period in the row is actually
        # a period, while the other "periods" are numerical data).
        return(FALSE)
      } else if (is_period_row_sum == 1 &&
                 col_nr == get_last_non_empty_column(tbl)) {
        # see the comment above in the code block for is_period_row_sum > 1
        return(TRUE)
      }

      # Check if numerical values are likely years. An integer value
      # 13424566 or -1345 is probably not a year.

      num_per_row <- as.numeric(row_periods)
      num_per_col <- as.numeric(col_periods)

      num_is_year <- function(x, format) {
        # this function returns true if a numerical value is likely a year.
        # for format = "YYYY" we assume for digits, otherwise two digits
        if (format == "YYYY") {
          return(x >= 1800 & x <= 3000)
        } else {
          return(x >= 0 & x < 99)
        }
      }

      is_rowwise_year <- function(format = "YYYY") {
        num_is_year_row <- num_is_year(num_per_row, format)
        num_is_year_col <- num_is_year(num_per_col, format)
        if (all(num_is_year_row) &&
            (is_period_col_sum == 1 || !all(num_is_year_col[-1]))) {
          return(TRUE)
        } else if (all(num_is_year_col) &&
                   (is_period_row_sum == 1 || !all(num_is_year_row[-1]))) {
          return(FALSE)
        } else {
          return(NA)
        }
      }

      # first try year format YYYY, if that test fails try YY
      is_rowwise <- is_rowwise_year()
      if (is.na(is_rowwise)) is_rowwise <- is_rowwise_year(format = "YY")
      if (!is.na(is_rowwise)) return(is_rowwise)
    }
  }

  # we have been unable to determine is the timeseries are stored rowwise
  return(NA)
}



get_first_non_empty_row <- function(tbl) {
  for (row_nr in 1:nrow(tbl)) {
    if (any(!is.na(tbl[row_nr, ]))) {
      return(row_nr)
    }
  }
  return(NA)
}

get_last_non_empty_row <- function(tbl) {
  for (row_nr in nrow(tbl):1) {
    if (any(!is.na(tbl[row_nr, ]))) {
      return(row_nr)
    }
  }
  return(NA)
}

get_last_non_empty_column <- function(tbl) {
  for (col_nr in ncol(tbl):1) {
    if (any(!is.na(tbl[[col_nr]]))) {
      return(col_nr)
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




# get_name_info_rowwise: internal function to get names and labels from
# rowwise timeseries.
# INPUT:
# layout           : the result of function inspect_tibble
# data_tbl         : the tibble containing all rows below the period row
# labels           : label option ("before", "after" or "no")
# name_fun         : function applied to the names
# RETURN: a list with names, labels, and  a logical vector which indicates
#         which rows in data_tbl contain a name.

get_name_info_rowwise <- function(layout, data_tbl, labels, name_fun) {
  # get the names and labels for a rowwise tibble containing timeseries data.

  if (layout$first_data_col > 1) {
    pre_data_cols <- 1 : (layout$first_data_col - 1)
    pre_data_tbl <- data_tbl[ , pre_data_cols]
    empty_cols <- apply(is.na(pre_data_tbl), MARGIN = 2, FUN = all)
    name_label_cols <- which(!empty_cols)
  } else {
    name_label_cols <- numeric(0)
  }

  if (length(name_label_cols) == 0) {
    return(list(row_has_name = rep(FALSE, nrow(data_tbl)), names = character(0),
                lbls = NULL))
  }

  n_label_cols <- length(name_label_cols) - 1

  # no labels if there is only one non-empty column
  if (n_label_cols == 0) labels = "no"

  name_col <- if (labels == "before") {
    name_label_cols[n_label_cols + 1]
  } else {
    name_label_cols[1]
  }

  if (labels == "before") {
    label_cols <- name_label_cols[1 : n_label_cols]
  } else if (labels == "after") {
    label_cols <- name_label_cols[2 : (n_label_cols + 1)]
  } else {
    label_cols <- numeric(0)
  }

  name_col_data <- data_tbl[[name_col]]
  row_has_name <- !is.na(name_col_data) & nzchar(name_col_data)

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


# get_name_info_colwise: internal function to get names and labels from
# columnwise timeseries.
# INPUT:
# first_data_row   : first row in tbl with a period and data
# first_row_nr     : first non-empty row
# period_col       : column number of the periods
# tbl              : tibble with data of the sheet
# labels           : label option ("before", "after" or "no")
# name_fun         : function applied to the names
# xlsx             : TRUE if these function is used by read_ts_xlsx.
# RETURN: a list with names, labels, and  a logical vector which indicates
#         which columns in tbl contain a name.
get_name_info_colwise <- function(first_data_row, first_row_nr, period_col, tbl,
                                  labels, name_fun, xlsx) {

  if (first_row_nr < first_data_row && period_col < ncol(tbl)) {
    pre_data_rows <- first_row_nr : (first_data_row - first_row_nr)
    pre_data_tbl <- tbl[pre_data_rows, (period_col + 1) : ncol(tbl)]
    empty_rows <- apply(is.na(pre_data_tbl), MARGIN = 1, FUN = all)
    name_label_rows <- which(!empty_rows) + (first_row_nr - 1)
  } else {
    name_label_rows <- numeric(0)
  }

  if (length(name_label_rows) == 0) {
    return(list(col_has_name = rep(FALSE, ncol(tbl)), names = character(0),
                lbls = NULL))
  }

  n_label_rows <- length(name_label_rows) - 1

  # no labels if there is only one non-empty column
  if (n_label_rows == 0) labels = "no"

  name_row <- if (labels == "before") {
    name_label_rows[n_label_rows + 1]
  } else {
    name_label_rows[1]
  }

  if (labels == "before") {
    label_rows <- name_label_rows[1 : n_label_rows]
  } else if (labels == "after") {
    label_rows <- name_label_rows[2 : (n_label_rows + 1)]
  } else {
    label_rows <- numeric(0)
  }

  name_row_data <- get_row_tbl(tbl, name_row, xlsx)
  name_row_data[is.na(name_row_data)] <- ""
  if (xlsx) name_row_data <- as.character(name_row_data)

  col_has_name <- nzchar(name_row_data)
  col_has_name[1 : period_col] <- FALSE

  if (!any(col_has_name)) {
    names <- character(0)
    lbls <- NULL
  } else {

    names <- name_row_data[col_has_name]
    if (!missing(name_fun)) names <- name_fun(names)

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
    label_tbl <- as_tibble(t(label_tbl), .name_repair = "minimal")
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

test_duplicates <- function(rts){
  # test duplicate names in xlsx or csv
  dupl <- duplicated(colnames(rts))
  if (any(dupl)){
    warning(sprintf("Duplicate names in file: %s\n",
                    paste(colnames(rts)[dupl], collapse = ", ")))
  }
  return(invisible(NULL))
}

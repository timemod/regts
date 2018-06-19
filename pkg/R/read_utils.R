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
is_period_tbl <- function(tbl, frequency, xlsx) {
  is_per_text <- is_period_text(tibble_2_char(tbl), frequency)
  if (xlsx && (!is.na(frequency) && 12 %% frequency == 0)) {
    # When read from an xlsx file the tbl may contain  POSIXt values.
    # POSIXt values are possible periods if the frequency is a divisor of 12.
    is_posixt <- sapply(tbl, FUN = function(x) {inherits(x[[1]], "POSIXt")})
    return(is_per_text | is_posixt)
  } else {
    # csv
    return(is_per_text)
  }
}

# Returns the periods as either a character or Date vector
get_periods_tbl <- function(tbl, frequency, xlsx) {
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
    conv_date <- function(x) {
      x <- x[[1]]
      if (inherits(x, "POSIXt")) {
        return(as.Date(x))
      } else {
        # convert string to period object and then to POSIXct
        return(as.Date(period(x, frequency)))
      }
    }
    date_list <- lapply(tbl, FUN = conv_date)
    # convert to vector of Dates (sapply does not work)
    return(do.call(c, date_list))
  } else {
    # return a character vector
    return(tibble_2_char(tbl, FALSE))
  }
}


# internal function: find the first row or column containing a period in the
# tibble read by read_excel. Returns NULL if no period has been found
find_periods <- function(tbl, frequency, rowwise, xlsx) {

  found <- FALSE

  if (!missing(rowwise) && !rowwise) {
    # columnwise

    for (col_nr in 1:ncol(tbl)) {
      is_period <- is_period_tbl(tbl[[col_nr]], frequency, xlsx)
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

    # first search for a period rowwise
    for (row_nr in 1:nrow(tbl)) {
      is_period_row <- is_period_tbl(tbl[row_nr, ], frequency, xlsx)
      if (any(is_period_row)) {
        col_nr <- Position(function(x) {x}, is_period_row)
        if (missing(rowwise)) {
          if (row_nr == 1) {
            rowwise <- TRUE
          } else {
            is_period_col <- is_period_tbl(tbl[[col_nr]], frequency, xlsx)
            rowwise <- col_nr != 1  && sum(is_period_row) > sum(is_period_col)
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

  if (found) {
    return(list(rowwise = rowwise, row_nr = row_nr, col_nr = col_nr,
                is_period = is_period))
  } else {
    return(NULL)
  }
}

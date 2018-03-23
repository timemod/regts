
# converts a tibble to a character vector.
tibble_2_char <- function(tbl, replace_na = TRUE) {
  ret <- unlist(tbl, use.names = FALSE)
  ret <- as.character(ret)
  if (replace_na) {
    ret[is.na(ret)] <- ""
  }
  return(ret)
}


# internal function: find the first row containing a period in the
# tibble read by read_excel. Returns NULL if no period has been found
find_periods <- function(tbl, frequency, rowwise) {

  found <- FALSE

  if (!missing(rowwise) && !rowwise) {
    # columnwise

    for (col_nr in 1:ncol(tbl)) {
      is_period <- is_period_text(tibble_2_char(tbl[[col_nr]]), frequency)
      if (any(is_period)) {
        found <- TRUE
        break
      }
    }

    if (found) row_nr <- Position(function(x) {x}, is_period)

  }  else {
    # rowwise or columnwise

    # first search for a period rowwise
    for (row_nr in 1:nrow(tbl)) {
      is_period_row <- is_period_text(tibble_2_char(tbl[row_nr, ]), frequency)
      if (any(is_period_row)) {
        found <- TRUE
        break
      }
    }

    if (found) {

      col_nr <- Position(function(x) {x}, is_period_row)

      if (missing(rowwise)) {
        if (row_nr == 1) {
          rowwise <- TRUE
        } else {
          is_period_col <- is_period_text(tibble_2_char(tbl[[col_nr]]))
          rowwise <- col_nr != 1  && sum(is_period_row) > sum(is_period_col)
        }
      }

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

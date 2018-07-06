# numeric_matrix is an internal function to convert a data.frame to a numeric
# matrix.
#
# It returns the matrix obtained by converting all the variables in a data frame
# to numeric mode and then binding them together as the columns of a matrix.
# Factors, Dates and other non-numerical variables are converted to characters.
#
# This function is similar to the function data.matrix of the base package,
# except that:
# * it is possible to specify a decimal separator used to convert strings to
#   numeric values
# * factors are first converted to characters and then to numeric values
# * if the data frame contains strings that cannot be converted
#   to numerical values, then numeric_matrix gives a warning about
#   the first 10 character strings that could not be succesfully converted.
#
# INPUT
#   x    a data frame
#   dec  decimal separator

numeric_matrix <- function(x, dec = ".") {

  if (nrow(x) == 0 || ncol(x) == 0) {
    # no data available
    return(matrix(0.0, nrow = nrow(x), ncol = ncol(x)))
  }

  x <- as.data.frame(x)

  # Convert factors, Dates etc. to characters. Also replace
  # decimals separator with ".".
  convert_col <- function(x) {
    if (is.numeric(x) | is.logical(x)) {
      return(x)
    } else {
      x <- as.character(x)
      x <- ifelse(trimws(x) == "", NA_character_, x)
      if (dec != ".") {
        return(sub(dec, ".", x, fixed = TRUE))
      } else {
        return(x)
      }
    }
  }
  x_converted <- as.data.frame(lapply(x, FUN = convert_col),
                               stringsAsFactors = FALSE)

  num_mat <- suppressWarnings(data.matrix(x_converted))

  error_sel <- is.na(num_mat) & !is.na(x_converted)

  if (any(error_sel)) {
    weird_texts <- unique(x[error_sel])
    nweird <- length(weird_texts)
    NWEIRD_MAX <- 10
    nmax <- min(NWEIRD_MAX, nweird)
    weird_texts <- paste0("\"", weird_texts[1:nmax], "\"")

    if (nweird <= NWEIRD_MAX) {
      warning(paste0("NAs introduced by coercion\n",
                     "The following texts could not be converted to numeric:\n",
                     paste0(weird_texts, collapse = "\n")))
    } else {
      warning(paste0("NAs introduced by coercion.\n",
                     nweird, " texts could not be converted to numeric.\n",
                     "The first ", NWEIRD_MAX, " texts that gave problems are:\n",
                     paste0(weird_texts, collapse = "\n")))
    }
  }

  return(num_mat)
}

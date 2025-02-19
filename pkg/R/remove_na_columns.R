#' Function for removing columns with NAs in a (multivariate) regts
#'
#' This function removes columns with NA values from a (multivariate)
#' \code{\link{regts}}.
#' A column will by default be regarded as NA if all elements
#' in the column are NA.  Specify argument \code{is.na = "any"} to change
#' this behaviour. If all columns are removed the function returns \code{NULL}.
#'
#' @param x a regts object
#' @param is_na character with values \code{"all"} or \code{"any"}.
#' If \code{"all"} (default) then a column will be regarded as \code{NA} only
#' if all elements in the column are \code{NA}. If \code{"any"} then a column
#' will be regarded as \code{NA} if it has any \code{NA}s.

#' @return A \code{regts} object in which NA columns have been removed.
#'
#' @examples
#' # remove columns with all NAs
#' data <- matrix(c(1,3, 5, NA, NA, NA,3,7,9), ncol = 3)
#' rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
#' remove_na_columns(rts)
#'
#' data <- matrix(c(NA,3,NA,NA,5,6,NA,7,9), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' remove_na_columns(rts, is_na = "any")


#' @export
remove_na_columns <- function(x, is_na = c("all", "any")) {

  # Use function inherits instead of is.ts to check if x1 is a timeseries.
  # is.ts returns FALSE if x1 is a timeseries with 0 columns
  if (!inherits(x, "ts")) {
    stop("Argument x is not a (multivariate) timeseries")
  }
  isna <- match.arg(is_na)

  if (NCOL(x) == 0) return(x)

  if (!is.matrix(x)) {
    elem <- which(!is.na(x))
    if (isna == "all" && length(elem) == 0) {
      return(NULL)
    } else if (length(elem) < length(x)) {
      return(NULL)
    } else {
      return(x)
    }

  } else {
    if (isna == "all") {
      elem <-   which(!apply(is.na(x), 2, all))

    } else {
      elem <-   which(!apply(is.na(x), 2, any))
    }
  }
  if (length(elem) == 0) {
    return(NULL)
  }

  return(x[, elem, drop = FALSE])

}

#' Function for removing leading and trailing NAs
#'
#' This function removes leading or trailing NAs or both from a (multivariate)
#' regts object.
#' For multivariate regts a row will by default be regarded as NA if all
#' elements in the row are NA. Use argument \code{is.na = "any"} to change this
#' behaviour. The function returns \code{NULL} if all values are \code{NA}.
#'
#' @param x a \code{\link{regts}} object
#' @param method character string with values \code{"both",} \code{"first"} or
#' \code{"last"} to remove NAs at both ends (by default), just at the start
#' or just at the end.
#' @param is_na character string with values \code{"all"} or \code{"any"}.
#' If \code{"all"} (default) then a row will be regarded as \code{NA} only if
#' all elements in the row are \code{NA}. If \code{"any"} then a row will be
#' regarded as \code{NA} if it has any \code{NA}s. For one dimensional regts
#' objects this argument has no effect.
#' @return A \code{\link{regts}} object in which leading and/or trailing NAs
#' have been removed, or \code{NULL} if all values are \code{NA}.
#'
#' @seealso \code{\link{zero_trim}}
#'
#' @examples
#' # remove only leading NAs
#' ts1 <- regts(c(NA,1,3,NA,4,8,NA), start = "2000")
#' na_trim(ts1, method = "first")
#'
#' # remove trailing NAs
#' data <- matrix(c(1,3,NA,2,5,NA,3,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
#' na_trim(rts, method = "last")
#'
#' data <- matrix(c(NA,3,NA,NA,5,6,NA,7,NA), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' # remove leading NAs if all elements in the row are NA
#' na_trim(rts, method = "first")
#' # or remove rows on both sides if any NA occurs in that row
#' na_trim(rts, is_na = "any")

#' @export
na_trim <- function(x, method = c("both", "first", "last"),
                    is_na = c("all", "any")) {

  # Use function inherits instead of is.ts to check if x1 is a timeseries.
  # is.ts returns FALSE if x1 is a timeseries with 0 columns
  if (!inherits(x, "ts")) {
    stop("Argument x is not a timeseries")
  } else if (!is.regts(x)) {
    x <- as.regts(x)
  }

  if (NCOL(x) == 0) return(x)

  side <- match.arg(method)
  isna <- match.arg(is_na)

  fst_not_na <- function(x) {
    ret <- Position(function(x) x, !is.na(x))
    if (is.na(ret)) ret <- length(x) + 1
    return(ret)
  }
  lst_not_na <- function(x) {
    ret <- Position(function(x) x, !is.na(x), right = TRUE)
    if (is.na(ret)) ret <- 0
    return(ret)
  }

  if (!is.matrix(x)) {
    first <- fst_not_na(x)
    last <- lst_not_na(x)
    len <- length(x)

  } else {
    first <- apply(x, 2, fst_not_na)
    last  <- apply(x, 2, lst_not_na)

    if (isna == "any") {
      first <- max(first)
      last <- min(last)
    } else {
      first <- min(first)
      last <- max(last)
    }
    len <- nrow(x)
  }

  if (first > last) {
    return(NULL)
  }

  if (side == "both") {
    sel <- first : last
  } else if (side == "first") {
    sel <- first : len
  } else {
    sel <- 1 : last
  }

  per <- start_period(get_period_range(x)) + sel[1] - 1
  period <- period_range(per, per + length(sel) - 1)

  return(x[period, ])
}

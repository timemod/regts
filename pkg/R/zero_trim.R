#' Function for removing leading and trailing zeros
#'
#' This function removes leading or trailing zeros or both from a (multivariate)
#' regts object.
#' For multivariate regts a row will by default be regarded as 0 if all elements
#' in the row are 0.
#' The function returns \code{NULL} if all values are zero.
#'
#' @param x a regts object
#' @param method character string with values \code{"both",} \code{"first"} or
#' \code{"last"} to remove zeros at both ends (by default), just at the start
#' or just at the end.
#' @return A \code{\link{regts}} object in which leading and/or trailing zeros
#' have been removed, or \code{NULL} if all values in the timeseries are zero.
#'
#' @examples
#' # remove only leading zeros
#' ts1 <- regts(c(0, 1, 3, 0, 4, 8, 0), start = "2000")
#' zero_trim(ts1, method = "first")
#'
#' # remove trailing zeros
#' data <- matrix(c(1, 3, 0,2, 5, 0, 3, 7, 0), ncol = 3)
#' rts <- regts(data, start = "2010Q2", names = c("a", "b", "c"))
#' zero_trim(rts, method = "last")
#'
#' # removing zeros in a multivariate regts
#' data <- matrix(c(0, 3, 0, 0, 5, 6, 0, 7, 0), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' # remove leading zeros if all elements in the row are zero
#' zero_trim(rts, method = "first")
#' @export
zero_trim <- function(x, method = c("both", "first", "last")) {


  # Use function inherits instead of is.ts to check if x1 is a timeseries.
  # is.ts returns FALSE if x1 is a timeseries with 0 columns
  if (!inherits(x, "ts")) {
    stop("Argument x is not a timeseries")
  } else if (!is.regts(x)) {
    x <- as.regts(x)
  }
  method <- match.arg(method)

  if (NCOL(x) == 0) return(x)

  if (!is.matrix(x)) {
    sel <- x != 0
    sel[is.na(sel)] <- TRUE
    elem <- which(sel)
  } else {
    sel <- apply(x != 0, 1, any)
    sel[is.na(sel)] <- TRUE
    elem <- which(sel)
  }

  if (length(elem) == 0) {
    return(NULL)
  }

  if (method == "both") {
    sel <- min(elem) : max(elem)
  } else if (method == "first") {
    sel <- min(elem) : NROW(x)
  } else {
    sel <- 1 : max(elem)
  }

  per <- start_period(get_period_range(x)) + sel[1] - 1
  period <- period_range(per, per + length(sel) - 1)

  return(x[period, , drop = FALSE])
}

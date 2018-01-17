#' Function for removing leading and trailing zeros
#'
#' This function removes leading or trailing zeros or both from a (multivariate)
#' regts object.
#' For multivariate regts a row will by default be regarded as 0 if all elements
#' in the row are 0.
#'
#' @param x a regts object
#' @param method character string with values \code{"both",} \code{"first"} or
#' \code{"last"} to remove zeros at both ends (by default), just at the start
#' or just at the end.
#' @return A \code{\link{regts}} object in which leading and/or trailing zeros have been removed.
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
#' data <- matrix(c(0, 3, 0, 0, 5, 6, 0, 7, 0), ncol = 3)
#' rts <- regts(data, start = "2010Q1", names = c("a", "b", "c"))
#' # remove leading zeros if all elements in the row are zero
#' zero_trim(rts, method = "first")
#' @export
zero_trim <- function (x, method = c("both", "first","last")) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  } else if (!is.regts(x)) {
    x <- as.regts(x)
  }
  side <- match.arg(method)

  if (!is.matrix(x)) {
    elem <- which(x != 0)
    len <- length(x)
  } else {
    elem <-   which(!apply(x == 0, 1, all))
    len <- nrow(x)
  }
  if (length(elem) == 0) {
    return(NULL)
  }
  if (side == "both") {
    sel <- min(elem) : max(elem)
  } else if (side == "first") {
    sel <- min(elem) : len
  } else {
    sel <- 1 : max(elem)
  }
  per <- start_period(get_period_range(x)) + sel[1] - 1
  period <- period_range(per, per + length(sel) - 1)

  return(x[period, , drop = FALSE])
}

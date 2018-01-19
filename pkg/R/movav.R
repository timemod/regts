#' Moving average of a timeseries
#'
#' @description
#' Function \code{movavb} computes the backward moving avergage and
#' function \code{movavc} the centered moving average.
#'
#' For example, the backward moving average of order 3 is defined as
#'
#' \eqn{A[t] = (x[t-2] + x[t-1] + x[t]) / 3},
#'
#' while the centered moving average of order 3 is calculated as
#'
#' \eqn{A[t] = (x[t - 1] + x[t] + x[t + 1]) / 3}
#'
#' Currently, the centered moving average has only been implemented for
#' odd orders.
#'
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}} object
#' @param order the order of the moving average
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{order} NA values. For
#' \code{movavc} these NAs will apear on the left side of and for \code{movavc}
#' they will be distributed over both sides.
#' If \code{TRUE} then the result timeseries is \code{order} periods
#' shorter than the input timeseries.
#' @return a \code{regts} object with the moving average values
#' @examples
#' x <- regts(rnorm(10), start = "2018Q1")
#'
#' movavb(x, order = 3)
#'
#' movavc(x, order = 3, keep_range = FALSE)
#' @useDynLib regts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @name movav
NULL

#' @describeIn movav Backward moving average
#' @export
movavb <- function(x, order, keep_range = TRUE) {
  return(movav_internal(x, from = -(order) + 1L, to = 0L,
                        keep_range = keep_range))
}

#' @describeIn movav Centered moving average (currently only for odd orders)
#' @export
movavc <- function(x, order, keep_range = TRUE) {
  if (!order%%2) {
    stop("movavc not yet supported for even orders.")
  } else {
    bound <- floor(order / 2)
    return(movav_internal(x, from = -bound, to = bound,
                          keep_range = keep_range))
  }
}

movav_internal <- function(x, from, to, keep_range) {
  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  data <- moving_average(x, from, to, keep_range)

  if (!x_is_matrix) {
    # convert the vector to data
    dim(data) <- NULL
  }

  # determine resulting period range
  old_range <- get_period_range(x)
  if (keep_range) {
    new_range <- old_range
  } else {
    new_range <- period_range(start_period(old_range) - from,
                              end_period(old_range) - to)
  }

  return(regts(data, period = new_range, names = colnames(x),
               labels = ts_labels(x)))
}

#' Moving average of a timeseries
#'
#' @description
#' Function \code{movavb} computes the backward moving average and
#' function \code{movavc} the centered moving average.
#'
#' For example, the backward moving average of order 3 is defined as
#'
#' \code{A[t] = (x[t-2] + x[t-1] + x[t]) / 3},
#'
#' while the centered moving average of order 3 is calculated as
#'
#' \code{A[t] = (x[t - 1] + x[t] + x[t + 1]) / 3}.
#'
#' The calculation of the centered moving average for even orders is somewhat
#' more complicated, see Details.
#'
#' @details
#' The centered moving average for even orders is usually computed by
#' using one more observation than the order and to use weights 0.5 for the
#' end points. For example, for order 4 we have
#'
#' \code{A[t] = (0.5 x[t - 2] + x[t - 1] + x[t] + x[t + 1] + 0.5 x[t + 2]) / 4}.
#'
#' In this way the observations are distributed evenly over the past
#' and future. An alternative approach is to use the same number
#' of observations as the order but use one more observation from the past
#' than from the future, or the other way around. These methods can be be used
#' by specifying argument \code{method}. Possible methods are
#' \describe{
#' \item{\code{centre}}{Standard method
#'  e.g.  (0.5 x[t - 2] + x[t - 1] + x[t] + x[t + 1] + 0.5 x[t + 2]) / 4}
#' \item{\code{left  }}{Use one more observation from the past, e.g.
#' (x[t - 2] + x[t - 1] + x[t] + x[t + 1]) / 4}
#' \item{\code{right }}{Use one more observation from the future, e.g.
#' (x[t - 1] + x[t] + x[t + 1] + x[t + 2]) / 4}
#' }
#'
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}} object
#' @param order the order of the moving average
#' @param method method used to handle the centered moving average for
#' even orders. Possible values are \code{"centre"} (the default),
#' \code{"left"} and \code{"right"}. See Details. This argument is ignored for odd orders.
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{order} NA values. For
#' \code{movavb} these NAs will appear on the left side and for \code{movavc}
#' they will be distributed over both sides.
#' If \code{FALSE} then the result timeseries is \code{order} periods
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
  return(movav_internal(x, w = numeric(), from = -(order) + 1L, to = 0L,
                        keep_range = keep_range))
}

#' @describeIn movav Centered moving average
#' @export
movavc <- function(x, order, keep_range = TRUE,
                   method = c("centre", "left", "right")) {

  method <- match.arg(method)

  is_odd_order <- as.logical(order%%2)
  if (is_odd_order || method == "centre") {
    to <- floor(order / 2)
    from <- -to
  } else if (method == "right") {
    to <- order / 2
    from <- - to + 1
  } else {
    to <- order / 2 - 1
    from <- - to - 1
  }

  if (is_odd_order || method != "centre") {
    w <- numeric(0)
  } else {
    w <- (c(0.5, rep(1, (order - 1)), 0.5))
  }

  return(movav_internal(x, w = w, from = from, to = to,
                        keep_range = keep_range))
}

movav_internal <- function(x, w, from, to, keep_range) {
  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  data <- moving_average(x, w, from, to, keep_range)

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

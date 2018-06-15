#' Return the relative change of a timeseries
#'
#' @description
#' Function \code{growth} computes the relative change of a timeseries.
#' The one period relative change of a timeseries is defined as:
#' growth(x) <- (x[t] - x[t-1]) / |x[t-1]|
#'
#' The \code{n} period relative change of a timeseries is defined as:
#' growth(x) <- (x[t] - x[t-\code{n}]) / |x[t-\code{n}]|
#'
#' The formula implies that when the timeseries decreases, the result will be
#' negative regardless of the sign of x. The function also works for
#' multivariate timeseries.
#'
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}} object
#' @param n an integer indicating which lag to use
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{lag} NA values at the start.
#' If \code{FALSE} then the result timeseries is \code{lag} periods
#' shorter than the input timeseries.
#'
#' @return a \code{regts} object with relative changes
#' @examples
#' x <- regts(rnorm(10), start = "2018Q1")
#' growth(x, keep_range = FALSE)
#' growth(x, 4)

#' @export
# function works for univariate and multivariate regts
growth <- function(x, n = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
     stop("Argument x is not a timeseries")
  }

  if (n >= NROW(x)){
    stop("Timeseries must have more observations than size of lag")
  }

  ret <- (diff(x, n) / abs(lag(x,-n)))

  # if keep_range then extend data with NA at start of period
  if (keep_range) {
    ret <- ret[get_period_range(x)]
  }

  ts_labels(ret) <- ts_labels(x)

  return(ret)

}

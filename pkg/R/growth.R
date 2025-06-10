#' Return the relative growth rate of a timeseries
#'
#' @description
#' Function \code{growth} computes the relative growth rate of a (multivariate)
#' timeseries. The one period relative growth rate  of a timeseries is defined
#' as \code{growth(x) = (x[t] - x[t-1]) / x[t-1]}.
#' The \code{n} period relative change of a timeseries is defined as
#' \code{growth(x,n) = (x[t] - x[t-n]) / x[t-n]}.
#'
#' Note that `growth` divides the change by ` x[t-n]` and not by the
#' absolute value of `x[t-1]`.
#' This implies that the growth rate is positive when a negative timeseries
#' becomes more negative.
#'
#' Vector, matrix and data frame arguments are first converted to a \code{regts}
#' with function \code{\link{regts}}. This conversion results in a timeseries
#' with frequency 1 and starting at year 1.
#'
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}} object.
#' Can also be a vector, matrix or data frame (see Details).
#' @param n an integer indicating the period of relative change
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{n} \code{NA} values at the start.
#' If \code{FALSE} then the result timeseries is \code{n} periods
#' shorter than the input timeseries.
#'
#' @return a \code{regts} object with relative changes
#' @seealso \code{\link{rel2index}}, \code{\link{lag_ts}} and
#' \code{\link{diff_ts}}.
#' @examples
#' x <- regts(rnorm(10), start = "2018Q1")
#' growth(x, keep_range = FALSE)
#' growth(x, 4)
#'
#' @importFrom stats lag
#' @export
# function works for univariate and multivariate regts
growth <- function(x, n = 1, keep_range = TRUE) {

  if (n >= NROW(x)) {
    stop("Timeseries must have at least ", n + 1, " observations")
  }

  ret <- diff_ts(x, n, keep_range = keep_range) /
    lag_ts(x, n, keep_range = keep_range)

  return(ret)
}

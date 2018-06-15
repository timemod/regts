#' Return the growth function of a timeseries
#'
#' @description
#' Function \code{growth_ts} computes the growth function of a timeseries.
#' This function is defined as: (x[t] - x[t-lag]) / |x[t-lag]|
#'
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}} object
#' @param lag an integer indicating which lag to use
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{lag} NA values at the start.
#' If \code{FALSE} then the result timeseries is \code{lag} periods
#' shorter than the input timeseries.
#'
#' @return a \code{regts} object with growth values
#' @examples
#' x <- regts(rnorm(10), start = "2018Q1")
#'
#' growth_ts(x, lag = 4)

#' @export
# function works for univariate and multivariate regts
growth_ts <- function(x, lag = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
     stop("Argument x is not a timeseries")
  }
  range <- get_period_range(x)
  if (lag >= nperiod(range)){
    stop("Timeseries must have more observations than size of lag")
  }

  data <- (diff(x, lag) / abs(lag(x,-lag)))

  # if keep_range then extend data else adapt period range
  if (keep_range) {
    # extend data with NA at start of period
    data <- data[range]
  } else {
    range <- period_range(start_period(range) + lag, end_period(range))
  }

  return(regts(data, period = range, names = colnames(x),
               labels = ts_labels(x)))
}

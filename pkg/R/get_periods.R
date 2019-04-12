#' Return all periods in a  \code{period_range} or timeseries.
#'
#' The periods in a \code{\link{period_range}} or timeseries are returned
#' as a \code{period} vector
#'
#' @param x a \code{\link{period_range}} or times series
#' (\code{\link[stats]{ts}} or \code{\link{regts}}).
#' @param ... arguments passed to methods (currently not used).
#' @examples
#' # example for period range
#' range <- period_range("2018m1/2018m3")
#' get_periods(range)
#'
#' # example for timeseries
#' x <- regts(1:3, start = "2010Q4")
#' get_periods(x)
#' @seealso \code{\link{get_period_range}}
#' @export
get_periods <- function(x, ...) {
  UseMethod("get_periods")
}

#' @export
get_periods.period_range <- function(x, ...) {
  return(create_period(as.numeric(x[1] : x[2]), frequency = frequency(x)))
}

#' @export
get_periods.ts <- function(x, ...) {
  return(get_periods(get_period_range(x)))
}

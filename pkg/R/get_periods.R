#' Return all periods in a  \code{period_range} or timeseries.
#'
#' The periods in a \code{\link{period_range}} or timeseries are returned
#' as a \code{\link{period}} vector
#'
#' @param x a \code{\link{period_range}}, a character that can be coerced to a
#' `period_range` with function \code{\link{as.period_range}}, or a times series
#' (\code{\link[stats]{ts}} or \code{\link{regts}}).
#' @param ... arguments passed to methods (currently not used).
#' @return A \code{\link{period}} vector.
#' @examples
#' # example for period range
#' range <- period_range("2018m1/2018m3")
#' get_periods(range)
#'
#' # example for timeseries
#' x <- regts(1:3, start = "2010Q4")
#' get_periods(x)
#'
#' # example: print the value of a timeseries for every quarter
#' x <- regts(1:4, start = "2018q1")
#' for (prd in as.list(get_periods(x))) {
#'   cat(sprintf("x[%s] = %g\n", prd, x[prd]))
#' }
#' # Note that we do not loop directly over the period vector, but first convert
#' # the vector to a list. Otherwise the period class is lost.
#' @seealso \code{\link{period}}, \code{\link{get_period_range}} and
#' \code{\link{seq}}
#' @export
get_periods <- function(x, ...) {
  UseMethod("get_periods")
}

#' @rdname get_periods
#' @export
get_periods.period_range <- function(x, ...) {
  return(create_period(as.numeric(x[1] : x[2]), frequency = frequency(x)))
}

#' @rdname get_periods
#' @export
get_periods.ts <- function(x, ...) {
  return(get_periods(get_period_range(x)))
}

#' @rdname get_periods
#' @export
get_periods.character <- function(x, ...) {
  return(get_periods(as.period_range(x), ...))
}

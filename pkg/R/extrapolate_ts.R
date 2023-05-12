#' Extrapolate timeseries
#'
#' Extrapolate a timeseries by assuming a constant level or first difference
#' in the extrapolation period.
#'
#' The result is a timeseries with end period equal to `final_period`.
#' If `method == "level_constant"` (the default),
#' the values in period `last_period_x + 1` and `final_period` are
#' equal to the values of `x` in `last_period_x`.
#' If `method == "diff_constant"` ,
#' the first differences in period `last_period_x + 1` and `final_period` are
#' equal to first difference of `x` in `last_period_x`.

#' @param x a timeseries object (\code{\link{regts}} or \code{\link[stats]{ts}}
#' object).
#' @param final_period The final period to which the timeseries is
#' extrapolated. This should be a \code{\link{period}} object or an
#' object that can be coerced to a `period` object.
#' @param last_period_x The last period in timeseries `x` used
#' to extrapolate the timeseries. Ths period should be smaller than
#' `final_period`.
#' @param method the extrapolation method. See details.
#' @return a code `regts` object
#' @examples
#' x <- regts(rnorm(3), start = "2019q1")
#' extrapolate_ts(x, "2020q1")
#'
#' x <- regts(matrix(rnorm(8), ncol = 2), names = c("a", "b"),
#'            start = "2019q1")
#' plot(extrapolate_ts(x, final_period = "2020q4",
#'                last_period_x = "2019q3", method = "diff_constant"))
#' @export
extrapolate_ts <- function(x, final_period, last_period_x = end_period(x),
                           method = c("level_constant", "diff_constant")) {
  # TODO: add method growth_constant
  method <- match.arg(method)
  x <- as.regts(x)
  last_period_x <- as.period(last_period_x)
  final_period <- as.period(final_period)
  if (frequency(final_period) != frequency(x)) {
    stop("final period has a different frequency than timeseries x")
  }
  if (!missing(last_period_x)) {
    if (frequency(last_period_x) != frequency(x)) {
      stop("last_period has a different frequency than timeseries x")
    }
    if (final_period <= last_period_x) {
      stop("final_period should be larger than last_period")
    }
  }

  range <- period_range(last_period_x + 1, final_period)
  last_value <- x[last_period_x]
  nper <- nperiod(range)
  x[range] <- rep(last_value, each = nper)
  if (method == "diff_constant") {
    diff_value <- diff(x)[last_period_x]
    diff_matrix <- matrix(rep(diff_value, each = nper), ncol = NCOL(x))
    cumulated <- apply(diff_matrix, MARGIN = 2, FUN = cumsum)
    x[range] <- x[range] + cumulated
  }
  # TODO: add method growth_constant?

  # TODO: give a warning when there are NA values at last_period,
  # and if method == "diff_constant" give a warning when there are NA
  # values at last_period - 1
  return(x[period_range(NULL, final_period)])
}

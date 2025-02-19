#' Extrapolate timeseries.
#'
#' Extrapolate a timeseries forwards by assuming a constant level or first
#' difference in the extrapolation period.
#'
#' The result is a timeseries with end period `to`.
#' If `method == "level_constant"` (the default),
#' the values in periods between `from` and `to` (including period `to`) are
#' equal to the values of `x` in period `from`.
#' If `method == "diff_constant"`,
#' the first differences in period between `from` and `to` (including period
#' `to`)
#' are equal to first difference of `x` at period `from`.

#' @param x a timeseries object (\code{\link{regts}} or \code{\link[stats]{ts}}
#' object).
#' @param to The final period to which the timeseries is
#' extrapolated. This should be a \code{\link{period}} object or an
#' object that can be coerced to a `period` object.
#' @param from The last period in timeseries `x` used
#' to extrapolate the timeseries. The period should be smaller than
#' `to`. See Details.
#' @param method the extrapolation method. See details.
#' @return a code `regts` object
#' @examples
#' # univariate ts
#' x <- regts(rnorm(3), start = "2019q1")
#' extrapolate_ts(x, "2020q1")
#'
#' # multivariate ts
#' x <- regts(matrix(rnorm(8), ncol = 2), names = c("a", "b"),
#'            start = "2019q1")
#' plot(extrapolate_ts(x, to = "2020q4",
#'                from = "2019q3", method = "diff_constant"), type = "b")
#' @export
extrapolate_ts <- function(x, to, from = end_period(x),
                           method = c("level_constant", "diff_constant")) {
  # TODO: add method growth_constant ?? Do this when required.
  method <- match.arg(method)
  x <- as.regts(x)
  to <- as.period(to)
  if (frequency(to) != frequency(x)) {
    stop("'to' has a different frequency than timeseries 'x'")
  }
  if (!missing(from)) {
    from <- as.period(from)
    if (frequency(from) != frequency(x)) {
      stop("'from' has a different frequency than timeseries 'x'")
    }
    if (to <= from) {
      stop("'from' should be smaller than 'to'")
    }
  } else if (to <= from) {
    stop("'to' should be larger than the end period of 'x'")
  }

  range <- period_range(from + 1, to)
  last_value <- x[from]
  nper <- nperiod(range)
  x[range] <- rep(last_value, each = nper)
  if (method == "diff_constant") {
    diff_value <- diff(x)[from]
    diff_matrix <- matrix(rep(diff_value, each = nper), ncol = NCOL(x))
    cumulated <- apply(diff_matrix, MARGIN = 2, FUN = cumsum)
    x[range] <- x[range] + cumulated
  }

  return(x[period_range(min(start_period(x), from), to)])
}

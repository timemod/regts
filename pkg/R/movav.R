#' Moving average of a timeseries
#'
#' @description
#' The moving average smoother calculates for each observation the average
#' of the observations in a range around that observation.
#' For example, the backwards moving average
#' of order 3 is given by  \eqn{A[t] = (x[t-2] + x[t-1] + x[t]) / 3}.
#' In general, the moving average is calculated as
#'
#' \eqn{A[t] = (x[t - p] + x[t - p + 1] + \dots + x[t] + \dots +
#' x[t + q - 1] + x[t + q]) / n},
#'
#' where \eqn{p} and \eqn{q} are the
#' maximum lag and lead, respectively, and \eqn{n = p + q + 1} is the order
#' of the moving average.
#'
#' @param x a a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param max_lag the maximum lag
#' @param max_lead the maximal lead
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' The result will have \code{max_lag} \code{NA}s at the left side and
#' \code{max_lead} NAs at the right side). If \code{FALSE}, then the
#' result has a shorter period rang than the input timeseries
#' (it starts \code{max_lags} periods later and ends \code{max_lead} periods
#' earlier).
#' @return a \code{regts} object with the moving average values
#' @examples
#' x <- regts(rnorm(10), start = "2018Q1")
#'
#' # backward moving average of order 3
#' movav(x, max_lag = 2)
#'
#' # centered moving average of order 3
#' movav(x, max_lag = 1, max_lead = 1, keep_range = FALSE)
#' @export
#' @useDynLib regts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
movav <- function(x, max_lag = 0L, max_lead = 0L, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  if (max_lag < 0 || max_lead < 0) {
    stop("Argument max_lag and max_lead should be >= 0")
  }


  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  data <- moving_average(x, max_lag, max_lead, keep_range)

  if (!x_is_matrix) {
    # convert the vector to data
    data <- data[, 1, drop = TRUE]
  }

  # determine resulting period range
  old_range <- get_period_range(x)
  if (keep_range) {
    new_range <- old_range
  } else {
    new_range <- period_range(start_period(old_range) + max_lag,
                              end_period(old_range) - max_lead)
  }

  return(regts(data, period = new_range, names = colnames(x),
                 labels = ts_labels(x)))
}

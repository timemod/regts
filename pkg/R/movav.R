#' Moving average
#'
#' Function \code{movav} calculates the moving average of a timeseries.
#'
#' The moving average of a timeseries \eqn{x[t]} at time \eqn{t}
#' is defined as \eqn{A[t] = (x[t - p] + x[t - p + 1] + ... +
#' x[t + q - 1] + x[t + q]) / n}, where \eqn{-p} is the maximum lag
#' and \eqn{q} the maximum lead in the summation
#' @param x a a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param max_lag a numeric value specifying the maximum lag
#' for the moving averaging calculation. This should be a negative number or zero.
#' @param max_lead a numeric value the bound in the summation (see details).
#' This should be a positive number or zero.
#' @param na_pad If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' NA values are padded to the left and the right of calculated average (
#' there wille be \code{-from} \code{NA}s at the left side and \code{to} NAs
#' to the right side)
#' @return a \code{regts} object with the moving average values
#' @export
#' @useDynLib regts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
movav <- function(x, max_lag = 0L, max_lead = 0L, na_pad = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  if (max_lag < 0) {
    stop("Argument from should be >= 0")
  }
  if (max_lead < 0) {
    stop("Argument to should be >= 0")
  }

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  data <- moving_average(x, max_lag, max_lead, na_pad)

  if (!x_is_matrix) {
    # convert the vector to data
    data <- data[, 1, drop = TRUE]
  }

  # determine resulting period range
  old_range <- get_period_range(x)
  if (na_pad) {
    new_range <- old_range
  } else {
    new_range <- period_range(start_period(old_range) + max_lag,
                              end_period(old_range) - max_lead)
  }

  return(regts(data, period = new_range, labels = ts_labels(x)))
}

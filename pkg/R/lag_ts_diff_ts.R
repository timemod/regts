#' Lag or Lead a Timeseries
#'
#' Computes the lag or lead of a timeseries by shifting the observations
#' by a given number of periods.
#' @param x A univariate or multivariate timeseries
#' @param k The number of lags or leads (in units of observations).
#'          Must be a positive number.
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{k} \code{NA} values at the
#' beginning (\code{lead_ts}) or the end (\code{lag_ts}). For
#' If \code{FALSE} then the result timeseries has a shifter period range.
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' lag_ts(x, 1)    # calculate x[t+1]
#' lead_ts(x, 1)   # return x[t-1]
#' @name lag_ts-lead_ts
NULL

#' @describeIn lag_ts-lead_ts Lag a timeseries
#' @export
lag_ts <- function(x, k = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }
  if (k < 0) stop("Argument k should be positive")

  return(shift_ts(x, k = -k, keep_range = keep_range))
}

#' @describeIn lag_ts-lead_ts Lead a timeseries
#' @export
lead_ts <- function(x, k = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }
  if (k < 0) stop("Argument k should be positive")

  return(shift_ts(x, k = k, keep_range = keep_range))
}

# internal function used by both lag_ts and lead_ts
shift_ts <- function(x, k, keep_range) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  if (k == 0) return(x)

  if (keep_range) {

    # keep the original range, use function data.table::shift
    type <- if (k < 0) "lag" else "lead"
    shift_fun <- function(ts) {
      return(data.table::shift(ts, n = abs(k), type = type))
    }
    if (is.matrix(x)) {
      x[] <- apply(x, MARGIN = 2 , FUN = shift_fun)
      return(x)
    } else {
      return(shift_fun(x))
    }
  } else {
    # use the stats::lag function
    return(stats::lag(x, k = k))
  }
}

#' Lagged differences
#'
#' Returns suitably lagged and iterated differences.
#' This function works similar as \code{\link[base]{diff}},
#' except that the period range of the result is the same as that
#' of the input timeseries. This behaviour can be changed by specifying
#' argument \code{keep_range}.
#'
#' @param x A univariate or multivariate timeseries
#' @param lag an integer indicating which lag to use
#' @param differences an integer indicating the order of the difference.
#' @param keep_range If \code{TRUE} (the default), then  the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{lag + differences - 1}
#' \code{NA} values at the beginning (\code{lead_ts}) or the end (\code{lag_ts}).
#' If \code{FALSE} then the result timeseries starts \code{lag + differences - 1}
#' periods later than the input timeseries.
#' @export
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' diff_ts(x)
#' diff_ts(x, lag = 2, keep_range = FALSE)
diff_ts <- function(x, lag = 1, differences = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  k <- lag + differences - 1
  n <- NROW(x)
  if (n < k + 1) {
    stop("Timeseries x has too few observations")
  }

  diff_result <- diff(x, lag = lag, differences = differences)

  if (keep_range) {
    n <- NROW(x)
    k <- lag + differences - 1
    x_sel_na <- seq(1, k)
    x_sel <- seq(k + 1, n)

    if (is.matrix(x)) {
      x[x_sel_na, ] <- NA
      x[x_sel, ]  <- diff_result
    } else {
      x[x_sel_na] <- NA
      x[x_sel] <- diff_result
    }
    return(x)
  } else {
    return(diff_result)
  }
}

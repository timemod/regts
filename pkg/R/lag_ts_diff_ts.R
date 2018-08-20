#' Lag or Lead a Timeseries
#'
#' Compute the lag or lead of a timeseries, shifting the observations
#' by a given number of periods.
#'
#' \code{lag_ts} and \code{lead_ts} differ from \code{\link[stats]{lag}}
#' in the \code{stats} package in that the specified number of lags or leads
#' is positive, and that by default the resulting timeseries has the same
#' period range as the input timeseries.
#'
#' @param x a univariate or multivariate timeseries
#' @param n the number of lags or leads (in units of observations).
#'          Must be a positive number.
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{n} \code{NA} values at the
#' beginning (\code{lag_ts}) or the end (\code{lead_ts}).
#' If \code{FALSE} the period range of the result timeseries is shifted
#' by \code{n} periods. For example, for \code{lag_ts} the result timeseries
#' starts and ends \code{n} periods later.
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' lag_ts(x, 1)                        # calculate x[t-1]
#' lead_ts(x, 1, keep_range = FALSE)   # calculate x[t+1]
#' @name lag_ts/lead_ts
NULL

#' @describeIn lag_ts-slash-lead_ts Lag a timeseries
#' @export
lag_ts <- function(x, n = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }
  if (n < 0) stop("Argument n should be positive")

  return(shift_ts(x, k = -n, keep_range = keep_range))
}

#' @describeIn lag_ts-slash-lead_ts Lead a timeseries
#' @export
lead_ts <- function(x, n = 1, keep_range = TRUE) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }
  if (n < 0) stop("Argument n should be positive")

  return(shift_ts(x, k = n, keep_range = keep_range))
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

#' Lagged differences of a timeseries
#'
#' Returns suitably lagged and iterated differences of a timeseries.
#' This function works similarly as \code{\link[base]{diff}},
#' except that the period range of the result is the same as that
#' of the input timeseries. This behaviour can be changed by specifying
#' argument \code{keep_range}.
#'
#' @param x a univariate or multivariate timeseries
#' @param lag an integer indicating which lag to use
#' @param differences an integer indicating the order of the difference.
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{lag + differences - 1}
#' \code{NA} values at the beginning.
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

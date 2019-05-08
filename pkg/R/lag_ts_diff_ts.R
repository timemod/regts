#' Lag a Timeseries
#'
#' Compute the lag of a timeseries, shifting the observations
#' forwards by a given number of periods.
#'
#' Vector, matrix and data frame arguments are first converted to a \code{regts}
#' with function \code{\link{regts}}. This conversion results in a timeseries
#' with frequency 1 and starting at year 1.
#'
#' \code{lag_ts} differs from \code{\link[stats]{lag}}
#' in the \code{stats} package in that the specified number of lags or leads
#' is positive, and that by default the resulting timeseries has the same
#' period range as the input timeseries.
#'
#' @param x a univariate or multivariate timeseries.
#' Can also be a vector, matrix or data frame (see details).
#' @param n the number of lags (in units of observations).
#'          Must be a positive number.
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' The result timeseries will have \code{n} \code{NA} values at the
#' beginning. If \code{FALSE} the period range of the result timeseries is
#' shifted by \code{n} periods. The result timeseries
#' starts and ends \code{n} periods later.
#' @param ... further arguments to be passed to or from methods
#'            (currently not used in package \code{regts})
#' @seealso \code{\link{lead_ts}} and \code{\link{diff_ts}}
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' lag_ts(x)
#' lag_ts(x, k = 2, keep_range = FALSE)
#' @export
lag_ts <- function(x, n = 1, keep_range = TRUE, ...) {
  UseMethod("lag_ts")
}

#' @rdname lag_ts
#' @export
lag_ts.ts <- function(x, n = 1, keep_range = TRUE, ...) {

  if (n < 0) stop("Argument n should be positive")

  return(shift_ts(x, k = -n, keep_range = keep_range))
}

# @rdname lag_ts
#' @export
lag_ts.default <- function(x, n = 1, keep_range = TRUE, ...) {
  return(lag_ts(regts(x), n = n, keep_range = keep_range, ...))
}

#' Lead a Timeseries
#'
#' Compute the lead of a timeseries, shifting the observations
#' backwards by a given number of periods.
#'
#' Vector, matrix and data frame arguments are first converted to a \code{regts}
#' with function \code{\link{regts}}. This conversion results in a timeseries
#' with frequency 1 and starting at year 1.
#'
#' @param x a univariate or multivariate timeseries.
#' Can also be a vector, matrix or data frame (see details).
#
#' @param n the number of leads (in units of observations).
#'          Must be a positive number.
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' The result timeseries will have \code{n} \code{NA} values at the
#' beginning end.
#' If \code{FALSE} the period range of the result timeseries is shifted
#' by \code{n} periods. The result timeseries
#' starts and ends \code{n} periods earlier.
#' @param ... further arguments to be passed to or from methods
#'            (currently not used in package \code{regts})
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' lead_ts(x)
#' lead_ts(x, k = 2, keep_range = FALSE)
#' @seealso \code{\link{lag_ts}} and \code{\link{diff_ts}}
#' @export
lead_ts <- function(x, n = 1, keep_range = TRUE, ...) {
  UseMethod("lead_ts")
}

#' @rdname lead_ts
#' @export
lead_ts.ts <- function(x, n = 1, keep_range = TRUE, ...) {

  if (n < 0) stop("Argument n should be positive")

  return(shift_ts(x, k = n, keep_range = keep_range))
}

#' @rdname lead_ts
#' @export
lead_ts.default <- function(x, n = 1, keep_range = TRUE, ...) {
  return(lead_ts(regts(x), n = n, keep_range = keep_range, ...))
}

#
# internal function used by both lag_ts and lead_ts
#
shift_ts <- function(x, k, keep_range) {

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
#' Vector, matrix and data frame arguments are first converted to a \code{regts}
#' with function \code{\link{regts}}. This conversion results in a timeseries
#' with frequency 1 and starting at year 1.
#'
#' @param x a univariate or multivariate timeseries.
#' Can also be a vector, matrix or data frame (see details).
#' @param lag an integer indicating which lag to use
#' @param differences an integer indicating the order of the difference.
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' Then the result timeseries will have \code{lag + differences - 1}
#' \code{NA} values at the beginning.
#' If \code{FALSE} then the result timeseries starts \code{lag + differences - 1}
#' periods later than the input timeseries.
#' @param ... further arguments to be passed to or from methods
#'            (currently not used in package \code{regts})
#' @export
#' @examples
#' x <- regts(1:10, start = "2018q3")
#' diff_ts(x)
#' diff_ts(x, lag = 2, keep_range = FALSE)
#' @seealso \code{\link{lag_ts}} and \code{\link{lead_ts}}
diff_ts <- function(x, lag = 1, differences = 1, keep_range = TRUE, ...) {
  UseMethod("diff_ts")
}

#' @rdname diff_ts
#' @export
diff_ts.ts <- function(x, lag = 1, differences = 1, keep_range = TRUE, ...) {

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

#' @rdname diff_ts
#' @export
diff_ts.default <- function(x, lag = 1, differences = 1, keep_range = TRUE, ...) {
  return(diff_ts(regts(x), lag = lag, differences = differences,
                 keep_range = keep_range, ...))
}

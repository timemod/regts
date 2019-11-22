#' Calculate an index timeseries from a timeseries with relative or
#' percentage changes.
#'
#' Function `rel2index` is the inverse of function \code{\link{growth}}.
#' The growth `g[t]` (also called the relative change) of a timeseries `z[t]`
#' is defined as
#' ```
#' g[t] = (z[t] - z[t - 1]) / |z[t - 1]|.
#' ```
#' The function constructs an index series for `z[t]` given the values of
#' `g[t]`, assuming that the value of timeseries `z` at the
#' period before the start period of timeseries `g` (specified with
#' argument `x`) is positive. See Details.
#' \cr\cr
#' Function `pct2index` computes the index series
#' from a timeseries of percentage changes, defined as `100 * g[t]`.
#' Thus expression `pct2index(x)` gives the same result as `rel2index(x / 100)`.
#'
#'
#' If `g[t]` is given but `z[t]` is unknown, we can compute
#' `z[t]` as
#' ```
#' z[t] = z[t - 1] * (1 + sign(z[t - 1]) g[t]).
#' ```
#' Given an initial value for `z` at some period (say `z[0]`), the equation
#' above can be used repeatedly to calculate the values of `z[t]` for `t > 0`.
#' In function `rel2index` `z[0]` is not known, but if we assume that it is
#' positive, this value is not needed if we calculate the index series
#' defined as
#' ```
#' i[t] =  scale * z[t] / z[base].
#' ```
#' The index series `i` is independent on the absolute value of `z[0]`, but
#' does depend on the sign of `z[0]`. If `z[0]` is actually negative
#' then the results of `rel2index` and `pct2index` are not correct.
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}} (can also be a
#' multivariate timeseries) with the relative of percentage changes.
#' @param base a \code{\link{period}} or a
#' \code{\link{period_range}} specifying the base period, or an object that can
#' be coerced to a \code{period} or \code{period_range}.
#' By default the base period is the period before the first period of the
#' input timeseries `x`. For example,  if `x` starts at `2018q1`, then the
#' default base period is `2017q3`. If the base period is a `period_range`,
#' then the average value of the index series will be equal to `scale`.
#' @param scale the (average) value of the index series at the base period
#' (by default 100)
#' @param keep_range if \code{TRUE} (the default), then the output
#' timeseries has the same period range as the input timeseries.
#' If \code{FALSE} then the result timeseries starts 1 period earlier.
#'
#' @seealso \code{\link{index_ts}} and \code{\link{growth}}
#' @examples
#' ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
#' print(rel2index(ts1))
#' print(rel2index(ts1, base = "2010Q3", scale = 1, keep_range = TRUE))
#' @name rel2index/pct2index
NULL

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with relative changes
#' @export
rel2index <- function(x, base = NULL, scale = 100,
                          keep_range = TRUE) {

  x <- as.regts(x)

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  # call C++ function rel2index_cpp (in file src/rel2index_cpp.cpp)
  # to perform the cumulation.
  data <- rel2index_cpp(x)

  if (is.null(base)) {

    # the base period has not been specified, thus it is the first
    # period before the start of timeseries x.
    # For efficiency, the data is prepared first and the timeseries
    # is created only at the last stage.

    if (keep_range) {
      # remove first row
      data <- data[-1, , drop = FALSE]
      new_start <- start_period(x)
    } else {
      new_start <- start_period(x) - 1
    }

    if (!x_is_matrix) {
      # convert the vector to data
      dim(data) <- NULL
    }

    if (scale != 1) data <- data * scale

    return(regts(data, start = new_start, names = colnames(x),
                 labels = ts_labels(x)))

  } else {

    # base period specified

    if (!x_is_matrix) {
      # convert the vector to data
      dim(data) <- NULL
    }

    # prepare a timeseries that can be passed to function index_ts.

    result <- regts(data, start = start_period(x) - 1, names = colnames(x),
                    labels = ts_labels(x))

    result <- index_ts(result, base, scale = scale)

    if (keep_range) {
      result <- result[get_period_range(x)]
    }
    return(result)
  }
}

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with percentage changes
#' @export
pct2index <- function(x, base = NULL, scale = 100, keep_range = TRUE) {
  return(rel2index(x / 100, base = base, scale = scale,
                   keep_range = keep_range))
}

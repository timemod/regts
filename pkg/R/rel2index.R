#' Calculate an index timeseries from a timeseries with relative or
#' percentage changes.
#'
#' Function `rel2index` is the inverse of function \code{\link{growth}}.
#' The growth rate `x[t]` (also called the relative change) of a timeseries `z[t]`
#' is defined as
#' ```
#' x[t] = (z[t] - z[t-1]) / z[t-1].
#' ```
#' Note that according to this definition the denominator is not the absolute
#' value of \code{z[t-1]}).
#' The function constructs an index series for `z[t]` given the values of
#' `x[t]`. See Details.
#' \cr\cr
#' Function `pct2index` computes the index series
#' from a timeseries of percentage changes.
#' Thus expression `pct2index(x)` gives the same result as `100 * rel2index(x / 100)`.
#'
#' If `x[t]` is given but `z[t]` is unknown, `z[t]` can be calculated from
#' `z[t-1]` using
#' ```
#' z[t] = z[t-1] * (1 + x[t]).
#' ```
#' Given an initial value for `z` at some period (say `z[0]`), the equation
#' above can be used repeatedly to calculate the values of `z[t]` for `t > 0`.
#' Finally the index series is computed using
#' ```
#' i[t] =  scale * z[t] / mean(z[base]),
#' ```
#' where `base` is the base period.
#'
#' `z[0]` is not known, but the result `i[t]` does not depend on `z[0]` and
#' therefore it can be arbitrarily set to 1.
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}} (can also be a
#' multivariate timeseries) with the relative of percentage changes.
#' @param base a \code{\link{period}} or a
#' \code{\link{period_range}} specifying the base period, or an object that can
#' be coerced to a \code{period} or \code{period_range}. The (average) value
#' of the timeseries at the base period is set to `scale` (by default 100).
#' By default the base period is the period before the first period of the
#' input timeseries `x`. For example,  if `x` starts at `2018q1`, then the
#' default base period is `2017q4`. If the base period is a `period_range`,
#' then the average value of the index series will be equal to `scale`.
#' @param scale the (average) value of the index series at the base period
#' (by default 100). This may be a negative number.
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
rel2index <- function(x, base = NULL, scale = 100, keep_range = TRUE) {

  series_name <- deparse(substitute(x))

  if (!is.numeric(scale) || length(scale) != 1) {
    stop("Argument 'scale' should be a numeric vector of length 1")
  }
  # remove attributes (for example time series attributes):
  scale <- as.numeric(scale)
  if (is.na(scale)) stop("The scale is NA")

  x <- as.regts(x)

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  # calculate the index of the start period of the base period in
  # timeseries x
  if (is.null(base)) {
    first_base_row <- 0
  } else {
    base <- check_base(base, x)
    first_base_row <- start_period(base) - start_period(x) + 1
  }

  # call C++ function rel2index_cpp (in file src/rel2index_cpp.cpp)
  # to perform the cumulation. Subtract 1 because C++ has index base 0.
  data <- rel2index_cpp(x, as.integer(first_base_row - 1))

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
    #
    # base period specified
    #
    if (!x_is_matrix) {
      # convert the vector to data
      dim(data) <- NULL
    }

    # prepare a timeseries that can be passed to function index_ts.
    result <- regts(data, start = start_period(x) - 1, names = colnames(x),
                    labels = ts_labels(x))

    result <- index_ts_internal(result, base, scale, series_name,
                                check_negative = FALSE)

    if (keep_range) result <- result[get_period_range(x)]

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

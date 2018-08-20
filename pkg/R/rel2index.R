#' Calculate an index timeseries from a timeseries with relative or
#' percentage changes.
#'
#' This is the inverse of function \code{\link{growth}}.
#' The relative change, or growth, of a timeseries x[t]
#' is defined as \code{growth[t] = (x[t] - x[t-1]) / |x[t-1]|}.
#' Suppose that \code{growth[t]} is given but \code{x[t]} is not known.
#' Then it is possible to calculate the index series
#' \code{i[t] = s * x[t] / x[t*]}, where \code{s} is arbitrary scale and
#' \code{t*} an arbitrary base period.
#' Function \code{rel2index} computes this index series \code{i[t]}
#' \cr\cr
#' Similarly, function \code{pct2index} computes the index series
#' for a timeseries of percentage changes, defined as
#' \code{100 (x[t] - x[t-1]) / | x[t-1]|}.
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}} (can also be a multivariate
#' timeseries)
#' @param base base period of the index timeseries (a \code{\link{period}}
#' object or an object that can be coerced to a \code{period} object)
#' @param scale the value of the index series at the base period (by default 100)
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
rel2index <- function(x, base = start_period(x) - 1, scale = 100,
                      keep_range = TRUE) {

  x <- as.regts(x)

  x_is_matrix <- is.matrix(x)
  if (!x_is_matrix) {
    dim(x) <- c(length(x), 1)
  }

  # call C++ function rel2index_cpp (in file src/rel2index_cpp.cpp)
  # to perform the cumulation.
  data <- rel2index_cpp(x)

  # determine result period range
  old_range <- get_period_range(x)
  new_start <- start_period(x) - 1

  if (!missing(base)) {
    base <- as.period(base)
    if (frequency(base) != frequency(x)) {
      stop(paste0("The base period ", base, " has a different frequency",
                  " than the timeseries (", frequency(x), ")."))
    }
    base_index <- base - new_start + 1
    if (base_index < 1 || base_index > nrow(data)) {
      stop(paste0("The base period should lie between ",
          new_start, " and ", end_period(old_range), "."))
    }
    data <- apply(data, MARGIN = 2,
                  FUN = function(x) {x /x[base_index]})
  }

  if (scale != 1) data <- data * scale

  if (keep_range) {
    # remove first row
    data <- data[-1, ]
    new_start = new_start + 1
  }

  if (!x_is_matrix) {
    # convert the vector to data
    dim(data) <- NULL
  }



  return(regts(data, start = new_start, names = colnames(x),
               labels = ts_labels(x)))
}

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with percentage changes
#' @export
pct2index <- function(x, base = start_period(x) - 1, scale = 100,
                      keep_range = TRUE) {
  return(rel2index(x/100, base = base, scale = scale, keep_range = keep_range))
}



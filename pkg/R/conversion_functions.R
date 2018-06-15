#' Calculate an index timeseries from a timeseries with relative or
#' percentage changes.
#'
#' The relative change of a timeseries X[t]
#' is defined as \code{x[t] = (X[t] - X[t-1]) / |X[t-1]|}.
#' Suppose that \code{x[t]} is given but \code{X[t]} is not known.
#' Then it is possible to calculate the index series
#' \code{i[t] = s * X[t] / X[t*]}, where \code{s} is arbitrary scale and
#' \code{t*} an arbitrary base period.
#' Function \code{rel2index} computes this index series \code{i[t]}
#' \cr\cr
#' Similarly, function \code{pct2index} computes the index series
#' for a timeseries of percentage changes, defined as
#' \code{x[t] = 100 (X[t] - X[t-1]) / | X[t-1]|}.
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}}
#' @param base base period of the index timeseries (a \code{\link{period}}
#' object or an object that can be coerced to a \code{period} object)
#' @param scale the value of the index series at the base period (by default 100)
#' @seealso \code{\link{index_ts}}
#' @examples
#' ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
#' print(rel2index(ts1))
#' print(rel2index(ts1, base = "2010Q3", scale = 1))
#' @name rel2index/pct2index
NULL

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with relative changes
#' @export
rel2index <- function(x, base = start_period(x) - 1, scale = 100) {

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
  new_start <- start_period(old_range) - 1

  if (!missing(base)) {
    base <- as.period(base)
    base_index <- base - new_start + 1
    if (base_index < 1 || base_index > nrow(data)) {
      stop("The base period is outside the input period range")
    }
    data <- apply(data, MARGIN = 2,
                  FUN = function(x) {x /x[base_index]})
  }

  if (scale != 1) data <- data * scale

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
pct2index <- function(x, base = start_period(x) - 1, scale = 100) {
  return(rel2index(x/100, base = base, scale = scale))
}



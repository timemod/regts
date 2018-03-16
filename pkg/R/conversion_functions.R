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
#' @param base_period base period of the index timeseries (a \code{\link{period}}
#' object or an object that can be coerced to a \code{period} object)
#' @param scale the value of the index series at the base period
#' @seealso \code{\link{index_ts}}
#' @examples
#' ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
#' print(rel2index(ts1))
#' print(rel2index(ts1, base_period = "2010Q3", scale = 1))
#' @name rel2index/pct2index
NULL

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with relative changes
#' @export
rel2index <- function(x, base_period = start_period(x) - 1, scale = 100) {

  x <- as.regts(x)
  is_mat <- is.matrix(x)

  p_start <-  start_period(x) - 1

  if (is_mat) {
    cum_mat <- apply(1 + x, MARGIN = 2, FUN = cumprod)
    ret <- regts(rbind(rep(1, ncol(cum_mat)), cum_mat), start = p_start,
                 labels = ts_labels(x))
  } else {
    cum <- cumprod(1 + x)
    ret <- regts(c(1, cum), start = p_start, labels = ts_labels(x))
  }

  if (missing(base_period)) {
    return(scale * ret)
  } else {
    base_period <- as.period(base_period)
    i <- base_period - p_start + 1
    if (is_mat) {
      ret[] <- apply(ret, MARGIN = 2, FUN = function(x) {scale * x /x[i]})
    } else {
      ret[] <-  scale * ret / ret[i]
    }
    return(ret)
  }
}

#' @describeIn rel2index-slash-pct2index Calculates an index timeseries from a
#' timeseries with percentage changes
#' @export
pct2index <- function(x, base_period = start_period(x) - 1, scale = 100) {
  return(rel2index(x/100, base_period = base_period, scale = scale))
}



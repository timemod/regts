#' Calculate an index timeseries from a timeseries with relative or percentage changes.
#'
#' A timeseries of relative changes \eqn{x_t} is related to the original
#' timeseries \eqn{X_t} by the relation \eqn{x_t = (X_t - X_{t-1}) / | X_{t-1}|}.
#' Function \code{rel2index} computes the index series
#' \eqn{\tilde{X}_t = s X_t / X_{t^*}}, where \eqn{s} is the scale and
#' \eqn{t^*} is the base period
#'
#' @section Functions:
#' \itemize{
#' \item \code{rel2index}: Calculates an index timeseries from a timeseries with
#' relative changes
#' \item \code{pct2index}: Calculates an index timeseries from a timeseries with
#' percentage changes
#' }
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}}
#' @param base_period base period of the index timeseries (a \code{\link{period}}
#' object or an object that can be coerced to a \code{period} object)
#' @param scale the value of the index series at the base period

#' @examples
#' ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
#' print(rel2index(ts1))
#' print(rel2index(ts1, base_period = "2010Q3", scale = 1))
#' @name pct2index/rel2index
NULL
#'
#' @rdname pct2index/rel2index
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

#' @rdname pct2index/rel2index
#' @export
pct2index <- function(x, base_period = start_period(x) - 1, scale = 100) {
  return(rel2index(x/100, base_period = base_period, scale = scale))
}



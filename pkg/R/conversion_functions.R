# This file contains several conversion functions for timeseries,
# for example for a conversion from a relative growth series to an index series

#' Calculates an index timeseries from a timeseries with relative mutations
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}}
#' @param base_period base period of the index timeseries
#' @param scale the value of the index series at the base period
#' @export
rel2index <- function(x, base_period = start_period(x) - 1, scale = 100) {
  x <- as.regts(x)
  cum_mat <- apply(1 + x, MARGIN = 2, FUN = cumprod)
  ret <- regts(rbind(rep(1, ncol(cum_mat)), cum_mat),
               start = start_period(x) - 1)

  if (missing(base_period)) {
    return(scale * ret)
  } else {
    stop("argument base_period not yet supported")
    base_period <- as.period(base_period)
    fac <- scale / ret[base_period, , drop = FALSE]
    ret <- ret * fac
    return(ret * fac)
  }
}



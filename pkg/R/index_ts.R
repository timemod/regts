#' Construct an index timeseries by scaling
#'
#' This function scales a timeseries by dividing all observations with
#' one selected observation or by the mean of a range of observations.
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param base_period The base period: a \code{\link{period}} or a
#' \code{\link{period_range}}, or an object that can be coerced to a period or
#' period range. By default base period is the first period of the input
#' timeseries.
#' @param  index_value the index value (by default 100). If \code{base_period}\
#' is a single period, then the returned timeseries will be equal to the index value
#' at this period. If \code{base_period} is a period
#' range with multiple periods, then the average of the index timeseries
#' is equal to the index value
#' @return  an index timeseries
#' @export
index_ts <- function(x, base_period = start_period(get_period_range(x)),
                     index_value = 100) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  base_period <- as.period_range(base_period)
  if (!missing(base_period)) {
    freq_base <- frequency(base_period)
    freq_x <- frequency(x)
    if (freq_base != freq_x) {
      if (freq_base > freq_x) {
        stop(paste("Argument base_period should not have a higher frequency",
                   "than the input timeseries"))
      }
      base_period <- change_frequency(base_period, new_frequency = freq_x)
    }
  }

  p_x <- get_period_range(x)
  startp_x <- start_period(p_x)
  endp_x <- end_period(p_x)

  startp_base <- start_period(base_period)
  endp_base <- end_period(base_period)

  if (startp_base < startp_x || endp_base > endp_x) {
    # TODO: improve error message
    stop(paste0("Base period (", base_period, " ) not within timeseries period",
                " (",  p_x, ")"))
  }

  if (is.mts(x)) {
    # multivariate timeseries
    i1 <- startp_base - startp_x + 1
    i2 <- endp_base - startp_x+ 1
    x[] <-  apply(x, FUN = function(x) {index_value * x / mean(x[i1:i2])},
                  MARGIN = 2)
    return(x)
  } else {
    # univariate timeseries
    return(index_value * x / mean(x[base_period]))
  }
}

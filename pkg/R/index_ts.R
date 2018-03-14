#' Construct an index timeseries by scaling
#'
#' This function scales a timeseries by dividing all observations by
#' one selected observation or by the mean of a range of observations.
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param base a \code{\link{period}} or a
#' \code{\link{period_range}} specifying the base period
#' used for calculating the index factor,
#' or an object that can be coerced to a
#' \code{period} or \code{period_range}. By default the
#' base period is the first period of the input timeseries.
#' @param  index_value the index value (by default 100).
#' This is the value of the index timeseries at the base period.
#' If the base period is a \code{period_range} spanning multiple
#' periods, then \code{index_value} is the average of the index timeseries
#' at the base period.
#' @examples
#' \dontshow{
#' set.seed(123)
#' }
#' ab <- regts(matrix(rnorm(18), ncol = 2), start = "2016Q1",
#'             names = c("a", "b"))
#'
#' index_ts(ab)
#'
#' index_ts(ab, base = "2017", index_value = 1)
#'
#' @export
index_ts <- function(x, base = start_period(x), index_value = 100) {

  if (!is.ts(x)) {
    stop("Argument x is not a timeseries")
  }

  if (!missing(base)) {
    base <- as.period_range(base)
    freq_base <- frequency(base)
    freq_x <- frequency(x)
    if (freq_base != freq_x) {
      if (freq_base > freq_x) {
        stop(paste0("Base period (", base, ") should not have a higher",
                    " frequency than the input timeseries (", freq_x, ")"))
      }
      base <- change_frequency(base, new_frequency = freq_x)
    }
  } else {
    base <- period_range(base, base)
  }

  p_x <- get_period_range(x)
  startp_x <- p_x[1]
  endp_x <- p_x[2]

  startp_base <- base[1]
  endp_base <- base[2]
  if (is.na(startp_base) || is.na(endp_base)) {
    stop("The base period should have a lower and upper bound")
  }

  if (startp_base < startp_x || endp_base > endp_x) {
    stop(paste0("Base period (", base, ") not within timeseries period",
                " (",  p_x, ")"))
  }

  psel <- (startp_base : endp_base) - startp_x + 1

  if (is.mts(x)) {
    # multivariate timeseries
    x[] <- apply(x, FUN = function(x) {x / mean(x[psel])}, MARGIN = 2)
    return(x * index_value)
  } else {
    # univariate timeseries
    return(index_value * x / mean(x[psel]))
  }
}

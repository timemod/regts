#' Construct an index timeseries by scaling
#'
#' This function scales a timeseries by dividing all observations by
#' one selected observation or by the mean of a range of observations.
#' The index series is calculated with \code{i[t] = s * X[t] / mean(X[base])},
#' where \code{s} is an arbitrary scale and \code{base} an arbitrary base period.
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param base a \code{\link{period}} or a
#' \code{\link{period_range}} specifying the base period or an object that can
#' be coerced to a \code{period} or \code{period_range}.
#' By default the base period is the first period of the input timeseries.
#' @param scale the (average) value of the index series at the base period
#' (by default 100).
#'
#' @seealso \code{\link{rel2index}} and \code{\link{pct2index}}
#' @examples
#' \dontshow{
#' set.seed(123)
#' }
#' ab <- regts(matrix(rnorm(18), ncol = 2), start = "2016Q1",
#'             names = c("a", "b"))
#'
#' index_ts(ab)
#'
#' index_ts(ab, base = "2017", scale = 1)
#'
#' @export
index_ts <- function(x, base = start_period(x), scale = 100) {

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
    return(scale * x)
  } else {
    # univariate timeseries
    return(scale * x / mean(x[psel]))
  }
}

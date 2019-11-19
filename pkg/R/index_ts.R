#' Construct an index timeseries by scaling
#'
#' This function scales a timeseries by dividing all observations by
#' one selected observation or by the mean of a range of observations.
#' The index series `i` is calculated from the input series `x` as
#'  ```
#'  i[t] = scale * x[t] / mean(X[base]),
#'  ```
#' where `scale` is usually 100 and `base` the base period, which can be
#' a single `period` or a `period_range` (by default the base period is the
#' first period of `x`). The function gives an error when the
#' average value of `mean(X[base])` is negative.
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param base a \code{\link{period}} or a
#' \code{\link{period_range}} specifying the base period, or an object that can
#' be coerced to a \code{period} or \code{period_range}.
#' By default the base period is the first period of the input timeseries.
#' @param scale the (average) value of the index series at the base period
#' (by default 100). This should be a positive number.
#'
#' @seealso \code{\link{rel2index}} and \code{\link{pct2index}}
#' @examples
#' \dontshow{
#' set.seed(123)
#' }
#' ab <- regts(matrix(1:18 + rnorm(18), ncol = 2), start = "2016Q1",
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

  if (!missing(scale)) {
    if (!is.numeric(scale) || is.na(scale[1]) || scale[1] < 0) {
      stop("Argument scale must be a positive number.")
    }
    if (length(scale) > 1) stop("Argument scale should be a single number.")
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

    xdat <- x[psel, , drop = FALSE]
    means <- as.numeric(colMeans(xdat))
    cnames <- colnames(x)
    if (is.null(cnames)) cnames <- seq_len(ncol(x))
    if (any(na_sel <- is.na(means))) {
      # result series will be NA or NaN
      na_cols <- cnames[na_sel]
      warning(paste0("Input timeseries contains NA values in base period ",
                     base, " for columns: ", paste(na_cols, collapse = ", "),
                     "."))
    }
    if (any(zero_sel <- !is.na(means) & means == 0)) {
      # result series will be Inf or -Inf
      zero_cols <- cnames[zero_sel]
      warning(paste0("Input timeseries has zero (average) value at base period ",
                     base, " for columns: ", paste(zero_cols, collapse = ", "),
                     "."))
    }
    if (any(neg_sel <- !is.na(means) & means < 0)) {
      # error: if the value in the base period is negative, it is not
      # possible to construct a meaningful index series
      neg_cols <- cnames[neg_sel]
      stop(paste0("Input timeseries has negative (average) value at base",
                  " period ", base, " for columns: ",
                  paste(neg_cols, collapse = ", "), "."))
    }
    x[] <- x * rep(1 / means, each = nrow(x))
    return(scale * x)
  } else {
    # univariate timeseries
    m  <- mean(x[psel])
    if (is.na(m)) {
      warning(sprintf("Input timeseries contains NA values in base period %s.",
                     as.character(base)))
    } else if (m == 0) {
      warning(sprintf(paste("Input timeseries has zero (average) value at base",
                            "period %s."), as.character(base)))
    } else if (m < 0) {
      stop(sprintf(paste("Input timeseries has negative (average) value at",
                         "base period %s."), as.character(base)))
    }
    return(scale * x / m)
  }
}

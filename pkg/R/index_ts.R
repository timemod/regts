#' Construct an index timeseries by scaling
#'
#' This function scales a timeseries by dividing all observations by
#' one selected observation or by the mean of a range of observations.
#' The index series `i` is calculated from the input series `x` as
#'  ```
#'  i[t] = scale * x[t] / mean(x[base]),
#'  ```
#' where `scale` is usually 100 and `base` the base period, which can be
#' a single `period` or a `period_range` (by default the base period is the
#' first period of `x`).
#' If  `mean(x[base])` is negative a warning is given and the (mean) value
#' of the resulting index series at the  base period will be `-scale`.
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
index_ts <- function(x, base = NULL, scale = 100) {
  series_name <- deparse(substitute(x))
  x <- as.regts(x)

  # check scale argument. Remove attributes (for example time series attributes):
   if (!missing(scale)) {
    if (!is.numeric(scale) || is.na(scale[1]) || scale[1] < 0) {
      stop("Argument scale must be a positive number.")
    }
     # remove any attrbitues (timeseries attributes etc.)
    scale <- as.numeric(scale)
    if (length(scale) > 1) stop("Argument scale should be a single number.")
  }


  return(index_ts_internal(x, base, scale, series_name, check_negative = TRUE))
}

# Internal function to compute an index timeseries.
# If check_negative == TRUE, a warning is given if the (average) value of x
# in the base period is negative, and the result is set such that the
# result at the base period is -100.
index_ts_internal <- function(x, base, scale, series_name, check_negative) {

  if (!is.null(base)) {
    base <- check_base(base, x)
  } else {
    base <- as.period_range(start_period(x))
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

  if (is.matrix(x) && (ncol(x) > 1) || !is.null(colnames(x))) {
    # multivariate timeseries

    xdat <- x[psel, , drop = FALSE]
    means <- as.numeric(colMeans(xdat))
    cnames <- colnames(x)
    if (is.null(cnames)) cnames <- seq_len(ncol(x))

    if (check_negative && any(neg_sel <- !is.na(means) & means < 0)) {
      # Give a warning if the value at the base period is negative,
      # and multiply the result with -1.
      means[neg_sel] <- -means[neg_sel]
      neg_cols <- cnames[neg_sel]
      warning(paste0("Negative (average) value at base",
                     " period ", base, " for columns: ",
                     paste(neg_cols, collapse = ", "), "."))
    }

    if (any(na_sel <- is.na(means))) {
      # result series will be NA or NaN
      na_cols <- cnames[na_sel]
      warning(paste0("NA values in base period ",
                     base, " for columns: ", paste(na_cols, collapse = ", "),
                     "."))
    }

    if (any(zero_sel <- !is.na(means) & means == 0)) {
      # result series will be Inf or -Inf
      zero_cols <- cnames[zero_sel]
      warning(paste0("Zero (average) value at base period ",
                     base, " for columns: ", paste(zero_cols, collapse = ", "),
                     "."))
    }
    x[] <- x * rep(1 / means, each = nrow(x))
    return(scale * x)
  } else {
    # univariate timeseries
    m  <- mean(x[psel])
    if (is.na(m)) {
      warning(sprintf(paste("NA values in base period %s for variable %s."),
                      as.character(base), series_name))
    } else if (m == 0) {
      warning(sprintf(paste("Zero (average) value at base period %s for variable %s."),
                      as.character(base), series_name))
    } else if (check_negative && m < 0) {
      warning(sprintf(paste("Negative (average) value at base period %s for variable %s."),
                      as.character(base), series_name))
      m <- -m
    }
    return(scale * x / m)
  }
}

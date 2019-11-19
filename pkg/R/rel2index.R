#' Calculate an index timeseries from a timeseries with relative or
#' percentage changes.
#'
#' Function `rel2index` is the inverse of function \code{\link{growth}}.
#' The growth `g[t]` (also called the relative change) of a timeseries `z[t]`
#' is defined as
#' ```
#' g[t] = (z[t] - z[t - 1]) / |z[t - 1]|.
#' ```
#' The function constructs an index series for `z[t]` given the values of
#' `g[t]`, assuming that the value of timeseries `z` at the
#' period before the start period of timeseries `g` (specified with
#' argument `x`) is positive. See Details.
#' \cr\cr
#' Function `pct2index` computes the index series
#' from a timeseries of percentage changes, defined as `100 g[t]`.
#' Thus expression `pct2index(x)` gives the same result as `rel2index(x / 100)`.
#'
#'
#' If `g[t]` is given but `z[t]` is unknown, we can compute
#' `z[t]` as
#' ```
#' z[t] = z[t - 1] * (1 + sign(z[t - 1]) g[t]).
#' ```
#' Given an initial value for `z` at some period (say `z[0]`), the equation
#' above can be used repeatedly to calculate the values of `z[t]` for `t > 0`.
#' The magnitude of `z[0]` is, however, irrelevant if we construct an
#' index series
#' ```
#' i[t] =  scale * z[t] / z[t*],
#' ```
#' where `t*` is the base period. If we assume that `z[0]` is positive,
#' we can simplfy set it to 1. If this assumption is not correct and
#' `z[0]` is actually negative, then `rel2index` does not give correct results.
#'
#' @param x  a \code{\link[stats]{ts}} or \code{\link{regts}} (can also be a
#' multivariate timeseries) with the relative of percentage changes.
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
  new_start <- start_period(x) - 1
  new_end <- end_period(x)

  if (!missing(base)) {
    base <- as.period(base)
    if (!identical(base, new_start)) {
      data <- rel2index_rebase(data, base, new_start, new_end, x_is_matrix,
                               colnames(x))
    }
  }

  if (scale != 1) data <- data * scale

  if (keep_range) {
    # remove first row
    data <- data[-1, , drop = FALSE]
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
  return(rel2index(x / 100, base = base, scale = scale,
                   keep_range = keep_range))
}


# rebase the calculated index series: the index series had a base period
# at new_start, but should now have  a base_period at base.
rel2index_rebase <- function(data, base, new_start, new_end, x_is_matrix,
                             cnames) {

  if (frequency(base) != frequency(new_start)) {
    stop(paste0("The base period ", base, " has a different frequency",
                " than the timeseries (", frequency(new_start), ")."))
  }
  base_index <- base - new_start + 1
  if (base_index < 1 || base_index > nrow(data)) {
    stop(paste0("The base period should lie between ",
                new_start, " and ", new_end, "."))
  }

  factors <- data[base_index, ]
  if (is.null(cnames)) cnames <- seq_len(ncol(data))
  if (any(na_sel <- is.na(factors))) {
    # WARNING: result timeseries will be NA
    if (x_is_matrix) {
      neg_cols <- cnames[na_sel]
      warning(paste0("Cumulated timeseries has NA value at base",
                  " period ", base, " for columns: ",
                  paste(neg_cols, collapse = ", "), "."))
    } else {
      warning(sprintf(paste("Cumulated timeseries has NA value at",
                         "base period %s."), as.character(base)))
    }
  }
  if (any(zero_sel <- !is.na(factors) & factors == 0)) {
    # WARNING: result timeseries with be Inf or -Inf
    if (x_is_matrix) {
      zero_cols <- cnames[zero_sel]
      warning(paste0("Cumulated timeseries has zero value at base",
                  " period ", base, " for columns: ",
                  paste(neg_cols, collapse = ", "), "."))
    } else {
      warning(sprintf(paste("Cumulated timeseries has zero value at",
                         "base period %s."), as.character(base)))
    }
  }
  if (any(neg_sel <- !is.na(factors) & factors < 0)) {
    # ERROR: if the value in the base period is negative, it is not
    # possible to construct a meaningful index series
    if (x_is_matrix) {
      neg_cols <- cnames[neg_sel]
      stop(paste0("Cumulated timeseries has negative value at base",
                  " period ", base, " for columns: ",
                  paste(neg_cols, collapse = ", "), "."))
    } else {
      stop(sprintf(paste("Cumulated timeseries has negative value at",
                         "base period %s."), as.character(base)))
    }
  }
  return(data * rep(1 / factors, each = nrow(data)))
}

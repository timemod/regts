# The S3 method window.ts removes the regts class, therefore use as.regts.
#' @importFrom stats window
#' @export
window.regts <- function(x, ...) {
  ret <- as.regts(NextMethod(.Generic))
  ts_labels(ret) <- ts_labels(x)
  return (ret)
}

# The S3 method diff.ts removes the regts class, therefore use as.regts.
#' @export
diff.regts <- function(x, ...) {
  ret <- as.regts(NextMethod(.Generic))
  ts_labels(ret) <- ts_labels(x)
  return (ret)
}

# The S3 method aggregate.ts removes the regts class, therefore call  as.regts.
# Also takes care of weird result if the first period of x does not
# start at a subperiod for the new frequency (e.g.  a quartly timeseries
# that is aggregated to a yearly timeseries starts at 2010.2q)).
# In that case the initial period must be shifted
#' @importFrom stats aggregate
#' @importFrom stats aggregate.ts
#' @export
aggregate.regts <- function(x, nfrequency = 1, ...) {
  rep <- frequency(x) / nfrequency
  p1 <- start_period(x)
  extra <- as.integer(p1) %% rep
  if (extra != 0) {
    # shift initial period
    p1 <- p1 + rep - extra
    x <- window_regts(x, period_range(p1, NULL))
  }

  ret <- as.regts(aggregate.ts(x, nfrequency, ...))

  if (is.matrix(x) && is.null(colnames(x))) {
    # aggregate.ts create colnames Series 1, Series 2 etc. if x does not
    # have colnames and if x is a matrix. We do not want that.
    colnames(ret) <- NULL
  }

  ts_labels(ret) <- ts_labels(x)
  return (ret)
}

#' @export
t.regts <- function(x) {
  # ts.default is used for ts and matrix objects.
  # ts.default always returns a matrix.
  # However, the regts class is not removed from the list
  # of classes. Therefore we have to first unregts the regts object.
  x <- unregts(x)
  return(NextMethod(.Generic))
}

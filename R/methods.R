# The S3 method window.ts removes the regts class, therefore use as.regts.
#' @importFrom stats window
#' @export
window.regts <- function(x, ...) {
    lbls <- ts_labels(x)
    x <- as.regts(NextMethod(.Generic))
    if (!is.null(lbls)) {
        ts_labels(x) <- lbls
    }
    return (x)
}

# The S3 method diff.ts removes the regts class, therefore use as.regts.
#' @export
diff.regts <- function(x, ...) {
    lbls <- ts_labels(x)
    x <- as.regts(NextMethod(.Generic))
    if (!is.null(lbls)) {
        ts_labels(x) <- lbls
    }
    return (x)
}

# The S3 method aggregate.ts removes the regts class, therefore call  as.regts.
# Also takes care of weird result if the first period of x does not
# start at a subperiod for the new frequency (e.g.  a quartly timeseries
# that is aggregated to a yearly timeseries starts at 2010.2q)).
# In that case the initial period must be shifted
#' @importFrom stats aggregate
#' @export
aggregate.regts <- function(x, nfrequency = 1, ...) {
    lbls <- ts_labels(x)
    rep <- frequency(x) / nfrequency
    p1 <- start_period(x)
    extra <- as.integer(p1) %% rep
    if (extra != 0) {
        # shift initial period
        p1 <- p1 + rep - extra
        x <- window_regts(x, regperiod_range(p1, NULL))
    }
    ret <- as.regts(NextMethod(.Generic))

    if (is.null(colnames(x))) {
        # aggregate.ts create colnames Series 1, Series 2 etc. if x does not
        # have colnames. We do not want that.
        colnames(ret) <- NULL
    }
    if (!is.null(lbls)) {
        ts_labels(ret) <- lbls
    }
    return (ret)
}

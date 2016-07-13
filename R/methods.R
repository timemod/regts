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

# Returns the timeseries label from an arbitrary object.
# Only regts objects have labels
get_labels <- function(x) {
    if (is.regts(x)) {
        return (ts_labels(x))
    } else {
        nc <- ncol(x)
        if (!is.null(nc)) {
            return (rep("", nc))
        } else {
            return ("")
        }
    }
}

# Check if the arguments in ... contain any label, and if so than add
# all labels to x
handle_labels <- function(x, ...) {
    arguments <- list(...)
    # Check if there are any labels
    has_labels <- any(unlist(lapply(arguments,
                                    FUN = function(x) !is.null(ts_labels(x)))))
    if (has_labels) {
        labels <- unname(unlist(lapply(arguments, FUN = get_labels)))
        ts_labels(x) <- labels
    }
    return(x)
}

#' Bind Two or More Timeseries
#'
#' Bind time series which have a common frequency using the methods
#' \code{\link[stats]{ts.intersect}} and \code{\link[stats]{ts.union}} of the
#' \code{stats} package. The implementation for \code{regts} converts the value
#' returned by \code{ts.intersect} and \code{ts.union}
#' to a \code{regts} and also takes care of the timeseries labels
#' (if present). The \code{cbind} methods for \code{regts} objects works
#' as \code{regts.intersect}, except that the argument \code{dframe} is
#' not used.
#' @importFrom stats ts.intersect
#' @importFrom stats ts.union
#' @param  ... two or more univariate or multivariate time series,
#' or objects which can coerced to time series
#' @param dframe logical if \code{TRUE} return the result as a data frame
#' @return  A \code{regts} timeseries object is \code{dframe} is
#' \code{FALSE}, otherwise a data frame.
#' @export
regts.intersect <- function(..., dframe = FALSE) {
    ret <- ts.intersect(..., dframe = dframe)
    if (!dframe) {
        ret <- handle_labels(as.regts(ret), ...)
    }
    return (ret)
}

#' @rdname regts.intersect
#' @export
regts.union <- function(..., dframe = FALSE) {
    ret <- ts.union(..., dframe = dframe)
    if (!dframe) {
        ret <- handle_labels(as.regts(ret), ...)
    }
    return (ret)
}

#' @rdname regts.intersect
#' @export
cbind.regts <- function(...) {
    return (regts.intersect(...))
}

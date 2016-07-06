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
    x <- as.regts(NextMethod(.Generic))
    if (!is.null(lbls)) {
        ts_labels(x) <- lbls
    }
    return (x)
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

#' @importFrom stats ts.union
#' @export
regts.union <- function(...) {
    ret <- as.regts(ts.union(...))
    ret <- handle_labels(ret, ...)
    return (ret)
}

#' @importFrom stats ts.intersect
#' @export
regts.intersect <- function(...) {
    ret <- as.regts(ts.intersect(...))
    ret <- handle_labels(ret, ...)
    return (ret)
}

#' @export
cbind.regts <- function(...) {
    return (regts.intersect(...))
}

#' @export
Ops.regts <- function(e1, e2) {
    return (as.regts(NextMethod(.Generic)))
}

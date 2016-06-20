# The S3 method window.ts removes the regts class, therefore use as.regts.
#' @export
window.regts <- function(x, ...) {
    return (as.regts(NextMethod(.Generic)))
}

# The S3 method aggregate.ts removes the regts class, therefore call  as.regts.
# Also takes care of weird result if the first period of x does not
# start at a subperiod for the new frequency (e.g.  a quartly timeseries
# that is aggregated to a yearly timeseries starts at 2010.2q)).
# In that case the initial period must be shifted.
#' @export
aggregate.regts <- function(x, nfrequency = 1, ...) {
    rep <- frequency(x) / nfrequency
    p1 <- start_period(x)
    extra <- as.integer(p1) %% rep
    if (extra != 0) {
        # shift initial period
        p1 <- p1 + rep - extra
        x <- x[regperiod_range(p1, NULL), ]
    }
    return (as.regts(NextMethod(.Generic)))
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

#' regts.union
#'
#' @export
regts.union <- function(...) {
    ret <- as.regts(ts.union(...))
    ret <- handle_labels(ret, ...)
    return (ret)
}

#' regts.intersect
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
Ops.regts <- function(x, y) {
    return (as.regts(NextMethod(.Generic)))
}

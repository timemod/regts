# The S3 method window.ts removes the regts class, therefore use as.regts.
#' @export
window.regts <- function(x, ...) {
    return (as.regts(NextMethod(.Generic)))
}

# The S3 method aggregate.ts removes the regts class, therefore as as.regts.
#' @export
aggregate.regts <- function(x, ...) {
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
    # we cannot simply use .Generic, because the method dispatching is not
    # done via UserMethod(), but by C-internal patching. Therefore use the
    # following tric:
    arguments <- list(...)
    names(arguments) <- eval(substitute(alist(...)))

    # convert regts object to normal objects
    f <- function(x) {
       if (is.regts(x)) {
           return (remove_regts_class(x))
       } else {
            return (x)
       }
     }
     arguments <- lapply(arguments, FUN = f)
     ret <- as.regts(do.call(cbind, arguments))
     ret <- handle_labels(ret, ...)
}

#' @export
Ops.regts <- function(x, y) {
    return (as.regts(NextMethod(.Generic)))
}

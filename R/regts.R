#' @export
regts <- function(data, start, end = NULL, frequency = NA, ...) {
    start <- as.regperiod(start, frequency)
    if (missing(end)) {
        retval <- ts(data, start = start$data, frequency = start$freq, ...)
    } else {
        end <- as.regperiod(end, frequency)
        if (end$freq != start$freq) {
            stop(paste("Frequency start", start$freq, "and end",
                       end$freq, "do not agree"))
        }
        retval <- ts(data, start = start$data, end = end$data,
                     frequency = start$freq, ...)
    }
    return (as.regts(retval))
}

#' @export
is.regts <- function(x) {
    return (inherits(x, "regts"))
}

#' @export
as.regts <- function(x, ...) {
    UseMethod("as.regts")
}

#' @export
as.regts.ts <- function(x, ...) {
    if (!is.regts(x)) {
        old_classes <- class(x)
        class(x) <- c("regts", old_classes)
    }
    return (x)
}

#' @export
as.regts.default <- function(x, ...) {
   stop(paste("as.regts is not implemented for class type", class(x)))
}

# Add columns with names new_colnames to x, and fill with NA.
add_columns <- function(x, new_colnames) {

    new_columns <- ts(matrix(NA, ncol = length(new_colnames)),
                      start = start(x), end = end(x),
                      frequency = frequency(x), names = new_colnames,
                      class = c("mts", "ts", "matrix"))

    old_colnames <- colnames(x)

    # call ts.intersect with normal ts object, otherwise the
    # problems may occur
    x <- ts.intersect(x, new_columns)
    colnames(x) <- c(old_colnames, new_colnames)
    return (as.regts(x))
}

#' @export
"[<-.regts" <- function (x, i, j, value) {

    if (!missing(j) && is.character(j)) {
        # Check if j contains names of columns not present in x.
        # Add missing columns if necessary
        new_colnames <- setdiff(j, colnames(x))
        if (length(new_colnames) > 0) {
            x <- add_columns(x, new_colnames)
        }
    }

    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        i <- as.regperiod_range(i)
        i <- modify_frequency(i, frequency(x))
        suppressWarnings({
            # suppress warnings about extending  timeseries with NA values
            if (missing(j)) {
                window(x, start = i$start, end = i$end, extend = TRUE) <- value
            } else {
                window(x, start = i$start, end = i$end, extend = TRUE)[, j] <-
                                                          value
            }
        })
        return (as.regts(x))
    } else {
        return (NextMethod("[<-"))
    }
}

#' @export
"[.regts" <- function(x, i, j, drop = FALSE) {
    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        i <- as.regperiod_range(i)
        i <- modify_frequency(i, frequency(x))
        if (missing(j)) {
            x <- window(x, start = i$start, end = i$end, extend = TRUE)
        } else {
            x <- window(x, start = i$start, end = i$end, extend = TRUE)[, j]
        }
        return (as.regts(x))
    } else {
        x <- NextMethod("[")
        if (is.ts(x)) {
            return (as.regts(x))
        } else {
            return (x)
        }
    }
}

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

# This function computes the row numbers of the selected rows in a
# regts object. If the period selector lies partially outside
# the defintion period of the input timeseries, then the function
# returns NULL.
# PARAMETERS:
# x  : a regts object
# y  : a regperiod_range object. This object is used
#      to select rows in argument x.
# RETURNS: a vector with the numbers of the select rows in x,
#          or NULL if y lies partially outside the
#          definition period of x.
get_row_selection <- function(x, y) {
    ts_per <- get_regperiod_range(x)
    ts_per_start <- get_start_period(ts_per)
    ts_per_end  <- get_end_period(ts_per)
    ts_per_len <- ts_per_end - ts_per_start + 1  # TODO: create special function
    if (is.null(y$start)) {
        p_start <- ts_per_start
    } else {
        p_start <- get_start_period(y)
    }
    if (is.null(y$end)) {
        p_end <- ts_per_end
    } else {
        p_end <- get_end_period(y)
    }

    index1 <- p_start - ts_per_start + 1
    index2 <- p_end - ts_per_start + 1
    if (index1 > index2) {
        stop(paste("Start period", p_start, "before end period", p_end))
    }
    extend <- index1 < 1 || index2 > ts_per_len
    if (!extend) {
        return (index1 : index2)
    } else {
        return (NULL)
    }
}

# Returns TRUE if regperiod_range y lies partially outside the
# definition period of timeseries x. This means that the timeseries
# would be extended if the period selection is applied.
check_extend <- function(x, y) {
    return (is.null(get_row_selection(x, y)))
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
        row_numbers <- get_row_selection(x, i)
        extend <- is.null(row_numbers)
        if (extend) {
            # The window function of ts is quite slow when applied to the
            # lhs of a statement, especially if extend = TRUE.
            # Therefore only use window if extend = TRUE, use the
            # row number selection if extend = FALSE.
            suppressWarnings({
                # suppress warnings about extending  timeseries with NA values
                if (missing(j)) {
                    window(x, start = i$start, end = i$end, extend = TRUE) <- value
                } else {
                    window(x, start = i$start, end = i$end, extend = TRUE)[, j] <- value
                }
            })
        } else {
            i <- row_numbers
            x <- NextMethod("[<-")
        }
        return (x)
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
        extend <- check_extend(x, i)
        # The window function of ts is quite slow if extend = TRUE.
        # Therefore only use extend if it is really necessary
        if (missing(j)) {
            x <- window(x, start = i$start, end = i$end, extend = extend)
        } else {
            x <- window(x, start = i$start, end = i$end, extend = extend)[, j]
        }
        return (x)
    } else {
        x <- NextMethod("[")
        if (is.ts(x)) {
            return (as.regts(x))
        } else {
            return (x)
        }
    }
}

#' Returns the period range of the time series as a \code{regperiod_range} object
#'
#' @param x a \code{regts}
#' @return a \code{regperiod_range}
#' @export
get_regperiod_range <- function(x) {
    if (!is.regts(x)) {
        stop("Argument x is not a regts")
    }
    return (structure(list(start = start(x), end = end(x), freq = frequency(x)),
                      class="regperiod_range"))
}

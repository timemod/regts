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
                      frequency = frequency(x))
    old_colnames <- colnames(x)
    x <- cbind(x, new_columns)
    colnames(x) <- c(old_colnames, new_colnames)
    return (as.regts(x))
}

# This function makes sure that regperiod_range object range has
# the same frequency as timeseries x, and also replaces
# NULL values for range$start or range$end by the appropriate
# values from timeseries x.
# param range a regperiod_range object used as selector of a timeseries
# param x  a timeseries object (regts or ts)
convert_range_selector <- function(range, x) {

    range <- modify_frequency(range, frequency(x))

    # replace NULl periods by periods in x
    if (is.null(range$start)) {
        range$start <- start(x)
    }
    if (is.null(range$end)) {
        range$end <- end(x)
    }

    # Check if range is a valid selector of timeseries x.
    # An error will occur for example if timeseries x has period
    # 2010Q1/2011Q2 while range is 2012Q2/.
    p_start <- get_start_period(range)
    p_end   <- get_end_period(range)
    if (p_start > p_end) {
        # This
        stop(paste("Start period", p_start, "before end period", p_end))
    }
    return (range)
}

#' Adjust the period of the timeseries
#'
#' @param x a timeseries object (\code{ts} or \code{regts})
#' @param range the new period range of the timeseries
#' @return a \code{regts} with the adjusted period
#' @export
adjust_period <- function(x, range) {
    range <- convert_range_selector(as.regperiod_range(range), x)
    # TODO: if range within x, then use the window function,
    # this might be faster (or not? check this)
    return (.adjust_period(x, range))
}

.adjust_period <- function(x, range) {
    per_len <- get_end_period(range) - get_start_period(range) + 1
    retval <- regts(matrix(NA, nrow = per_len, ncol = ncol(x)),
                    start = get_start_period(range), names = colnames(x))
    p <- .regrange_intersect(get_regperiod_range(x), range)
    if (!is.null(p)) {
        retval[p, ] <- window(x, start = p$start, end = p$start)
    }
    return (retval)
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
    ts_per       <- get_regperiod_range(x)
    ts_per_start <- get_start_period(ts_per)
    ts_per_end   <- get_end_period(ts_per)
    ts_per_len   <- ts_per_end - ts_per_start + 1  # TODO: create function
    p_start      <- get_start_period(y)
    p_end        <- get_end_period(y)
    index1       <- p_start - ts_per_start + 1
    index2       <- p_end   - ts_per_start + 1
    extend       <- index1 < 1 || index2 > ts_per_len
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
        range <- convert_range_selector(as.regperiod_range(i), x)
        row_numbers <- get_row_selection(x, range)
        if (is.null(row_numbers)) {
            # Do not use the window function of ts to extend the timeseries,
            # it is very slow. Use our own function .adjust_period
            x <- .adjust_period(x, .regrange_union(range,
                                                   get_regperiod_range(x)))
            row_numbers <- get_row_selection(x, range)
        }
        i <- row_numbers
    }
    return (NextMethod("[<-"))
}

#' @export
"[.regts" <- function(x, i, j, drop = FALSE) {
    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        range <- convert_range_selector(as.regperiod_range(i), x)
        extend <- check_extend(x, range)
        # The window function of ts is quite slow if extend = TRUE.
        # Therefore only use extend if it is really necessary
        if (missing(j)) {
            x <- window(x, start = range$start, end = range$end,
                        extend = extend)
        } else {
            x <- window(x, start = range$start, end = range$end,
                        extend = extend)[, j]
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

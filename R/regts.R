#' Create a \code{regts} timeseries object
#'
#' The \code{regts} class is an extension of the \link{ts} class of the \link{stats} package.
#' Working with \code{regts} makes it easier to select periods. Another difference is
#' related to the representation of univariate timeseries.  A \code{regts} object
#' always has \link{colnames}, also for univariate timeseries. A univariate \code{ts} object
#' only has column names when the input data was a one-dimensional matrix.
#'
#'
#' @param data a vector or matrix of the observed time-series values. A data frame will be
#' coerced to a numeric matrix via \link{data.matrix}. (See also the description of the
#' function \link{ts} of the \link{stats} package).
#' @param start the starting period as a  \link{regperiod} object or a character string that can
#' be converted to a \code{regperiod} object
#' @param end the end period as a  \link{regperiod} object or a character string that can
#' be converted to a \code{regperiod} object. If not specified, then the end period is calculated
#' from the dimension of \code{data}
#' @param frequency the frequency of the timeseries. This argument should only be specified if
#' the start or end period is specified with a general period format without period indicator,
#' e.g. \code{"2011-3"}
#' @param names a character vector of names for the series: defaults to the colnames of data, or Series 1,
#' Series 2, .... if the data does not have colnames. In contrast to the function\link{ts}, this function
#' also uses the supplied name if the result is a single timeseries.
#' @return a \code{regts} object
#' @examples
#' # univariate timeseries
#' ts1 <- regts(1:10, start = "2010Q4")
#'
#' # multivariate timeseries
#' ts2 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"))
#'
#' # create a half-yearly timeseries
#' ts3 <- regts(1:10, start = "2010-1", end = '2011-2', frequency = 2)
#' @export
regts <- function(data, start, end = NULL, frequency = NA, names = NULL) {
    start <- as.regperiod(start, frequency)
    if (missing(end)) {
        retval <- ts(data, start = start$data, frequency = start$freq)
    } else {
        end <- as.regperiod(end, frequency)
        if (end$freq != start$freq) {
            stop(paste("Frequency start", start$freq, "and end",
                       end$freq, "do not agree"))
        }
        retval <- ts(data, start = start$data, end = end$data,
                     frequency = start$freq)
    }

    if (is.null(attr(retval, "dim"))) {
        # univariate timeseries wihout dimension attributes
        # add dimension attributes so that the timeseries has a column name.
        if (is.null(names)) {
            name <- "Series 1"
        } else {
            name <- names
        }
        retval <- insert_dim_attr(retval, name)
    } else if (!is.null(names)) {
        colnames(retval) <- names
    } else if (!is.null(colnames(data))) {
        colnames(retval) <- colnames(data)
    }
    return (insert_regts_class(retval))
}

# insert "regts" at the beginning of the classes vector
insert_regts_class <- function(x) {
    if (!is.regts(x)) {
        old_classes <- class(x)
        class(x) <- c("regts", old_classes)
    }
    return(x)
}

# add dimension attribute for univariate timeseriss
insert_dim_attr <- function(x, names) {
    dim_attr <- list(dim = c(length(x), 1), dimnames = list(NULL, names))
    attributes(x) <- c(dim_attr, attributes(x))
    return(x)
}

#' Test wether an object is a \link{regts} timeseries object
#' @param x an arbitrary R object
#' @return \code{TRUE} if \code{x} isa  \link{regts} object
#'
#' @export
is.regts <- function(x) {
    return (inherits(x, "regts"))
}

#' Coerce an object to a \link{regts} timeseries object
#' @param x an arbitrary R object
#' @return a \link{regts} object
#' @export
as.regts <- function(x) {
    UseMethod("as.regts")
}

##' @export
as.regts.ts <- function(x) {
    if (!is.regts(x)) {
        x <- insert_regts_class(x)
        if (is.null(attr(x, "dim"))) {
            # univariate timeseries wihout dimension attributes
            # add dimension attributes so that the timeseries has a column name.
            x <- insert_dim_attr(x, "Series 1")
        }
    }
    return (x)
}

##' @export
as.regts.default <- function(x) {
    return (as.regts(as.ts(x)))
}

# Add columns with names new_colnames to x, and fill with NA.
add_columns <- function(x, new_colnames) {
    new_columns <- ts(matrix(NA, ncol = length(new_colnames)),
                      start = start(x), end = end(x),
                      frequency = frequency(x))
    old_colnames <- colnames(x)
    x <- remove_regts_class(x)
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

# Adjust the period of the timeseries
#
# @param x a timeseries object (\code{ts} or \code{regts})
# @param range the new period range of the timeseries. This should
# be a regperiod_range object without NULL periods
# @return a regts with the adjusted period
.adjust_period <- function(x, range) {

    p_start <- get_start_period(range)
    p_end   <- get_end_period(range)
    per_len <- p_end - p_start + 1

    if (is.mts(x)) {
        ncolumns <- ncol(x)
    } else {
        ncolumns <- 1
    }
    retval <- regts(matrix(NA, nrow = per_len, ncol = ncolumns),
                    start = p_start, names = colnames(x))
    p <- .regrange_intersect(get_regperiod_range(x), range)
    if (!is.null(p)) {
        retval[p, ] <- window(x, start = p$start, end = p$end)
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
        # if argument j is missing, then we have to add an empty
        # column selection. x[i] does not return the same as x[i, ].
        if (missing(j) && is.mts(x)) {
            x[i, ] <- value
            return (x)
        }
    }
    return (NextMethod("[<-"))
}

#' @export
"[.regts" <- function(x, i, j) {
    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        range <- convert_range_selector(as.regperiod_range(i), x)
        extend <- check_extend(x, range)
        # The window function of ts is quite slow if extend = TRUE.
        # Therefore only use extend if it is really necessary
        if (missing(j)) {
            x <- window(x, start = range$start, end = range$end, extend = extend)
        } else {
            x <- window(x, start = range$start, end = range$end, extend = extend)[, j]
        }
        return (x)
    } else {
        x <- NextMethod("[", drop = FALSE)
        # x can be something else than a timeseries, for example an integer
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

# remove the regts class
remove_regts_class <- function(x) {
    old_classes <- class(x)
    class(x) <- old_classes[old_classes != "regts"]
    return (x)
}

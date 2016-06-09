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
#' @param names a character vector of names for the series: defaults to the
#' colnames of data, or \code{"Series 1"}, \code{"Series 2"} ..., if the data does
#' not have colnames. In contrast to the function \link{ts},  \code{names}
#' is also used if the result is a single timeseries.
#' @param labels a character vector of labels (descriptions of the timeseries)
#' @return a \code{regts} object
#' @examples
#' # univariate timeseries
#' ts1 <- regts(1:10, start = "2010Q4")
#'
#' # multivariate timeseries
#' ts2 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"))
#'
#'# multivariate timeseries with labels
#' ts3 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"),
#'              labels = paste("Timeseries", c("a", "b", "c")))
#'
#' # create a half-yearly timeseries, because ‘end’ is specified the length
#' # of the timeseries is smaller than the length of data (10).
#' ts4 <- regts(1:10, start = "2010-1", end = '2011-2', frequency = 2)
#' @seealso
#' The function \link{is.regts} can be used to test if an object is a \code{regts}.
#'
#' The S3 generic \link{as.regts} can be used to coerce an R object to a \code{regts}. There are currently
#' methods for \link{ts} and \link{data.frame}.
#'
#' \link{as.data.frame.regts} and \link{as.list.regts} can be used
#' to convert \code{regts} to a \link{data.frame} or a \link{list}.
#'
#' See also the description of the functions for handling labels
#' (\link{ts_labels} and \link{update_ts_labels}).
#'
#' @import evaluate
#' @export
regts <- function(data, start, end = NULL, frequency = NA, names = NULL,
                  labels = NULL) {
    start <- as.regperiod(start, frequency)
    start_freq <- frequency(start)
    if (missing(end)) {
        retval <- ts(data, start = c(get_year(start), get_subperiod(start)),
                     frequency = start_freq)
    } else {
        end <- as.regperiod(end, frequency)
        end_freq <- frequency(end)
        if (start_freq != end_freq) {
            stop(paste("Frequency start", start_freq, "and end",
                       end_freq, "do not agree"))
        }
        retval <- ts(data, start = c(get_year(start), get_subperiod(start)),
                     end = c(get_year(end), get_subperiod(end)),
                     frequency = start_freq)
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

    retval <- insert_regts_class(retval)

    if (!is.null(labels)) {
        ts_labels(retval) <- labels
    }
    return (retval)
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

#' Tests whether an object is a \link{regts} timeseries object
#' @param x an arbitrary R object
#' @return \code{TRUE} if \code{x} is a \link{regts}
#' @seealso
#' \link{regts} \link{as.regts}
#' @export
is.regts <- function(x) {
    return (inherits(x, "regts"))
}

#' Coerce an object to a \link{regts} timeseries object
#'
#' @param x an arbitrary R object
#' @param time_column the column names or numbers of the data frame
#' in which the time is stored. Specify \code{0} if the index is in the row names
#' of the data frame
#' @param fun a function for converting values in the time column to
#' \link{regperiod} objects
#' @param ... arguments passed to \code{fun}
#' @return a \link{regts} object
#' @seealso
#' \link{regts}, \link{is.regts}, \link{as.data.frame.regts}, \link{as.list.regts}
#' @examples
#' # convert a ts to regts
#' x <- ts(1:3, start = c(2015,3), frequency = 4)
#' x <- as.regts(x)
#'
#' # Now two examples for converting a data.frame
#'
#' # create a data frame with timeseries and with the
#' # time index in the rownames, and convert to a regts
#' df <- data.frame(a = 1:3)
#' rownames(df) <- c("2015Q3", "2015Q4", "2016Q1")
#' ts <- as.regts(df)
#'
#' # create a data frame with the time index in the first column and special
#' time format "2015 3" instead of "2015Q3", and convert to regts
#' df <- data.frame(periods = c("2015 3", "2015 4", "2016 1"),  a = 1:3)
#' ts <- as.regts(df, time_column = 1, frequency = 4)
#' @export
as.regts <- function(x, ...) {
    UseMethod("as.regts")
}

#' @describeIn as.regts Coerce a \link{ts} to a \link{regts}
#' @export
as.regts.ts <- function(x, ...) {
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

#' @describeIn as.regts Convert a \link{data.frame} to a \link{regts}
#' @import Hmisc
#' @export
as.regts.data.frame <- function(x, time_column = 0, fun = regperiod,
                                ...) {

    # extract time column(s) and data from the input data frame,
    # and convert to a matrix
    if (time_column == 0) {
        times <- rownames(x)
        data <- x
    } else {
        if (is.character(time_column)) {
            time_column <- which(colnames(x) %in% time_column)
        }
        times <- x[[time_column]]
        data <- x[-time_column]
    }
    data <- as.matrix(data)

    # convert the contents of the time column to a list of regperiods
    times <- lapply(as.character(times), FUN = fun, ...)

    # check that all frequencies are equal
    frequencies <- unlist(lapply(times, FUN = frequency))
    frequencies <- unique(frequencies)
    if (length(frequencies) > 1) {
        stop("The time column(s) contain different frequencies")
    } else {
        freq <- frequencies[1]
    }

    times <- unlist(times)
    if (identical(times, times[1]:times[nrow(data)])) {
        # normal regular timeseries, no missing periods and periods
        # are ordered synchronically
        ret <- regts(data, start = create_regperiod(times[1], freq))
    } else {
        # irregular timeseries in dataframe (missing periods or
        # unorderered time index)
        subp_min <- min(times)
        subp_max <- max(times)
        per_count <- subp_max - subp_min + 1
        pmin <- create_regperiod(subp_min, frequency = freq)
        ret <- regts(matrix(NA, nrow = per_count, ncol = ncol(data)),
                     start = pmin, names = colnames(data))
        rows <- times - subp_min +1
        ret[rows, ] <- data
    }

    # handle labels
    lbls <- label(x)
    if (!all(nchar(lbls) == 0)) {
        # remove the time column(s) from the labels
        if (time_column != 0) {
            lbls <- lbls[-time_column]
        }
        ts_labels(ret) <- lbls
    }

    return (ret)
}

#' @describeIn as.regts Default method to convert an R object to a \link{regts}.
#' This method #' first employs \link{at.ts} and then \link{as.regts.ts}
#' @export
as.regts.default <- function(x, ...) {
    return (as.regts(as.ts(x, ...)))
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

    freq <- frequency(x)

    # convert the frequency of the range if necessary
    range <- modify_frequency(range, freq)

    # replace NULl periods by periods in x
    if (is.null(range$start)) {
        start <- start(x)
        range$start <- as.integer(get_subperiod_count(start[1], start[2], freq))
    }
    if (is.null(range$end)) {
        end <- end(x)
        range$end <- as.integer(get_subperiod_count(end[1], end[2], freq))
    }

    # Check if range is a valid selector of timeseries x.
    # An error will occur for example if timeseries x has period
    # 2010Q1/2011Q2 while range is 2012Q2/.
    p_start <- get_start_period(range)
    p_end   <- get_end_period(range)
    if (p_start > p_end) {
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
        pstart <- get_start_period(p)
        pend   <- get_end_period(p)
        start  <- c(get_year(pstart), get_subperiod(pstart))
        end    <- c(get_year(pend), get_subperiod(pend))
        retval[p, ] <- window(x, start = start, end = end)
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
    lbls <- ts_labels(x)
    if (!is.null(lbls) && !missing(j)) {
        lbls <- lbls[j]
    }
    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        range <- convert_range_selector(as.regperiod_range(i), x)
        extend <- check_extend(x, range)
        # The window function of ts is quite slow if extend = TRUE.
        # Therefore only use extend if it is really necessary
        pstart <- get_start_period(range)
        pend   <- get_end_period(range)
        start <- c(get_year(pstart), get_subperiod(pstart))
        end   <- c(get_year(pend), get_subperiod(pend))
        if (missing(j)) {
            x <- window(x, start = start, end = end, extend = extend)
        } else {
            x <- window(x, start = start, end = end, extend = extend)[, j]
        }
        if (!is.null(lbls)) {
            ts_labels(x) <- lbls
        }
        return (x)
    } else {
        x <- NextMethod("[", drop = FALSE)
        # x can be something else than a timeseries, for example an integer
        if (is.ts(x)) {
            x <- as.regts(x)
            if (!is.null(lbls)) {
                ts_labels(x) <- lbls
            }
            return (x)
        } else {
            return (x)
        }
    }
}

#' Returns the period range of the time series as a link{regperiod_range} object
#'
#' @param x a \code{regts} or \code{ts}
#' @return a \code{regperiod_range}
#' @export
get_regperiod_range <- function(x) {
    if (!is.ts(x)) {
        stop("Argument x is not a regts or ts")
    }
    freq <- frequency(x)
    start <- start(x)
    end <- end(x)
    p1 <- get_subperiod_count(start[1], start[2], freq)
    p2 <- get_subperiod_count(end[1], end[2], freq)
    return (create_regperiod_range(p1, p2, freq))
}

# remove the regts class
remove_regts_class <- function(x) {
    old_classes <- class(x)
    class(x) <- old_classes[old_classes != "regts"]
    return (x)
}

#' Timeseries labels
#'
#'Retrieve or set labels for the timeseries. Timeseries labels
#'can be used to give a description of the contents of the timeseries.
#'The labels are stored in a named list: the names are the timeseries names
#'(column names), and the values the correpsonding label.
#'@param x a \link{regts}
#'@param value a character vector with the labels or \code{NULL}. The length
#'should be equal to the number of columns. Specify \code{NULL} to remove all labels.
#'@examples
#'ts <- regts(matrix(1:6, ncol = 2), start = "2016Q2", names = c("a", "b"))
#'ts_labels(ts) <- c("Timeseries a", "Timeseries b")
#'print(ts_labels(ts))
#'
#' # print the column names and labels as a nice data.frame
#' print(as.data.frame(ts_labels(ts)))
#' @describeIn ts_labels Retrieve timeseries labels
#' @seealso
#' \link{regts}, \link{update_ts_labels}
#' @export
ts_labels <- function(x) {
    return (attr(x, "ts_labels"))
}

#' @describeIn ts_labels Sets the timeseries labels
#' @export
`ts_labels<-` <- function(x, value) {
    if (!is.null(value)) {
        if (!is.character(value)) {
            stop("value should be a character vector")
        }
        if (length(value) != ncol(x)) {
            stop(paste("The length of the labels argument should be equal",
                       "to the number of columns"))
        }
        names(value) <- colnames(x)
    }

    attr(x, "ts_labels") <- value
    return (x)
}

#' Update one or more timeseries labels in a \code{regts} object
#'
#' @param x a \link{regts} object
#' @param labels a named list. The names are the column names
#' and the values are the labels. Specify \code{NULL} to remove all labels.
#' @examples
#' ts <- regts(matrix(1:6, ncol = 2), start = "2016Q2", names = c("a", "b"),
#'              labels <- c("Timeseries a", "???"))
#' ts <-update_ts_labels(ts, list(b = "Timeseries b"))
#' print(ts_labels(ts))
#'
#' @seealso \link{ts_labels}
#' @export
update_ts_labels <- function(x, labels) {
    if (is.null(labels)) {
        ts_labels(x) <- NULL
        return (x)
    }
    lbls <- ts_labels(x)
    if (is.null(lbls)) {
        lbls <- rep("", ncol(x))
        names(lbls) <- colnames(x)
    }
    sel <- which(colnames(x) %in% names(labels))
    lbls[sel] <- as.character(labels[colnames(x)[sel]])
    ts_labels(x) <- lbls
    return (x)
}

#' @export
# do not print ts_labels
print.regts <- function(x, ...) {
    ts_labels(x) <- NULL
    NextMethod("print", .Generic)
}

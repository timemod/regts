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
#' Information about the time period of the timeseries can be obtained
#' with the functions \link{get_regperiod_range}, \link{start_period}
#' and \link{end_period}.
#'
#' See also the description of the functions for handling labels
#' (\link{ts_labels} and \link{update_ts_labels}).
#'
#' @importFrom stats ts
#' @importFrom stats frequency
#' @export
regts <- function(data, start, end = NULL, frequency = NA, names,
                  labels = NULL) {

    data <- as.matrix(data)
    start <- as.regperiod(start, frequency)
    freq <- frequency(start)
    start <- as.numeric(start)  # this turns out to be more efficient
    if (!is.null(end)) {
        end <- as.regperiod(end, frequency)
        if (frequency(end) != freq) {
           stop("The frequency of the second period does not agree")
        }
        end <- as.numeric(end) # this turns out to be more efficient
    }

    if (!missing(names)) {
        if (length(names) != ncol(data)) {
            stop("The length of the names vector is not euqal to the number of columns")
        }
        colnames(data) <- names
    }

    retval <- create_regts(data, start, end, freq, labels)

    if (is.null(attr(retval, "dim"))) {
        # univariate timeseries wihout dimension attributes
        # add dimension attributes so that the timeseries has a column number
        # this situation might occur in the following example:
        # regts(1, start = "2010Q2", end = "2011Q3")
        if (missing(names) || is.null(names)) {
            name <- "Series 1"
        } else {
            name <- names
        }
        retval <- insert_dim_attr(retval, name)
    }

    return (retval)
}

create_regts <- function(data, startp, endp, freq, labels) {
    if (ncol(data) > 1) {
        classes <- c("regts", "mts", "ts", "matrix")
    } else {
        classes <- c("regts", "ts")
    }
    start_vector <- c(startp %/% freq, startp %% freq + 1)
    if (is.null(endp)) {
        retval <- ts(data, start = start_vector,  frequency = freq,
                     class = classes)
    } else {
        end_vector <- c(endp   %/% freq, endp   %% freq + 1)
        retval <- ts(data, start = start_vector,  end = end_vector,
                     frequency = freq,  class = classes)
    }
    if (!is.null(labels)) {
        ts_labels(retval) <- labels
    }
    return (retval)
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
#' \link{regts}, \link{is.regts}, \link{as.data.frame.regts},
#' \link{as.list.regts}, \link{start_period}, \link{end_period}
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
#' # time format "2015 3" instead of "2015Q3", and convert to regts
#' df <- data.frame(periods = c("2015 3", "2015 4", "2016 1"),  a = 1:3)
#' ts <- as.regts(df, time_column = 1, frequency = 4)
#' @export
as.regts <- function(x, ...) {
    UseMethod("as.regts")
}

#' @describeIn as.regts Coerce a \link{ts} to a \link{regts}
#' @export
as.regts.ts <- function(x) {
    if (!is.regts(x)) {
        class(x) <- c("regts", class(x))
        if (is.null(attr(x, "dim"))) {
            # univariate timeseries wihout dimension attributes
            # add dimension attributes so that the timeseries has a column name.
            x <- insert_dim_attr(x, "Series 1")
        }
    }
    return (x)
}

#' @describeIn as.regts Convert a \link{data.frame} to a \link{regts}
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
    lbls <- Hmisc::label(x)
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
#' This method first employs \link{as.ts} and then \link{as.regts.ts}
#' @export
as.regts.default <- function(x, ...) {
    return (as.regts(as.ts(x, ...)))
}

# Add columns with names new_colnames to x, and fill with NA.
add_columns <- function(x, new_colnames) {
    new_columns <- regts(matrix(NA, nrow = nrow(x), ncol = length(new_colnames)),
                         start = start_period.ts(x), frequency = frequency(x))
    old_colnames <- colnames(x)
    x <- regts.intersect(x, new_columns)
    colnames(x) <- c(old_colnames, new_colnames)
    return (x)
}

# This function computes the row numbers of the selected rows in a
# regts object. If the period selector lies partially outside
# the defintion period of the input timeseries, then the function
# returns NULL.
# PARAMETERS:
# sel : a regperiod_range object. This object is used to select rows in
#       a timeseries
# ts_range : the range of the timeseries.
# RETURNS: a vector with the numbers of the select rows in x,
#          or NULL if sel lies (partially) outside the definition period of x.
get_row_selection <- function(sel, ts_range) {
    start_row <- sel[1] - ts_range[1] + 1
    end_row <- start_row + lensub(sel) - 1
    if (start_row >= 1 & end_row <= lensub(ts_range)) {
        return (start_row : end_row)
    } else {
        return (NULL)
    }
}

# Selection on the left-hand side: replace a part of a regts
# (e.g. x["2010Q2", ] <- 2).
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
        ts_range <- get_regperiod_range(x)
        range <- convert_range_selector(as.regperiod_range(i), ts_range)
        row_numbers <- get_row_selection(range, ts_range)
        if (is.null(row_numbers)) {
            # Do not use the window function of ts to extend the timeseries,
            # it is very slow. Use our own function adjust_period
            ts_range_new <- regrange_union(range, ts_range)
            x <- window_regts(x, ts_range_new)
            row_numbers <- get_row_selection(range, ts_range_new)
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

# Selection on the right-hand-side (e.g. x["2010Q2", ]).
#' @export
"[.regts" <- function(x, i, j, ...) {
    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {
        # the row selector is a regperiod_range. Use the C++ function
        # windows_regts
        if (!missing(j)) {
            x <- x[, j]
        }
        x <- window_regts(x, as.regperiod_range(i))
        return (x)
    } else {
        lbls <- ts_labels(x)
        if (!is.null(lbls) && !missing(j)) {
            lbls <- lbls[j]
        }
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


window_regts <- function(x, range) {
    # call C++ function select_rows
    ret <- select_rows(x, range)
    data      <- ret[[1]]
    range_new <- ret[[2]]
    colnames(data) <- colnames(x)
    return (create_regts(data, range_new[1], range_new[2], range_new[3],
                         ts_labels(x)))
}

#' Timeseries labels
#'
#'Retrieve or set labels for the timeseries. Timeseries labels
#'can be used to give a description of the contents of the timeseries.
#'The labels are stored in a named list: the names are the timeseries names
#'(column names), and the values the correpsonding label.
#'@param x a \link{regts}
#'@param value a character vector with the labels or \code{NULL}. The length
#'should be equal to the number of columns. Specify \code{NULL} to remove all
#'labels.
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

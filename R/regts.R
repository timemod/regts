#' Create a \code{regts} timeseries object
#'
#' The \code{regts} class is an extension of the \code{\link[stats]{ts}} class
#' of the \link{stats} package. Working with \code{regts} makes it easier to
#' select periods.
#' #'
#' @param data a vector or matrix of the observed time-series values. A data frame will be
#' coerced to a numeric matrix via \code{\link{data.matrix}}. (See also the description of the
#' function \code{\link[stats]{ts}} of the \code{\link{stats}} package).
#' @param start the starting period as a  \code{\link{regperiod}} object or a
#' character string that can be converted to a \code{regperiod} object.
#' If not specified, then the end period is calculated
#' from argument \code{end} and the dimension of \code{data}
#' @param end the end period as a  \link{regperiod} object or a character string
#' that can be converted to a \code{regperiod} object. If not specified, then
#' the end period is calculated from argument \code{start} and
#' the dimension of \code{data}
#' @param period the period range as a \code{\link{regperiod_range}} object. This
#' argument replaces arguments \code{start} and \code{end}.
#' @param frequency the frequency of the timeseries. This argument should only be specified if
#' the start or end period is specified with a general period format without period indicator,
#' e.g. \code{"2011-3"}

#' @param names a character vector with the column names for the series
#' if \code{data} is a matrix or data frame. Defaults to the colnames of data.
#' @param labels a character vector of labels (descriptions of the timeseries)
#' @return a \code{regts} object
#' @examples
#' # univariate timeseries
#' ts1 <- regts(1:10, start = "2010Q4")
#'
#'
#' # multivariate timeseries
#' ts2 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"))
#'
#'# multivariate timeseries with labels
#' ts3 <- regts(matrix(1:9, ncol = 3), start = "2010Q4", names = c("a", "b", "c"),
#'              labels = paste("Timeseries", c("a", "b", "c")))
#'
#'# multivariate timeseries with period
#' range <- regperiod_range("2016Q1", "2017Q4")
#' ts4 <- regts(matrix(1:16, ncol = 2), period = range, names = c("a", "b"))
#'
#' # create a half-yearly timeseries, because \code{end} is specified the
#' # length of the timeseries is smaller than the length of data (10).
#' ts4 <- regts(1:10, start = "2010-1", end = '2011-2', frequency = 2)
#' @seealso
#' The function \code{\link{is.regts}} can be used to test if an object is a
#' \code{regts}.
#'
#' The S3 generic \code{\link{as.regts}} can be used to coerce an R object to a
#'  \code{regts}. There are currently methods for \code{\link[stats]{ts}} and
#'  \code{\link{data.frame}}.
#'
#' \code{\link{as.data.frame.regts}} and \code{\link{as.list.regts}} can be used
#' to convert \code{regts} to a \code{\link{data.frame}} or a \code{\link{list}}.
#'
#' Function \code{\link{cbind}} can be used to bind two or more
#' timeseries objects and create a multivariate \code{regts}.
#'
#' Information about the time period of the timeseries can be obtained
#' with the functions \code{\link{get_regperiod_range}},
#' \code{\link{start_period}} and \code{\link{end_period}}.
#'
#' See also the description of the functions for handling labels
#' (\code{\link{ts_labels}} and \code{\link{update_ts_labels}}).
#'
#' @importFrom stats ts
#' @importFrom stats frequency
#' @export
regts <- function(data, start, end, period, frequency = NA,
                  names = colnames(data), labels = NULL) {

    # CHECK THE PERIOD
    if (!missing(period) && !missing(start)) {
        stop("Arguments 'start' and 'period' exclude each other!")
    }
    if (missing(start) && missing(end) && missing(period)) {
        start = regperiod("1")
    }

    if (!missing(period)) {
        if (!is.regperiod_range(period)) {
            period <- as.regperiod_range(period)
        }
        start <- start_period(period)
        end <- end_period(period)
        freq <- frequency(start)
    } else {
        if (!missing(start)) {
            start <- as.regperiod(start, frequency)
            freq <- frequency(start)
        }
        if (!missing(end)) {
            end <- as.regperiod(end, frequency)
            if (missing(start)) {
                freq <- frequency(end)
            } else if (frequency(end) != freq) {
                stop("'start' and 'end' have different frequencies")
            }
        }
        if (missing(end)) {
            end <- start + NROW(data) - 1
        } else if (missing(start)){
            start <- end - NROW(data) + 1
        }
    }

    # from now on, work with numerical values (the number of subperiod after
    # Christ), this is more efficient.
    start <- as.numeric(start)
    end   <- as.numeric(end)

    if (start > end) {
        stop("'start' cannot be after 'end'")
    }

    # CONVERT DATA
    if (is.data.frame(data)) {
        data <- data.matrix(data)
    }
    if (is.matrix(data)) {
        ndata <- nrow(data)
        if (!is.null(names) && length(names) != ncol(data)) {
            stop(paste("The length of the names vector is not equal to",
                       "the number of columns"))
        }
        dimnames(data) <- list(NULL, names)

    } else {
        ndata <- length(data)
        if (!missing(names)) {
            warning("Argument names is ignored if data is a vector")
        }
    }
    if (ndata == 0) {
        stop("'ts' object must have one or more observations")
    }
    nobs <- end - start + 1
    if (nobs != ndata) {
        data <- if (is.matrix(data)) {
                    if (ndata < nobs)
                        data[rep_len(1L:ndata, nobs),  , drop = FALSE]
                    else if (ndata > nobs)
                        data[1L:nobs, , drop = FALSE]
                } else {
                    if (ndata < nobs) {
                        rep_len(data, nobs)
                    } else if (ndata > nobs) {
                        data[1L:nobs]
                    }
                }
    }

    return (create_regts(data, start, end, freq, labels))
}

# Internal function to create a regts. No checking of input data.
# startp and endp are the number of subperiods (e.g. quarters) after
# Christ, freq is the frequency of the timeseries.
create_regts <- function(data, startp, endp, freq, labels) {
    attr(data, "tsp") <- c(startp / freq, endp / freq, freq)
    class(data) <- if (NCOL(data) > 1) {
                       c("regts", "mts", "ts", "matrix")
                   }  else {
                       c("regts", "ts")
                   }
    if (!is.null(labels)) {
        ts_labels(data) <- labels
    }
    return (data)
}

#' Tests whether an object is a \code{\link{regts}} timeseries object
#' @param x an arbitrary R object
#' @return \code{TRUE} if \code{x} is a \code{regts}
#' @seealso
#' \code{\link{regts}} and  \code{\link{as.regts}}
#' @export
is.regts <- function(x) {
    return (inherits(x, "regts"))
}

#' Coerce an object to a \code{\link{regts}} timeseries object
#'
#' @param x an arbitrary R object
#' @param time_column the column names or numbers of the data frame
#' in which the time is stored. Specify \code{0} if the index is in the row names
#' of the data frame
#' @param numeric logical: should non numeric values be converted to numeric data.
#' By default they are converted to numeric. This can be changed by setting
#' \code{numeric = FALSE}
#' @param fun a function for converting values in the time column to
#' \code{\link{regperiod}} objects
#' @param ... arguments passed to \code{fun}
#' @return a \code{regts} object
#' @seealso
#' \code{\link{regts}}, \code{\link{is.regts}},
#' \code{\link{as.data.frame.regts}},
#' \code{\link{as.list.regts}}, \code{\link{start_period}}, \code{\link{end_period}}
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

#' @export
as.regts.regts <- function(x, ...) {
    return (x)
}

#' @describeIn as.regts Coerce a \code{\link[stats]{ts}} to a
#' \code{\link{regts}}
#' @export
as.regts.ts <- function(x, ...) {
    class(x) <- c("regts", class(x))
    return (x)
}

#' @describeIn as.regts Convert a \code{\link{data.frame}} to a
#' \code{\link{regts}}
#' @export
as.regts.data.frame <- function(x, time_column = 0, numeric = TRUE,
                                fun = regperiod, ...) {

    # extract time column(s) and data from the input data frame,
    # and convert to a matrix. data are converted to numeric if necessary

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

    datamat <- as.matrix(data)

    # convert (by default) non numeric data
    if (numeric && !is.numeric(datamat)){
        datamat <- apply(datamat, MARGIN = c(1, 2), FUN = as.numeric)
    }


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
    if (identical(times, times[1]:times[nrow(datamat)])) {
        # normal regular timeseries, no missing periods and periods
        # are ordered synchronically
        ret <- regts(datamat, start = create_regperiod(times[1], freq))
    } else {
        # irregular timeseries in dataframe (missing periods or
        # unorderered time index)
        subp_min <- min(times)
        subp_max <- max(times)
        per_count <- subp_max - subp_min + 1
        pmin <- create_regperiod(subp_min, frequency = freq)
        mat <- matrix(NA, nrow = per_count, ncol = ncol(datamat))
        colnames(mat) <- colnames(datamat)
        ret <- regts(mat, start = pmin)
        rows <- times - subp_min +1
        ret[rows, ] <- datamat
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

#' @describeIn as.regts Default method to convert an R object to a
#' \code{\link{regts}}. This method first employs \code{\link[stats]{as.ts}}
#' and then \code{\link{as.regts.ts}}
#' @importFrom stats as.ts
#' @export
as.regts.default <- function(x, ...) {
    return (as.regts(as.ts(x, ...)))
}

# Add columns with names new_colnames to x, and fill with NA
#' @importFrom stats ts.union
add_columns <- function(x, new_colnames) {
    ncols <- length(new_colnames)
    new_columns <- matrix(NA, nrow = nrow(x), ncol = ncols)
    ret <- as.regts(ts.union(x, new_columns))
    colnames(ret) <- c(colnames(x), new_colnames)
    lbls <- ts_labels(x)
    if (!is.null(lbls)) {
        ts_labels(ret) <- c(lbls, rep("", ncols))
    }
    return (ret)
}

# Selection on the left-hand side: replace a part of a regts
# (e.g. x["2010Q2", ] <- 2).
#' @importFrom stats is.mts
#' @export
"[<-.regts" <- function (x, i, j, value) {
    if (!missing(j) && is.character(j)) {
        # Check if j contains names of columns not present in x.
        # Add missing columns if necessary
        cnames <- colnames(x)
        if (is.null(cnames)) {
            stop("object has no column names")
        }
        new_colnames <- setdiff(j, cnames)
        if (length(new_colnames) > 0) {
            x <- add_columns(x, new_colnames)
        }
    }

    if (!missing(i) && (is.character(i) || inherits(i, "regperiod") ||
                        inherits(i, "regperiod_range"))) {

        # call C++ function get_regperiod_range
        ts_range <- get_regperiod_range(x)

        sel_range <- convert_selection_range(as.regperiod_range(i), ts_range)
        if (sel_range[1] < ts_range[1] || sel_range[2] > ts_range[2]) {
            ts_range <- c(min(sel_range[1], ts_range[1]),
                          max(sel_range[2], ts_range[2]), ts_range[3])
            x <- window_regts(x, ts_range)
        }
        i <- seq(sel_range[1] - ts_range[1] + 1,
                 length.out = length_range__(sel_range))
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
#' @importFrom stats is.ts
#' @export
"[.regts" <- function(x, i, j, drop = TRUE) {

    if (missing(i)) {
        lbls <- ts_labels(x)
        if (!is.null(lbls) && !missing(j)) {
            lbls <- lbls[j]
        }
        if (is.matrix(x) && nrow(x) == 1 && (missing(j) || length(j) > 1)) {
            # the result is very weird is the timeseries has a single row
            # and if drop = TRUE is used
            ret <- NextMethod(.Generic, drop = FALSE)
        } else {
            ret <- NextMethod(.Generic)
        }
        ret <- as.regts(ret)
        if (!is.null(lbls)) {
            ts_labels(ret) <- lbls
        }
        return (ret)
    } else {
        # row selection present
        if (is.character(i) || inherits(i, "regperiod") ||
            inherits(i, "regperiod_range")) {
            # first select columns
            if (!missing(j)) {
                x <- x[, j, drop = drop]
            }
            # the row selector is a regperiod_range. Use window_regts
            return (window_regts(x, as.regperiod_range(i)))
        } else  {
            # numeric / logical row selection: the result is a  matrix or vector
            # (no longer a ts)
            return (NextMethod(.Generic))
        }
    }
}

# This function converts regperiod_range object sel_range so that it is a valid
# period selector for a timeseries with period range ts_range. This implies
# that the frequency is made equal to the frequency of ts_range and that
# NA values are replaced by the values of ts_range.
convert_selection_range <- function(sel_range, ts_range) {

    # convert frequency
    if (ts_range[3] %% sel_range[3] != 0) {
        stop(paste0("frequency of timeseries (", ts_range[3],
                    ") not divisible by the frequency of",
                    "the selector (", sel_range[3], ")."))
    }
    fac <- ts_range[3] %/% sel_range[3]
    new_sel_range <- numeric(3)
    new_sel_range[1] <- floor(sel_range[1] * fac)
    new_sel_range[2] <- floor((sel_range[2] + 1) * fac - 1)
    new_sel_range[3] <- ts_range[3]

        # replace NA values by values from ts_range
    new_sel_range <- ifelse(is.na(new_sel_range), ts_range, new_sel_range)

    return (new_sel_range)
}

window_regts <- function(x, sel_range) {
    ts_range <- get_regperiod_range(x)
    sel_range <- convert_selection_range(sel_range, ts_range)
    nper_new <- length_range__(sel_range)
    if (nper_new < 0) {
        stop("Illegal selection")
    }
    shift <- sel_range[1] - ts_range[1]
    rmin <- max(1, 1 - shift)
    rmax <- min(nper_new, length_range__(ts_range) - shift)
    if (is.matrix(x)) {
        data <- matrix(NA, nrow = nper_new, ncol = ncol(x))
        if (rmax >= rmin) {
            data[rmin:rmax, ] <- x[(rmin+shift):(rmax+shift), ]
        }
        colnames(data) <- colnames(x)
    } else {
        data <- logical(nper_new)
        data[] <- NA
        if (rmax >= rmin) {
            data[rmin:rmax] <- x[(rmin+shift):(rmax+shift)]
        }
    }
    return (create_regts(data, sel_range[1], sel_range[2], sel_range[3],
                         ts_labels(x)))
}


#' @export
print.regts <- function(x, ...) {
    # do not print ts_labels
    ts_labels(x) <- NULL
    NextMethod("print", .Generic)
}

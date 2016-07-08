#' Create a \code{regperiod_range} object.
#'
#' Create a \code{regperiod_range} object from two \code{\link{regperiod}} objects
#' or character strings that can be converted to \code{regperiod} objects with
#' function \code{\link{as.regperiod}}. The \code{regperiod_range} object is used to
#' represent an interval of \code{regperiod},  for example, a period from
#' \code{"2012Q2"} to \code{"2016Q4"}.
#'
#' @param p1 the first period (a \code{regperiod}, a character string
#' that can be converted to a \code{regperiod}, or \code{NULL}). If \code{p1}
#' is \code{NULL}, the lower bound of the period range is undetermined.
#' @param p2 the last period (a \code{regperiod}, a character string
#' that can be converted to a \code{regperiod}). If \code{p2} is
#' \code{NULL}, the upper bound of the period range is undetermined.
#' @param frequency frequency of the regperiod objects. This argument is mandatory
#' if argument \code{p1} or \code{p2} is a character with general period format
#' without frequency indicator (e.g. \code{"2011-1"})
#' @return a \code{regperiod_range} object
#' @examples
#' # create a regperiod_range from 2010Q2 to 2016Q3
#' regperiod_range("2010Q2", "2016Q3")
#'
#' # create a regperiod_range for the first 5 quarters after 2013Q2
#' p1 <- regperiod("2013Q3")
#' regperiod_range(p1, p1 + 5)
#'
#' # create a regperiod_range from 2010Q2 with no upper bound
#' regperiod_range("2010Q2", NULL)
#'
#' #create a regperiod_range for a timeseries with frequency 2 (half year)
#' regperiod_range("2010-2", "2016-2", frequency = 2)
#' @seealso \code{\link{lensub}}, \code{\link{start_period}},
#' \code{\link{end_period}}
#' @export
regperiod_range <- function(p1, p2 = p1, frequency = NA) {
    if (is.null(p1) & is.null(p2)) {
        stop("At least one of the periods should not be NULL")
    }
    if (!is.null(p1)) {
        p1 <- as.regperiod(p1, frequency)
        freq1 <- attr(p1, 'frequency')
    }
    if (!is.null(p2)) {
        p2 <- as.regperiod(p2, frequency)
        freq2 <- attr(p2, 'frequency')
    }
    if ((!(is.null(p1) || is.null(p2)))) {
        if (freq1 != freq2) {
            stop("The two periods have different frequency")
        }
        if (p2 < p1) {
            stop(paste0("The start period (", p1, ") is after the end period (", p2, ")"))
        }
    }
    if (!is.null(p1)) {
        freq <- freq1
    } else {
        freq <- freq2
    }

    # convert regperiods to normal numbers
    if (!is.null(p1)) {
        p1 <- as.numeric(p1)
    } else {
        p1 <- NA_real_
    }
    if (!is.null(p2)) {
        p2 <- as.numeric(p2)
    } else {
        p2 <- NA_real_
    }
    return (create_regperiod_range(p1, p2, freq))
}

# internal function to create a regperiod_range from start, end
# and frequency
create_regperiod_range <- function(start, end, frequency) {
    return (structure(c(start, end, frequency), class = "regperiod_range"))
}

#' Coerce an R object to a \code{\link{regperiod_range}}
#'
#' @param x an R object
#' @param frequency the frequency (mandatory for
#' \code{as.regperiod_range.character}
#' if a period format without frequency indicator has been used,
#' e.g. \code{"2011-3"})
#' @param ... arguments passed to methods (not used in this package)
#' @details
#' \code{as.regperiod_range.character} converts a character string to a
#' regperiod_range object. The format for the first and last period of
#' the range are specied as in \code{\link{regperiod}}. The two periods strings
#' are separated by \code{"/"} (e.g. \code{"2010Q2/2016Q2"}).
#' The first or last period may be omitted: in that case the period range
#' has no lower of upper bound (e.g. \code{"2012Q3/"}). The \code{"/"}
#' separator is also optional: if omitted, then the period range
#' contains a single period.

#' @examples
#' as.regperiod_range("2010Q2/2016Q3")
#' as.regperiod_range("2010Q2/")
#' as.regperiod_range("/2016Q3")
#' as.regperiod_range("2016Q1")
#'
#' # convert a regperiod object to a regperiod_range:
#' p <- regperiod("2010Q2")
#' as.regperiod_range(p)

#' @return a \code{regperiod_range}
#' @export
as.regperiod_range <- function(x, frequency = NA, ...) {
    UseMethod("as.regperiod_range")
}

#' @describeIn as.regperiod_range Convert a character string to a
#' regperiod_range object
#' @export
as.regperiod_range.character <- function(x, frequency = NA, ...) {
    return (parse_regperiod_range(x, frequency));
}

#' @export
as.regperiod_range.regperiod_range <- function(x, ...) {
    return (x)
}

#' @describeIn as.regperiod_range Convert a single \code{\link{regperiod}} to a
#' \link{regperiod_range}.
#' @export
as.regperiod_range.regperiod <- function(x, ...) {
    return (create_regperiod_range(as.numeric(x), as.numeric(x), frequency(x)))
}

#' @export
as.character.regperiod_range <- function(x, ...) {
    if (!is.na(x[1])) {
        retval <- as.character.regperiod(start_period(x))
    } else {
        retval <- ""
    }
    retval <- paste0(retval, "/")
    if (!is.na(x[2])) {
        retval <- paste0(retval, as.character.regperiod(end_period(x)))
    }
    return (retval)
}

#' Returns the number of subperiods in a \code{\link{regperiod_range}} object.
#'
#' @param  x a \code{regperiod_range}
#' @return The number of subperiods in the range, or \code{Inf} is the
#' range is not bounded
#' @export
lensub  <- function(x) {
    if (!inherits(x, "regperiod_range")) {
        stop("Variable should be a regperiod_range object")
    }
    if (is.na(x[1]) | is.na(x[2])) {
        return (Inf)
    } else {
        return (x[2] - x[1] + 1)
    }
}

#' @export
print.regperiod_range <- function(x, ...) {
    print(as.character(x))
}



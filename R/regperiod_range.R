#' Create a \code{regperiod_range} object.
#'
#' Create a \code{regperiod_range} object from two \link{regperiod} objects
#' or character strings that can be converted to \link{regperiod} objects with
#' function \link{as.regperiod}. The \code{regperiod_range} object is used to represent an interval
#' of \link{regperiods},  for example, a period from \code{2012Q2} to \code{2016Q4}.
#'
#' @param p1 the first period (a \link{regperiod}, a character string
#' that can be converted to a \link{regpediod}, or \code{NULL}). If \code{p1}
#' is \code{NULL}, the lower bound of the period range is undetermined.
#' @param p2 the last period (a \link{regperiod}, a character string
#' that can be converted to a \link{regpediod}). If \code{p2} is
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
#' @export
regperiod_range <- function(p1, p2 = p1, frequency = NA) {
    if (is.null(p1) & is.null(p2)) {
        stop("At least one of p1 and p2 should not be NULL")
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
            stop(paste("The start period", p1, "is after the end period", p2))
        }
    }
    if (!is.null(p1)) {
        freq <- freq1
    } else {
        freq <- freq2
    }

    # convert regperiods to normal integer
    if (!is.null(p1)) {
        p1 <- as.integer(p1)
    }
    if (!is.null(p2)) {
        p2 <- as.integer(p2)
    }

    return (structure(list(start = p1, end = p2, frequency = freq),
                      class="regperiod_range"))
}

#' @export
as.regperiod_range <- function(x, ...) UseMethod("as.regperiod_range")

#' @export
as.regperiod_range.regperiod_range <- function(x, ...) {
    return (x)
}

#' @export
as.regperiod_range.regperiod <- function(x, ...) {
    return (structure(list(start = as.integer(x), end = as.integer(x),
                      frequency = attr(x, 'frequency')),
                      class = "regperiod_range"))
}

#' Convert a character string to a regperiod_range object
#'
#' Convert a character string to a regperiod_range object. The first and last
#' period of the range are specied as in \link{regts}. The two periods
#' are separated with \code{"/"} (e.g. \code{"2010Q2/2016Q2"}).
#' The first or last period may be omitted: in that case the period range
#' has no lower of upper bound (e.g. \code{"2012Q3/"}).
#' @param x a character string
#' @param frequency (mandatory if a period format without frequency indicator
#' has been used, e.g. \code{"2011-3"})
#' @examples
#' as.regperiod_range("2010Q2/2016Q3")
#' as.regperiod_range("2010Q2/")
#' as.regperiod_range("/2016Q3")
#'
#' # a single period can also be converted to a regperiod_range
#' as.regperiod_range("2016Q1")
#' @export
#' @import stringr
as.regperiod_range.character <- function(x, frequency = NA) {
    pos <- regexpr("/", x)
    if (pos == -1) {
        p1 <- regperiod(x, frequency)
        p2 <- p1
    } else {
        part1 <- substr(x, start = 1, stop = pos - 1)
        part2 <- substr(x, start = pos + 1, stop = nchar(x))
        part1 <- str_trim(part1, side = "right")
        part2 <- str_trim(part2, side = "left")
        if (nchar(part1) > 0) {
            p1 <- regperiod(part1, frequency)
        } else {
            p1 <- NULL
        }
        if (nchar(part2) > 0) {
            p2 <- regperiod(part2, frequency)
        } else {
            p2 <- NULL
        }
    }
    return (regperiod_range(p1, p2))
}

#' Get the start period of the \link{regperiod_range}
#'
#' @param x  a \link{regperiod_range{ object}}
#' @return a \link{regperiod} object representing the first period of the
#' range, or \code{NULL} if the range has no lower boundary.
#' @export
get_start_period <- function(x) {
    if (!inherits(x, "regperiod_range")) {
        stop("x should be a regperiod_range object")
    }
    if (!is.null(x$start)) {
        return (create_regperiod(x$start, x$frequency))
    } else {
        return (NULL)
    }
}

#' Get the end period of the \link{regperiod_range}
#'
#' @param x  a \link{regperiod_range{ object}}
#' @return a \link{regperiod} object representing the last period of the
#' range, or \code{NULL} if the range has no upper boundary.
#' @export
get_end_period <- function(x) {
    if (!inherits(x, "regperiod_range")) {
        stop("x should be a regperiod_range object")
    }
    if (!is.null(x$end)) {
        return (create_regperiod(x$end, x$frequency))
    } else {
        return (NULL)
    }
}

#' @export
as.character.regperiod_range <- function(x) {
    if (!is.null(x$start)) {
        retval <- as.character.regperiod(get_start_period(x))
    } else {
        retval <- ""
    }
    retval <- paste0(retval, "/")
    if (!is.null(x$end)) {
        retval <- paste0(retval, as.character.regperiod(get_end_period(x)))
    }
    return (retval)
}

#' @export
print.regperiod_range <- function(x) {
    print(as.character(x))
}

# Converts the frequency of a regperiod_range object, from lower to higher
# frequency.
modify_frequency <- function(x, new_freq) {
    if (new_freq %% x$frequency != 0) {
        stop("Frequency of regperiod_range is no divisor of the required frequency")
    }
    factor <- new_freq %/% x$frequency
    if (!is.null(x$start)) {
        x$start <- as.integer(x$start * factor)
    }
    if (!is.null(x$end)) {
        x$end <- as.integer((x$end + 1) * factor - 1)
    }
    x$frequency <- new_freq
    return (x)
}

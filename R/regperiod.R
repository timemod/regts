#' Create a regperiod object based on a string.
#'
#' Possible string formats are for example \code{"2010Q2"},
#' \code{"2010M2"}, \code{"2011"} or \code{"2011-1"}.
#' Argument \code{frequency} is mandatory if a general regperiod format
#' such as "2011-1" has been specified
#'
#' @param x a string
#' @param frequency frequency of the regperiod object.
#' @return a \code{regperiod} object
#' @examples
#' regperiod("2010Q3")
#' regperiod("2010-4", frequency = 3)
#' @export
#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
regperiod <- function(x, frequency = NA) {
    if (!is.character(x)) {
        stop("Argument x is not a character")
    }
    return (parse_regperiod(x, frequency))
}

#' Test if an object is a regperiod.
#'
#' @param x an object
#' @return <code>TRUE</code> if the object a \code{regperiod}
#' @export
is.regperiod <- function(x) {
    return (inherits(x, "regperiod"))
}

# binary operators (arithmetic and logical)
#' @export
Ops.regperiod <- function(x, y) {
    if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
        if (!(is.regperiod(x) & is.regperiod(y))) {
            stop("Illegal logical operation")
        }
        if (attr(x, 'frequency') != attr(y, 'frequency')) {
            if (operator %in% c("==", "!=")) {
                return (operator == "==")
            } else {
                stop("Illegal operation")
            }
        }
        return (NextMethod(.Generic))
    } else {
        # TODO: error handling. Prevent strange results
        # (adding to regperiods, subtracting two regperiods with
        # different frequencies etc.)
        retval <- NextMethod(.Generic)
        if (.Generic == "-" & is.regperiod(y)) {
            retval <- as.numeric(retval)
        }
        return(retval)
    }
}

#' Convert an \code{regperiod} to a character representation
#'
#' param x the \code{regperiod} object to be converted
#' @export
as.character.regperiod <- function(x) {
    freq <- frequency(x)
    if (freq == 1) {
        return (as.character(x))
    } else {
        if (freq == 4) {
            freq_char <- "Q"
        } else if (freq == 12) {
            freq_char <- "M"
        } else {
            freq_char <- "-"
        }
        return (paste0(get_year(x), freq_char, get_subperiod(x)))
    }
}

#' Returns the frequency of a \link{regperiod} object
#'
#' @param x a \code{regperiod}
#' @return the frequency of the period
#' @export
frequency.regperiod <- function(x) {
    return (attr(x, "frequency"))
}

# internal function
get_year <- function(x) {
    return (as.numeric(x) %/% frequency(x))
}

# internal function
get_subperiod <- function(x) {
    return (as.numeric(x) %% frequency(x) + 1)
}

#' @export
print.regperiod <- function(x) {
    print(as.character(x))
}

#' @export
as.regperiod <- function(x, ...) UseMethod("as.regperiod")

#' @export
as.regperiod.regperiod <- function(x, ...) {
    return (x)
}

#' @export
as.regperiod.character <- function(x, ...) {
    return (regperiod(x, ...))
}

# Returns the number of subperiods after Christ from a year, subperiod
# and frequency. Internal function.
get_subperiod_count <- function(year, subperiod, frequency) {
    return (year * frequency + subperiod - 1)
}

# Create a regperiod object based on the number of subperiods after Christ.
# Internal function.
create_regperiod <- function(subperiod_count, frequency) {
    return (structure(subperiod_count, class = "regperiod",
                      frequency = frequency))
}

# Returns a time vector according to the convention of the standard ts package.
# For years, this is a single number. For higher frequencies a vector
# of two integers (the year and the subperiod). Internal function.
# PARAMETERS
# x      : a regperiod
# OUTPUT :the time vector
get_time_vector <- function(x) {
    if (frequency(x) == 1) {
        return (as.numeric(x))
    } else {
        return (c(get_year(x), get_subperiod(x)))
    }
}

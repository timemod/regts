#' Create a \code{regperiod} object based on a string.
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
Ops.regperiod <- function(e1, e2) {
    if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
        # logical operator
        if (is.regperiod(e1) && is.regperiod(e2) &&
            (attr(e1, 'frequency') != attr(e2, 'frequency'))) {
            # if e1 and e2 are both regperiods with a different frequency,
            # the operators == and != are meaningful, but comparison
            # operators such as > do not make sense.
            if (.Generic %in% c("==", "!=")) {
                return (.Generic != "==")
            } else {
                stop("Illegal logical operation, only == and != allowed")
            }
        }
        return (NextMethod(.Generic))
    } else if (.Generic %in% c("+", "-")) {
        # arithmetic operator + or -
        retval <- NextMethod(.Generic)
        if (is.regperiod(e1) && is.regperiod(e2)) {
            if (attr(e1, 'frequency') != attr(e2, 'frequency')) {
                stop(paste("Arithmetic operations on regperiods with different",
                           "frequencies are not allowed"))
            }
            if (.Generic == "-") {
                # if the second operand is also a regperiod, then the result
                # is an ordinary number.
                retval <- as.numeric(retval)
            } else {
                stop("Arithmetic operation + on two regperiods is not allowed")
            }

        }
        # the return value should always be an integer
        return (floor(retval))
    } else {
         stop("Illegal arithmetic operation, only + and - allowed")
    }
}

#' @export
as.character.regperiod <- function(x, ...) {
    freq <- frequency(x)
    if (freq == 1) {
        return (as.character(as.numeric(x)))
    } else {
        if (freq == 4) {
            freq_char <- "Q"
        } else if (freq == 12) {
            freq_char <- "M"
        } else {
            freq_char <- "-"
        }
        return (paste0(get_year__(x), freq_char, get_subperiod__(x)))
    }
}

#' Returns the frequency of a \link{regperiod} object
#'
#' @param x a \code{regperiod}
#' @param ... additional arguments for future methods
#' @return the frequency of the \code{regperiod}
#' @export
frequency.regperiod <- function(x, ...) {
    return (attr(x, "frequency"))
}

# internal function
get_year__ <- function(x) {
    return (as.numeric(x) %/% frequency(x))
}

# internal function
get_subperiod__ <- function(x) {
    return (as.numeric(x) %% frequency(x) + 1)
}

#' Returns the year of a \code{\link{regperiod}}
#' @param x a \code{regperiod}
#' @return the year
#' @examples
#' get_year(regperiod("2010Q3"))
#' @seealso \code{\link{get_subperiod}}
#' @export
get_year <- function(x) {
    if (!is.regperiod(x)) {
        stop("Argument x is not a regperiod")
    }
    return (get_year__(x))
}

#' Returns the subperiod of a \code{\link{regperiod}}
#'
#' This function returns the subperiod within a year.
#' For example, for \code{regperiod} \code{2011Q3} the function
#' returns 3.
#' @param x a \code{regperiod}
#' @return the subperiod of a regperiod
#' @examples
#' get_subperiod(regperiod("2010Q3"))
#' @seealso \code{\link{get_year}}
#' @export
get_subperiod <- function(x) {
    if (!is.regperiod(x)) {
        stop("Argument x is not a regperiod")
    }
    return (get_subperiod__(x))
}

#' @export
print.regperiod <- function(x, ...) {
    print(as.character(x))
}

#' Coerce an R object to a regperiod
#'
#' @param x an R object
#' @param ... object passed to methods
#' @return a \link{regperiod}
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

# Create a regperiod object based on the number of subperiods after Christ.
# Internal function.
create_regperiod <- function(subperiod_count, frequency) {
    return (structure(subperiod_count, class = "regperiod",
                      frequency = frequency))
}

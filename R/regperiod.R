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
regperiod <- function(x, frequency = NA) {
    if (!is.character(x)) {
        stop("Argument x is not a character")
    }
    pattern <- "(^[0-9]+)\\s*(([MQ-])\\s*([0-9]+))?$"
    m <- regexec(pattern, x, ignore.case = TRUE)
    if (length(m[[1]]) == 1 && m[[1]] == -1) {
        stop(paste("Illegal period", x))
    }
    parts <- regmatches(x, m)[[1]]
    data <- as.numeric(c(parts[2], parts[5]))
    if (is.na(data[2])) {
        data[2] <- 1
    }
    period <- list(data = data)
    freq_char <- tolower(parts[[4]])
    if (nchar(freq_char) > 0) {
        period$freq <- switch(freq_char, m = 12, q = 4, "-" = frequency)
    } else {
        period$freq <- 1
    }
    if (is.na(period$freq)) {
        stop("Frequency unknown. Specify argument frequency")
    }
    if (!is.na(frequency) && frequency != period$freq) {
        stop("Supplied frequency does not agree with actual frequency in regperiod")
    }
    period <- normalise_regperiod(period)
    return (structure(period, class = "regperiod"))
}

# normalise_regpriod makes sure that the subperiod (x$data[2]) lies between 1
# and x$freq
normalise_regperiod <- function(x) {
    x$data[1] <- x$data[1] + (x$data[2] - 1) %/% x$freq
    x$data[2] <- (x$data[2] - 1) %% x$freq + 1
    return (x)
}

#' @export
"+.regperiod" <- function(x, y) {
    if (!is.numeric(y) || y != as.integer(y)) {
        stop(paste("Error in", substitute(x), "+", substitute(y),
                   ": second operand is not an integer"))
    }
    x$data[2] <- x$data[2] + y
    return (normalise_regperiod(x))
}

#' @export
get_subperiod_count <- function(x) {
    return (x$data[1] * x$freq + x$data[2])
}

#' @export
"-.regperiod" <- function(x, y) {
    if (class(y) == "regperiod") {
        if (x$freq != y$freq) {
            stop("x and y have different frequencies")
        }
        retval <- get_subperiod_count(x) - get_subperiod_count(y)
    } else if (is.numeric(y) && y == as.integer(y)) {
        retval <- x
        retval$data[2] <- retval$data[2] - y
        retval <- normalise_regperiod(retval)
    } else {
        stop(paste("Error in", substitute(x), "+", substitute(y),
                   ": second operand is not a period or integer"))
    }
    return (retval)
}

#' @export
as.character.regperiod <- function(x) {
    if (x$freq == 1) {
        return (as.character(x$data[1]))
    } else {
        if (x$freq == 4) {
            freq_char <- "Q"
        } else if (x$freq == 12) {
            freq_char <- "M"
        } else {
            freq_char <- "-"
        }
        return (paste0(x$data[1], freq_char, x$data[2]))
    }
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

#' @import stringr
#' @export
regperiod_range <- function(x, frequency = NA) {
    if (!is.character(x)) {
        stop("Argument x is not a character")
    }
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
            freq <- p1$freq
        } else {
            p1 <- NULL
        }
        if (nchar(part2) > 0) {
            p2 <- regperiod(part2, frequency)
        } else {
            p2 <- NULL
        }
    }

    if ((!(is.null(p1) || is.null(p2)))) {
        if (p1$freq != p2$freq) {
            stop("The two periods have different frequency")
        }
        if (p2 < p1) {
            stop(paste("The start period", p1, "is after the end period", p2))
        }
    }
    if (!is.null(p1)) {
        freq <- p1$freq
    } else {
        freq <- p2$freq
    }

    return (structure(list(start = p1$data, end = p2$data, freq = freq),
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
    return (structure(list(start = x$data, end = x$data, freq = x$freq),
                   class = "regperiod_range"))
}

#' @export
as.regperiod_range.character <- function(x, ...) {
    return (regperiod_range(x, ...))
}

#' @export
get_start_period <- function(x) {
    if (!inherits(x, "regperiod_range")) {
        stop("x should be a regperiod_range object")
    }
    if (!is.null(x$start)) {
        retval <- (structure(list(data = x$start, freq = x$freq),
                             class = "regperiod"))
    } else {
        retval <- NULL
    }
    return (retval)
}

#' @export
get_end_period <- function(x) {
    if (!inherits(x, "regperiod_range")) {
        stop("x should be a regperiod_range object")
    }
    if (!is.null(x$end)) {
        retval <- (structure(list(data = x$end, freq = x$freq),
                             class = "regperiod"))
    } else {
        retval <- NULL
    }
    return (retval)
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

# Modifies the frequency of a regperiod_range object.
modify_frequency <- function(x, new_freq) {
    if (!inherits(x, "regperiod_range")) {
        stop("x should be a regperiod_range object")
    }
    if (!is.numeric(new_freq) || new_freq != as.integer(new_freq)) {
        stop("new_freq is not an integer")
    }

    if (new_freq %% x$freq != 0) {
        stop("Frequency of regperiod_range is no divisor of the required frequency")
    }

    factor <- new_freq %/% x$freq
    if (!is.null(x$start)) {
        x$start <- c(x$start[1], factor * (x$start[2] - 1) + 1)
    }
    if (!is.null(x$end)) {
        x$end <- c(x$end[1], factor * x$end[2])
    }
    x$freq <- new_freq
    return (x)
}



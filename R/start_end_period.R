#' Returns the start period of a \link{regts} or a \link{regperiod_range}
#'
#' @param x  a \link{regts} or \link{regperiod_range{ object}}
#' @return a \link{regperiod} object representing the first period of the
#' range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{regperiod_range} with no lower boundary.
#' @export
start_period <- function(x) {
    UseMethod("start_period")
}

#' Returns the end period of a \link{regts} or a \link{regperiod_range}
#'
#' @param x  a \link{regts} or \link{regperiod_range{ object}}
#' @return a \link{regperiod} object representing the last period of the
#' range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{regperiod_range} with no upper boundary.
#' @export
end_period <- function(x) UseMethod("end_period")

#' @describeIn start_period Returns the first period
#' @export
start_period.regperiod_range <- function(x) {
    if (!is.null(x$start)) {
        return (create_regperiod(x$start, x$frequency))
    } else {
        return (NULL)
    }
}

#' @describeIn start_period Returns the first period
#' @export
start_period.ts <- function(x) {
    freq <- frequency(x)
    start <- start(x)
    p <- get_subperiod_count(start[1], start[2], freq)
    return (create_regperiod(p, freq))
}

#' @export
start_period.default <- function(x) {
    stop(paste("start_period not defined for objects of class", class(x)))
}

#' @describeIn start_period Returns the end period
#' @export
end_period.regperiod_range <- function(x) {
    if (!is.null(x$end)) {
        return (create_regperiod(x$end, x$frequency))
    } else {
        return (NULL)
    }
}

#' @describeIn start_period Returns the end period
#' @export
end_period.ts <- function(x) {
    freq <- frequency(x)
    end <- start(x)
    p <- get_subperiod_count(end[1], end[2], freq)
    return (create_regperiod(p, freq))
}

#' @export
end_period.default <- function(x) {
    stop(paste("end_period not defined for objects of class", class(x)))
}


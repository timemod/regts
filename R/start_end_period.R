#' Returns the start or end period of a timeseries object or a a
#' \code{\link{regperiod_range}}
#'
#' This function returns the start or end period of a
#' timeseries object (a \code{\link{regts}} or \code{\link[stats]{ts]}}))
#' or a \code{regperiod_range}.
#' @param x  a \code{regts} or \code{regperiod_range} object
#' @return A \code{regperiod} object representing the first or last period of the
#' range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{regperiod_range} with no lower or upper boundary.
#' @export
start_period <- function(x) {
    UseMethod("start_period")
}

#' @rdname start_period
#' @export
end_period <- function(x) UseMethod("end_period")

#' @describeIn start_period Returns the first or last period of a
#' \code{regperiod_range}
#' @export
start_period.regperiod_range <- function(x) {
    if (!is.na(x[1])) {
        return (create_regperiod(x[1], x[3]))
    } else {
        return (NULL)
    }
}

#' @describeIn start_period Returns the first period of a
#' timeseries object
#' @export
start_period.ts <- function(x) {
    r <- get_regperiod_range(x)
    return (create_regperiod(r[1], r[3]))
}

#' @export
start_period.default <- function(x) {
    stop(paste("start_period not defined for objects of class", class(x)))
}

#' @rdname start_period
#' @export
end_period.regperiod_range <- function(x) {
    if (!is.na(x[2])) {
        return (create_regperiod(x[2], x[3]))
    } else {
        return (NULL)
    }
}

#' @rdname start_period
#' @export
end_period.ts <- function(x) {
    r <- get_regperiod_range(x)
    return (create_regperiod(r[2], r[3]))
}

#' @export
end_period.default <- function(x) {
    stop(paste("end_period not defined for objects of class", class(x)))
}

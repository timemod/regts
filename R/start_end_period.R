#' Returns the start period of a \code{\link{regts}} or a
#' \code{\link{regperiod_range}}
#'
#' @param x  a \code{regts} or \code{regperiod_range} object
#' @return a \link{regperiod} object representing the first period of the
#' range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{regperiod_range} with no lower boundary.
#' @export
start_period <- function(x) {
    UseMethod("start_period")
}

#' Returns the end period of a \code{\link{regts}} or a
#' \code{\link{regperiod_range}}
#'
#' @param x  a \code{regts} or \code{regperiod_range} object
#' @return a \code{regperiod} object representing the last period of the
#' range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{regperiod_range} with no upper boundary.
#' @export
end_period <- function(x) UseMethod("end_period")

#' @describeIn start_period Returns the first period
#' @export
start_period.regperiod_range <- function(x) {
    if (!is.na(x[1])) {
        return (create_regperiod(x[1], x[3]))
    } else {
        return (NULL)
    }
}

#' @describeIn start_period Returns the first period
#' @export
start_period.ts <- function(x) {
    r <- get_regperiod_range(x)
    return (create_regperiod(r[1], r[3]))
}

#' @export
start_period.default <- function(x) {
    stop(paste("start_period not defined for objects of class", class(x)))
}

#' @describeIn start_period Returns the end period
#' @export
end_period.regperiod_range <- function(x) {
    if (!is.na(x[2])) {
        return (create_regperiod(x[2], x[3]))
    } else {
        return (NULL)
    }
}

#' @describeIn start_period Returns the end period
#' @export
end_period.ts <- function(x) {
    r <- get_regperiod_range(x)
    return (create_regperiod(r[2], r[3]))
}

#' @export
end_period.default <- function(x) {
    stop(paste("end_period not defined for objects of class", class(x)))
}

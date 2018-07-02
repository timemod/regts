#' Return the start or end period of a timeseries object or a
#' \code{period_range}
#'
#' This function returns the start or end period of a
#' timeseries object (a \code{\link{regts}} or \code{\link[stats]{ts}})
#' or a \code{\link{period_range}}.
#' @param x  a \code{regts}, \code{ts} or \code{period_range} object.
#' @return A \code{period} object representing the first or last period of
#' the range. The return value can be \code{NULL} if argument \code{x} is a
#' \code{period_range} with no lower or upper boundary.
#'
#' @name start_period/end_period
NULL

#' @rdname start_period-slash-end_period
#' @examples
#' # start and end period of a range
#' range <- period_range("2010Q4", "2011Q3")
#' start_period(range)
#' end_period(range)
#'
#' # start and end period of a regts
#' data <- regts(matrix(1:20, ncol = 2), start = "2010Q2", names = c("nl", "uk"))
#' start_period(data)
#' end_period(data)
#'
#' @export
start_period <- function(x) {
  UseMethod("start_period")
}

#' @rdname start_period-slash-end_period
#' @export
end_period <- function(x) UseMethod("end_period")

#' @rdname start_period-slash-end_period
#' @export
start_period.period_range <- function(x) {
  if (!is.na(x[1])) {
    return(create_period(x[1], x[3]))
  } else {
    return(NULL)
  }
}

#' @rdname start_period-slash-end_period
#' @export
start_period.ts <- function(x) {
  r <- get_period_range(x)
  return(create_period(r[1], r[3]))
}

#' @rdname start_period-slash-end_period
#' @export
end_period.period_range <- function(x) {
  if (!is.na(x[2])) {
    return(create_period(x[2], x[3]))
  } else {
    return(NULL)
  }
}

#' @rdname start_period-slash-end_period
#' @export
end_period.ts <- function(x) {
  r <- get_period_range(x)
  return(create_period(r[2], r[3]))
}

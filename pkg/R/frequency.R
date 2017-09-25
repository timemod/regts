#' Return the frequency of a \code{\link{period}} or \code{\link{period_range}}
#'
#' Function returns the frequency of a \code{period} or a
#' \code{period_range}
#'
#' @param x a \code{\link{period}} or a \code{\link{period_range}}
#' @param ... additional arguments for future methods
#' @return the frequency of the \code{period} or the
#' \code{period_range}
#'
#' @name frequency
NULL

#'
#' @rdname frequency
#' @examples
#' p <- period("2016Q1")
#' freq <- frequency(p)
#'
#' p <- period_range("2016Q1", "2018Q2")
#' freq <- frequency(p)#'
#'
#' @export
frequency.period <- function(x, ...) {
  return (attr(x, "frequency"))
}

#' @rdname frequency
#' @export
frequency.period_range <- function(x, ...) {
  return(x[3])
}

#' Return the frequency of a \code{period} or \code{period_range}
#'
#' This is an extension to S3 generic function \code{\link[stats]{frequency}}.
#' Function now also returns the frequency of a \code{period} or a
#' \code{period_range}
#'
#' @param x a \code{\link{period}} or a \code{\link{period_range}}
#' @param ... additional arguments for future methods
#' @return the frequency of the \code{period} or the \code{period_range}
#'
#' @name frequency
#' @examples
#' p <- period("2016Q1")
#' freq <- frequency(p)
#'
#' p <- period_range("2016Q1", "2018Q2")
#' freq <- frequency(p)#'
NULL

#' @describeIn frequency frequency of a \link{period} object
#' @export
frequency.period <- function(x, ...) {
  return (attr(x, "frequency"))
}

#' @describeIn frequency frequency of a \link{period_range} object
#' @export
frequency.period_range <- function(x, ...) {
  return(x[3])
}

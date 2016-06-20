#' Converts timeseries with absolute or relative growth rates to a lower
#' frequency
#'
#' @param x  a \link{ts} of \link{regts} object
#' @param frequency the frequency of the result, should be higher than
#' the frequency of timeseries \code{x}
#' @param method Aggregation method: \code{"cgr"}, \code{"cgrs"}, \code{"cgru"}
#' or \code{"cgrc"}. Consult the Isis reference manual for an explanation of
#' these methods
#' @return a \link{regts} with frequency \code{frequency}
#' @export
#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
aggregate_gr <- function(x,  method, nfrequency = 1) {
    if (!inherits(x, 'ts')) {
        stop("Argument x is not a timeseries")
    }

    # call C++ function agg_gr (see src/agg_gr.cpp)
    return (agg_gr(x, nfrequency, method))
}

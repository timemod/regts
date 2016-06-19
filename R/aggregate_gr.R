#' Converts timeseries with absolute or relative growth rates to a lower
#' frequency
#'
#' @param x  a \link{ts} of \link{regts} object
#' @param frequency the frequency of the result, should be higher than
#' the frequency of timeseries \code{x}
#' @return a \link{regts} with lower frequency
#' @export
#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
aggregate_gr <- function(x, frequency) {
    if (!inherits(x, 'ts')) {
        stop("Argument x is not a timeseries")
    }

    # call C++ function agg_gr (see src/agg_gr.cpp)
    return (agg_gr(x, frequency))
}

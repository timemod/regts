#' Converts timeseries with absolute or relative growth rates to a lower
#' frequency
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param nfrequency the frequency of the result, should be higher than
#' the frequency of timeseries \code{x}
#' @param method Aggregation method: \code{"cgr"}, \code{"cgrs"}, \code{"cgru"}
#' or \code{"cgrc"}. Consult the Isis reference manual for an explanation of
#' these methods
#' @return a \code{regts} with frequency \code{frequency}
#' @export
#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
aggregate_gr <- function(x,  method, nfrequency = 1) {
    if (!inherits(x, 'ts')) {
        stop("Argument x is not a timeseries")
    }
    if (!is.numeric(x)) {
        stop("aggregate_gr is not implemented for non-numeric timeseries")
    }
    # call C++ function agg_gr (see src/agg_gr.cpp)
    res <- agg_gr(x, nfrequency, method)
    data  <- res[[1]]
    range_new <- res[[2]]
    colnames(data) <- colnames(x)
    return (create_regts(data, range_new[1], range_new[2], range_new[3],
                         ts_labels(x)))
}

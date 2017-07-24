#' Converts timeseries with absolute or relative growth rates to a lower
#' frequency
#'
#' @param x  a \code{\link[stats]{ts}} of \code{\link{regts}} object
#' @param nfrequency the frequency of the result should be higher than
#' the frequency of timeseries \code{x}
#' @param method Aggregation method: \code{"dif1s"}, \code{"dif1"}, \code{"pct"}
#' or \code{"rel"}. Consult the Regts vignette for an explanation of
#' these methods
#' @return a \code{regts} with frequency \code{nfrequency}
#' @examples
#' ts_q <- regts(rnorm(10), start = "2016.Q1")
#' aggregate_gr(ts_q, method = "dif1s")
#'
#' ts_m <- regts(matrix(rnorm(20), ncol = 2), start = "2017.M1", names = c("a", "b"))
#' aggregate_gr(ts_m, method = "rel", nfrequency = 4)
#' @export
#' @useDynLib regts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
aggregate_gr <- function(x, method = c("dif1s", "dif1","pct","rel"), nfrequency = 1) {
    method <- match.arg(method)
    if (!is.ts(x)) {
        stop("Argument x is not a timeseries")
    }
    if (!is.numeric(x)) {
        stop("aggregate_gr is not implemented for non-numeric timeseries")
    }
    if (!is.matrix(x)) {
        is_mat <- FALSE
        dim(x) <- c(length(x), 1)
    } else {
        is_mat <- TRUE
    }
    # call C++ function agg_gr (see src/agg_gr.cpp)
    res <- agg_gr(x, nfrequency, method)
    data  <- res[[1]]
    range_new <- res[[2]]

    if (is_mat) {
        colnames(data) <- colnames(x)
    } else {
        dim(data) <- NULL
    }

    return (create_regts(data, range_new[1], range_new[2], range_new[3],
                         ts_labels(x)))
}

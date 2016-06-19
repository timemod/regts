#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
#' @export
aggregate_gr <- function(x, frequency) {
    return (agg_gr(x, frequency))
}

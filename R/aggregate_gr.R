#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
#' @export
aggregate_gr <- function(x, frequency) {
    # TODO: handle labels
    ret <- agg_gr(x, frequency)
    return(ret)
    retval = data
}

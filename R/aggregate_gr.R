#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
#' @export
aggregate_gr <- function(x, frequency) {

    rep <- as.integer(frequency(x) / frequency)

    first_old <- start_period.ts(x)
    first_new <- as.integer((first_old + 2 *(rep - 1)) / rep)
    data <- agg_gr(x, rep, first_old, first_new)

    start_per = create_regperiod(first_new, frequency)

    # TODO: handle labels
    retval <- regts(data, start = start_per)
    return (retval)
}

#' @useDynLib regts
#' @importFrom Rcpp sourceCpp
#' @export
aggregate_gr <- function(x, frequency) {

    rep <- as.integer(frequency(x) / frequency)

    # TODO: create a generic function get_start_period,
    # for regperiod_range and (reg)ts
    first_old <- get_start_period(get_regperiod_range(x))
    first_new <- as.integer((first_old + 2 *(rep - 1)) / rep)
    data <- agg_gr(x, rep, first_old, first_new)

    start_per = create_regperiod(first_new, frequency)

    # TODO: handle labels
    retval <- regts(data, start = start_per)
    return (retval)
}

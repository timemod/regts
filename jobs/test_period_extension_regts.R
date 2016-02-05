library(zoo)
library(microbenchmark)
library(regts)

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
zoo1  <- zooreg(data, start = as.yearqtr("2010Q2"), frequency = 4)
colnames(zoo1) <- namen

ts1 <- as.regts(as.ts(zoo1))
ts2 <- ts1

adjust_period <- function(x, period) {
    per_len <- get_end_period(period) - get_start_period(period) + 1
    retval <- regts(matrix(NA, nrow = per_len, ncol = ncol(ts1)),
                    start = get_start_period(period), names = colnames(x))
    p <- regrange_intersect(get_regperiod_range(x), period)
    if (!is.null(p)) {
        retval[p, ] <- window(x, start = p$start, end = p$start)
    }
    return (retval)
}


tm <- microbenchmark({
    ts3 <- adjust_period(ts2, regperiod_range("2009Q3/2059Q1"))
    ts3['2009Q3/2009Q4', 'ts_3400'] <- 2
}, times = 1)
tm

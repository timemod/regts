# test the performance of get_regperiod_range and a number of alternatives.
library(microbenchmark)
library(regts)

get_regperiod_range_alt1 <- function(x) {
    if (!inherits(x, 'ts')) {
        stop("Argument x is not a timeseries")
    }
    start <- start(x)
    freq <- frequency(x)
    first <- start[0] * freq + start[1] - 1;
    last  <- first + nrow(x) - 1;
    return (structure(c(first, last, freq), class = "regperiod_range"))
}

get_regperiod_range_alt2 <- function(x) {
    if (!inherits(x, 'ts')) {
        stop("Argument x is not a timeseries")
    }
    tsp <- attr(x, "tsp")
    frequency <- tsp[3]
    year <- floor(tsp[1])
    subp <- round((tsp[1] - year ) * frequency);
    first <- year * frequency + subp;
    last  <- first + nrow(x) - 1;
    return (structure(c(first, last, frequency), class = "regperiod_range"))
}

# TODO: first create
ts  <- regts(matrix(1:10, ncol = 2), start = "2010Q2")
t1 <- microbenchmark(get_regperiod_range(ts))
t2 <- microbenchmark(get_regperiod_range_alt1(ts))
t3 <- microbenchmark(get_regperiod_range_alt2(ts))
t4 <- microbenchmark(get_regperiod_range_alt2(ts))

result <- rbind(t1, t2, t3, t4)
print(result)

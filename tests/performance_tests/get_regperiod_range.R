# test the performance of get_regperiod_range and a number of alternatives.
library(regts)
source("performance_tests/time_commands.R")

regts1 <- regts(as.numeric(1:30), start = "2010Q2")

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

commands <- c("get_regperiod_range(regts1)",
              "get_regperiod_range_alt1(regts1)",
              "get_regperiod_range_alt2(regts1)"
)

print(time_commands(commands))

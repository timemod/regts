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

commands <- c("get_regperiod_range(ts)",
              "get_regperiod_range_alt1(ts)",
              "get_regperiod_range_alt2(ts)"
)

parsed_commands <- lapply(commands, FUN = function(x) parse(text = x))
result <- lapply(parsed_commands, FUN = function(x) summary(microbenchmark(eval(x)),
                                                            unit = "us"))
result <- do.call(rbind, result)
result <- data.frame(commands, mean = result$mean)
print(result)

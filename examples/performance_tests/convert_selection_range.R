library(regts)
source("examples/performance_tests/time_commands.R")

convert_range_selector_alt <- function(range, ts_range) {

    freq <- ts_range[3]
    range <- modify_frequency_alt(range, freq)

    # replace NULl periods by periods in x
    if (is.na(range[1])) {
        range[1] <- ts_range[1]
    }
    if (is.na(range[2])) {
        range[2] <- ts_range[2]
    }
    # Check if range is a valid selector of timeseries x.
    # An error will occur for example if timeseries x has period
    # 2010Q1/2011Q2 while range is 2012Q2/.
    if (range[1] > range[2]) {
        p_start <- start_period.period_range(range)
        p_end   <- end_period.period_range(range)
        stop(paste("Start period", p_start, "before end period", p_end))
    }
    return (range)
}

# Converts the frequency of a period_range object, from lower to higher
# frequency.
modify_frequency_alt <- function(x, new_freq) {
    if (new_freq %% x[3] != 0) {
        stop("Frequency of period_range is no divisor of the required frequency")
    }
    factor <- new_freq %/% x[3]
    x[1] <- x[1] * factor
    x[2] <- (x[2] + 1) * factor - 1
    x[3] <- new_freq
    return (x)
}

ts_range <- period_range("2010Q2", "2014Q2")
p1 <- as.period_range("2011/2012")
commands <- c("regts:::convert_selection_range(p1, ts_range)",
              "convert_range_selector_alt(p1, ts_range)"
)

print(time_commands(commands))

print(identical(r1, r2))

library(regts)
library(microbenchmark)

old_regperiod <- function(x, frequency = NA) {
    if (!is.character(x)) {
        stop("Argument x is not a character")
    }
    pattern <- "(^[0-9]+)\\s*(([MQ-])?\\s*([0-9]+))?$"
    m <- regexec(pattern, x, ignore.case = TRUE)
    if (length(m[[1]]) == 1 && m[[1]] == -1) {
        stop(paste("Illegal period", x))
    }
    parts <- regmatches(x, m)[[1]]
    year <- as.integer(parts[2])
    subperiod <- as.integer(parts[5])
    freq_char <- tolower(parts[[4]])
    if (nchar(freq_char) > 0) {
        freq <- switch(freq_char, m = 12, q = 4, "-" = frequency)
    } else if (is.na(frequency) & is.na(subperiod)) {
        # no frequency specified and no subperiod -> assume year
        freq <- 1
    } else {
        freq <- frequency
    }
    if (is.na(freq)) {
        stop("Frequency unknown. Specify argument frequency")
    }
    if (freq == 1 & is.na(subperiod)) {
        subperiod <- 1
    }
    if (subperiod == 0) {
        stop(paste("Illegal period", x))
    }
    if (!is.na(frequency) && frequency != freq) {
        stop("Supplied frequency does not agree with actual frequency in regperiod")
    }
    return (create_regperiod(get_subperiod_count(year, subperiod, freq), freq))
}

# Returns the number of subperiods after Christ from a year, subperiod
# and frequency. Internal function.
get_subperiod_count <- function(year, subperiod, frequency) {
    return (year * frequency + subperiod - 1)
}

# Create a regperiod object based on the number of subperiods after Christ.
# Internal function.
create_regperiod <- function(subperiod_count, frequency) {
    return (structure(subperiod_count, class = "regperiod",
                      frequency = frequency))
}

tm1 <- microbenchmark(x <- regperiod("2010Q2"))
tm1

tm2 <- microbenchmark(x <- old_regperiod("2010Q2"))
tm2


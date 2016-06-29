library(regts)
library(microbenchmark)

get_year <- function(x) {
    return (as.numeric(x) %/% frequency(x))
}

get_subperiod <- function(x) {
    return (as.numeric(x) %% frequency(x) + 1)
}

get_time_vector <- function(x) {
    if (frequency(x) == 1) {
        return (as.numeric(x))
    } else {
        return (c(get_year(x), get_subperiod(x)))
    }
}

my_window <- function(x, i, j) {
    # the row selector is a regperiod_range. Use the
    # window function of ts.
    #range <- convert_range_selector(as.regperiod_range(i), x)
    #extend <- check_extend(x, range)
    # The window function of ts is quite slow if extend = TRUE.
    # Therefore only use extend if it is really necessary
    range <- i
    pstart <- start_period(range)
    pend   <- end_period(range)
    start  <- get_time_vector(pstart)
    end    <- get_time_vector(pend)
    extend <- FALSE
    if (missing(j)) {
        x <- window(x, start = start, end = end, extend = extend)
    } else {
        x <- window(x, start = start, end = end, extend = extend)[, j]
    }
}

my_window2 <- function(x, i, j) {
    # the row selector is a regperiod_range. Use the
    # window function of ts.
    #range <- convert_range_selector(as.regperiod_range(i), x)
    #extend <- check_extend(x, range)
    # The window function of ts is quite slow if extend = TRUE.
    # Therefore only use extend if it is really necessary
    #range <- i
    n <- lensub(i)
    # shift <- start_period(i)- start_period(x)
    #print(n)
    #print(shift)
    n <- 3
    shift <- 6
    p1 <- regperiod("2010Q4")
    return (regts(x[shift:(shift+n-1), ],  p1))
}

regts1 <- regts(matrix(1:1000, ncol = 100), start = "2010Q2")
regts1

t <- microbenchmark(ts1 <- as.ts(regts1))
print(t1)
t <- microbenchmark(xx <- as.regts(ts1))
print(t1)

t <- microbenchmark(regperiod("2010Q2"))
t

t <- microbenchmark(regperiod_range("2010Q2", "2011Q4"))
t

t <- microbenchmark(x <- start_period(regts1) - start_period(regts1))
t


t <- microbenchmark(x <- window(ts1, start = c(2011,4), end = c(2012, 2)))
t

t <- microbenchmark(x <- window(regts1, start = c(2011,4), end = c(2012, 2)))
t

t <- microbenchmark(x <- regts1["2011Q4/2012Q2", ])
t

p <- as.regperiod_range("2011Q4/2012Q2")
t <- microbenchmark(x <- regts1[p, ])
t

t <- microbenchmark(x <- my_window(regts1, p))
t

t <- microbenchmark(x <- my_window2(regts1, p))
t





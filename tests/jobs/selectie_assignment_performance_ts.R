library(zoo)
library(microbenchmark)
library(zoo)

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
zoo1  <- zooreg(data, start = as.yearqtr("2010Q2"), frequency = 4)
colnames(zoo1) <- namen

ts1 <- as.ts(zoo1)
ts2 <- ts1

# kolomselectie
tm <- microbenchmark(ts2[, 'ts_3400'] <- 2, times = 10)
tm

tm <- microbenchmark(window(ts2, start = c(2012, 1), end = c(2012,2))
                            [, 'ts_3400'] <- 2, times = 10)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(ts2[128:129, 3400] <- 2, times = 10)
tm

# selectie van kolom en rij met index
sel <- 1:200
tm <- microbenchmark(ts2[sel, 2400] <- 1:2, times = 10)
tm

# extensie van periode
tm <- microbenchmark(window(ts2, start = c(2009, 3), end = c(2009,4),
                    extend = TRUE)[, 'ts_3400'] <- 2, times = 1)
tm



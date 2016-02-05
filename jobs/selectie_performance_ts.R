library(zoo)
library(microbenchmark)

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
zoo1  <- zooreg(data, start = as.yearqtr("2010Q2"), frequency = 4)
colnames(zoo1) <- namen

ts1 <- as.ts(zoo1)


# kolomselectie
tm <- microbenchmark(ts1[, 'ts_3400'], times = 1000)
tm

tm <- microbenchmark(window(ts1, start = c(2012, 1), end = c(2012))[, 'ts_3400'], 
                     times = 1000)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(ts1[128:129, 3400], times = 1000)
tm

tm <- microbenchmark(ts1[128:129, 'ts_3400'], times = 1000)
tm


#rijselectie
tm < microbenchmark(ts1[128, ], times = 1000)
tm

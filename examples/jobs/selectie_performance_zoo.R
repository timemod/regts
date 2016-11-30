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

ts1 <- zoo1
ts2 <- ts1

# kolomselectie
tm <- microbenchmark(ts2[, 'ts_3400'], times = 100)
tm

tm <- microbenchmark(window(ts2, start = "2012Q1", end = "2012Q2")
                     [, 'ts_3400'], times = 100)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(ts2[128:129, 3400], times = 10)
tm

m  <- as.matrix(zoo1)
tm <- microbenchmark(m[128:129, 3400], times = 1000)
tm

tm <- microbenchmark(m["2010 Q4", 3400], times = 1000)
tm




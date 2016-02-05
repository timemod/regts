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

# kolomselectie
tm <- microbenchmark(ts2[, 'ts_3400'] <- 2, times = 10)
tm

tm <- microbenchmark(ts2['2012Q1/2012Q2', 'ts_3400'] <- 3, times = 10)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(ts2[128:129, 3400] <- 1:2, times = 10)
tm

# extensie van periode
tm <- microbenchmark(ts2['2009Q3/2009Q4', 'ts_3400'] <- 2, times = 1)
tm

# nieuwe reeks toevoegen
tm <- microbenchmark(ts2[, 'x'] <- 2, times = 1)
tm

tm <- microbenchmark(ts2['2012Q3/2012Q4', 'z'] <- 2, times = 1)
tm



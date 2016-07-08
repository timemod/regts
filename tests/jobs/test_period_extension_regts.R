library(microbenchmark)
library(regts)

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
ts1  <- regts(data, start = "2010Q2", names = namen)

tm <- microbenchmark({
    x <- ts1['2014Q4/2015Q1', 'ts_3400']
}, times = 1)
tm

ts2 <- ts1
tm <- microbenchmark({
    ts2['2009Q3/2009Q4', 'ts_3400'] <- 2
}, times = 1)
tm

tm <- microbenchmark({
    x <- ts1['2008Q4/2009Q1', 'ts_3400']
}, times = 1)
tm



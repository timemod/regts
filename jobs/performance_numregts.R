library(zoo)
library(microbenchmark)

set.seed(12345)
aantal_variabelen <- 50
aantal_perioden <- 100

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(as.character(rnorm(n  = aantal_variabelen * aantal_perioden)),
               ncol = aantal_variabelen)
regts1  <- regts(data, start ="2010Q2")
ts1 <- as.ts(regts1)
colnames(ts1) <- namen

tm <- microbenchmark(window(ts1, start = c(2012, 1), end = c(2012, 4)))
tm

p <- as.regperiod_range("2010Q3/2012Q3")
tm <- microbenchmark(window_numregts(regts1, p))
tm

tm <- microbenchmark(window_regts(regts1, p))
tm



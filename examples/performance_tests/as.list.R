rm(list = ls())
library(regts)
library(tictoc)
# use as.list from pakcage zoo for normal ts objects
library(zoo)

set.seed(12345)
aantal_variabelen <- 10000
aantal_perioden <- 30

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010")
ts1 <- as.ts(regts1)

tic("regts")
lregts <- as.list(regts1)
toc()

tic("ts")
lts <- as.list(ts1)
toc()

tic("as.regts.list")
regts2 <- as.regts(lregts)
toc()

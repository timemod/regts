rm(list = ls())
library(regts)
library(zoo)
source("examples/performance_tests/time_commands.R")

set.seed(12345)
aantal_variabelen <- 5000
aantal_perioden <- 200

#aantal_variabelen <- 50
#aantal_perioden <- 20

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")
ts_labels(regts1) <- namen
ts1 <- as.ts(regts1)
zoo1 <- as.zoo(ts1)

lts <- as.list(ts1)
lregts <- as.list(regts1)
lzoo <- as.list(zoo1)

commands <- c("lts  <- as.list(ts1)",
              "lregts <- as.list(regts1)",
              "lzoo <- as.list(zoo1)",
              "do.call(ts.union, lts)",
              "do.call(ts.intersect, lts)",
              "do.call(cbind, lregts)",
              "do.call(merge, lzoo)",
              "as.regts(regts1)")

print(time_commands(commands, times = 1, unit = "ms"))



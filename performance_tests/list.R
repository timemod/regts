library(regts)
library(zoo)
source("performance_tests/time_commands.R")

set.seed(12345)
aantal_variabelen <- 5000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")
ts1 <- as.ts(regts1)
zoo1 <- as.zoo(ts1)

lts <- as.list(ts1)
lregts <- as.list(regts1)
lzoo <- as.list(zoo1)

commands <- c("lts  <- as.list(ts1)",
              "lregts <- as.list(regts1)",
              "lzoo <- as.list(zoo1)",
              "do.call(ts.intersect, lts)",
              "do.call(regts.intersect, lts)",
              "do.call(merge, lzoo)"
)

print(time_commands(commands, times = 1, unit = "ms"))



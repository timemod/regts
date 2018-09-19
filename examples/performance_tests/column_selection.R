library(regts)
library(zoo)
source("examples/performance_tests/time_commands.R")


set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")
ts1 <- as.regts(regts1)
zoo1 <- as.zooreg(ts1)

commands <- c("ts1[ , \"ts_3400\"]",
              "regts1[ , \"ts_3400\"]",
              "zoo1[ , \"ts_3400\"]"
)

print(time_commands(commands))



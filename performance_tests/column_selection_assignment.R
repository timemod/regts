library(regts)
library(zoo)
source("performance_tests/time_commands.R")

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")
ts1 <- as.ts(regts1)
zoo1 <- as.zoo(ts1)

commands <- c("ts1[ , \"ts_3400\"] <- 2",
              "regts1[ , \"ts_3400\"] <- 2",
              "zoo1[ , \"ts_3400\"] <- 2"
)

print(time_commands(commands))

# add a new columns
commands <- c("tmp <- regts1; tmp[ , \"x\"] <- 2"
)
print(time_commands(commands))



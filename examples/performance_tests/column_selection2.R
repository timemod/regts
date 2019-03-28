library(regts)
library(zoo)
source("time_commands.R")


set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")
ts1 <- as.regts(regts1)
zoo1 <- as.zooreg(ts1)

cols <- paste0("ts_", seq(1, 3400, by = 10))
print(cols)

commands <- c("ts1[ , cols]",
              "regts1[ , cols]",
              "zoo1[ , cols]"
)

print(time_commands(commands))



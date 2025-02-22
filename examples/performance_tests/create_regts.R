# test the performance of the function regts, create_regts and an alternative
library(microbenchmark)
library(regts)
source("examples/performance_tests/time_commands.R")

p1 <- period("2010Q2")
data <- matrix(1:10, ncol = 2)
names <- c("a", "b")
colnames(data) <- names
p2 <- p1 + nrow(data) - 1
range <- period_range(p1, p2)

commands <- c("ts(data, start = c(2010, 2), frequency = 4)",
              "regts(data, p1)",
              "regts:::create_regts(data, as.numeric(p1), as.numeric(p2), range[3], NULL)"

)

print(time_commands(commands))

library(regts)
source("tests/performance_tests/time_commands.R")

regts1 <- regts(as.numeric(1:30), start = "2010Q1")
ts1 <- regts:::unregts(regts1)
p1 <- as.regperiod_range("2010Q4/2011Q1")

commands <- c("aggregate(ts1)",
              "aggregate(regts1)",
              "aggregate_gr(regts1, method = \"cgr\")"
)

print(time_commands(commands))

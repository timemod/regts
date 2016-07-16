# test the performance of the function regts, create_regts and an alternative
library(regts)
library(zoo)
source("tests/performance_tests/time_commands.R")

regts1 <- regts(as.numeric(1:300), start = "2010Q2", labels = "aap")
ts1 <- regts:::unregts(regts1)
zoo1 <- as.zooreg(ts1)
regts2 <- regts(as.numeric(1:300), start = "2010Q2")
ts2 <- regts:::unregts(regts2)
zoo2 <- as.zooreg(ts2)

commands <- c("ts1 + 1",
              "regts1 + 1",
              "zoo1 + 1",
              "ts1 < 2",
              "regts1 < 2",
              "zoo1 < 2",
              "ts1 + ts1",
              "regts1 + regts1",
              "zoo1 + zoo1",
              "ts2 + 1",
              "regts2 + 1",
              "zoo2 + 1",
              "ts2 < 2",
              "regts2 < 2",
              "zoo2 < 2",
              "ts2 + ts2",
              "regts2 + regts2",
              "zoo2 + zoo2"
)

print(time_commands(commands))

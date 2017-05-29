library(regts)
library(data.table)

print(system.time(
  ts1 <- read_ts_csv("csv/nigem_rowwise.csv")
))

print(system.time(
  ts2 <- read_ts_csv("csv/nigem_columnwise.csv")
))

print(system.time(
  ts3 <- read_ts_csv("csv/nigem_rowwise.csv", labels = "after")
))

print(system.time(
  ts4 <- read_ts_csv("csv/nigem_columnwise.csv", labels = "after")
))

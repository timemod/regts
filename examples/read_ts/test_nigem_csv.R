library(regts)
library(data.table)

print(system.time(
  ts1 <- read_ts_csv("csv/nigem_rowwise.csv")
))

print(system.time(
  ts2 <- read_ts_csv("csv/nigem_columnwise.csv")
))

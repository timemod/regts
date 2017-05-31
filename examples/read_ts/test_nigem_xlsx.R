library(regts)
library(data.table)
library(xlsx)

print(system.time(
  df <- read.xlsx2("xlsx/nigem_rowwise.xlsx", 1)
))

print(system.time(
  ts1 <- read_ts_xlsx("xlsx/nigem_rowwise.xlsx")
))

print(system.time(
  ts2 <- read_ts_xlsx("xlsx/nigem_rowwise.xlsx", labels = "after")
))

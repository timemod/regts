library(regts)
library(data.table)

print(system.time(
  ts1 <- read_ts_xlsx("xlsx/nigem_rowwise.xlsx")
))

quit()

print(system.time(
  ts2 <- read_ts_xlsx("xlsx/nigem_rowwise.xlsx", labels = "after")
))

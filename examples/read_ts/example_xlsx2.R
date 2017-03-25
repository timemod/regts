library(regts)

csv_file <- "xlsx/example2.xlsx"

ts1 <- read_ts_xlsx(csv_file, skipcol = 1, sheet = 2, labels = "after")
print(ts1)

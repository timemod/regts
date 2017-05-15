library(regts)

csv_file <- "csv/example1.csv"

ret <- read_ts_csv(csv_file, skiprow = 1, skipcol = 1)
print(ret)

ts2 <- read_ts_csv(csv_file, skiprow = 1, skip = 1, skipcol =1 , labels = "after")
print(ts2)
View(ts2)

ts3 <- read_ts_csv(csv_file, skiprow = 1, skipcol =1 , labels = "before")
print(ts3)
View(ts3)

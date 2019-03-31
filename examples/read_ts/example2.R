library(regts)

csv_file <- "csv/example2.csv"

ts1 <- read_ts_csv(csv_file, skipcol = 1, strict = FALSE)
print(ts1)

ts2 <- read_ts_csv(csv_file, skipcol = 1, labels = "after", strict = FALSE)
print(ts2)
View(ts2)

ts3 <- read_ts_csv(csv_file, skipcol = 1, labels = "before", strict = FALSE)
print(ts3)
View(ts3)

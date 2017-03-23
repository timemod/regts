library(regts)

csv_file <- "csv/example1.csv"

df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
               skip = 1)
ts1 <- read_ts(df)
print(ts1)

ts2 <- read_ts(df, labels = "after")
print(ts2)
View(ts2)

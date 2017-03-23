library(regts)

csv_file <- "csv/example2.csv"

df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)

ts1 <- read_ts(df)
print(ts1)

ts2 <- read_ts(df, labels = "after")
print(ts2)
View(ts2)

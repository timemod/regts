library(regts)

csv_file <- "csv/example2.csv"

df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE)

# skip the first column
df <- df[, -1]

ts1 <- read_ts(df)
print(ts1)

ts2 <- read_ts(df, labels = "after")
print(ts2)
View(ts2)

ts3 <- read_ts(df, labels = "before")
print(ts3)
View(ts3)

library(regts)

csv_file <- "csv/example1.csv"


# read the timeseries and skip the first row (employing argument skip)
df <- read.csv(csv_file, check.names = FALSE, stringsAsFactors = FALSE,
               skip = 1)

# skip the first
df <- df[, -1]

ts1 <- read_ts(df)
print(ts1)

ts2 <- read_ts(df, labels = "after")
print(ts2)
View(ts2)

ts3 <- read_ts(df, labels = "before")
print(ts3)
View(ts3)

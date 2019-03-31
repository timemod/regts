library(regts)

csv_file <- "csv/example1.csv"

ret <- read_ts_csv(csv_file, skiprow = 1, skipcol = 1, strict = FALSE)
print(ret)

ts2 <- read_ts_csv(csv_file, skiprow = 1, skipcol =1 , labels = "after",
                   strict = FALSE)
print(ts2)
View(ts2)

ts3 <- read_ts_csv(csv_file, skiprow = 1, skipcol =1 , labels = "before",
                   strict = FALSE)
print(ts3)
View(ts3)

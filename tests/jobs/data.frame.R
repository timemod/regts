library(regts)
library(zoo)

df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3)
df
ts <- as.regts(df, FUN = as.yearqtr, index_column = 1)
ts

df <- data.frame(a = 1:3, b = 10:12)
rownames(df) <-c("2015 3", "2015 4", "2016 1")
df
ts <- as.regts(df, FUN = as.yearqtr, format = "%Y %q")
ts

df2 <- as.data.frame(t(df))
df2 <- cbind(data.frame(labels = c("Timeseries a", "Timeseries b")), df2)
ts2 <- as.regts(df2, columnwise = FALSE, FUN = as.yearqtr, format = "%Y %q",
                label_column = 1)


df <- data.frame(period = c("2015 3", "2015 4", "2016 1"),  a = 1:3, b = 10:12)
ts <- as.regts(df, index_col = 1, FUN = as.yearqtr, format = "%Y %q")
ts


ts <- regts(1:3 , start = "2015Q3", names = "a", labels = "Timeseries a")
print(ts_labels(ts))
print(as.data.frame(ts))
print(as.data.frame(ts, columnwise = FALSE))
ts_labels(ts) <- NULL
print(as.data.frame(ts))
print(as.data.frame(ts, columnwise = FALSE))

df <- data.frame(period = c(2015,2016,2017), a = 1:3)
df
read.zoo(df, index.column = 1, regular = TRUE)
as.regts(as.ts(read.zoo(df, index.column = 1, regular = TRUE)))
as.regts(df, index_column = 1)

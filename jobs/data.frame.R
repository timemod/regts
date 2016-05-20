library(regts)
library(zoo)

df <- data.frame(period = c("2015Q3", "2015Q4", "2016Q1"), a = 1:3)
df

ts <- as.regts(df, FUN = as.yearqtr)
ts
print(attributes(ts))


df <- data.frame(a = 1:3, b = 10:12)
rownames(df) <-c("2015 3", "2015 4", "2016 1")
df

ts <- as.regts(df, FUN = as.yearqtr, format = "%Y %q", index.column = "row.names")
ts
print(attributes(ts))

ts <- regts(1:3 , start = "2015Q3", names = "a")
print(as.data.frame(ts))

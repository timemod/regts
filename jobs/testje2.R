library(regts)
library(microbenchmark)
ts <- regts(1:10, "2010Q2", labels = "aap")
print(ts)

t <- microbenchmark(df <- as.data.frame(ts))
print(df)
print(t)


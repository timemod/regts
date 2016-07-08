library(regts)

ts <- ts(1:3, start = c(2010,3), frequency = 4)
ts
print(as.regts(ts))

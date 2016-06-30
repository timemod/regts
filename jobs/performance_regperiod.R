library(microbenchmark)
library(regts)
library(stringr)

t <- microbenchmark(regperiod("2010Q2"))
print(t)

t <- microbenchmark(as.regperiod_range("2010Q2/2011Q3"))
print(t)

p1 <- regperiod("2010Q2")
t <- microbenchmark(regperiod_range(p1, p1))
print(t)


library(microbenchmark)
library(regts)


t1 <- microbenchmark(regperiod("2010Q2"))
t2 <- microbenchmark(as.regperiod_range("2010Q2/2011Q3"))

result <- rbind(t1, t2)
print(result)




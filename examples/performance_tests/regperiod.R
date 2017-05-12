library(microbenchmark)
library(regts)


t1 <- microbenchmark(period("2010Q2"))
t2 <- microbenchmark(as.period_range("2010Q2/2011Q3"))

result <- rbind(t1, t2)
print(result)




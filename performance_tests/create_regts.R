# test the performance of the function regts, create_regts and an alternative
library(microbenchmark)
library(regts)

p1 <- regperiod("2010Q2")
data <- matrix(1:10, ncol = 2)
names <- c("a", "b")
colnames(data) <- names
p2 <- p1 + nrow(data) - 1
range <- regperiod_range(p1, p2)

t1 <- microbenchmark(ts(data, start = c(2010, 2), frequency = 4))
t2 <- microbenchmark(regts(data, p1))
t3 <- microbenchmark(create_regts(data, as.numeric(p1), NULL, range[3], NULL))

result <- rbind(t1, t2, t3)
print(result)

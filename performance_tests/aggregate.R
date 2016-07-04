# test the performance of the function regts, create_regts and an alternative
library(microbenchmark)
library(regts)

regts1 <- regts(as.numeric(1:30), start = "2010Q1")
ts1 <- as.ts(regts1)

p1 <- as.regperiod_range("2010Q4/2011Q1")

commands <- c("aggregate(ts1)",
              "aggregate(regts1)",
              "aggregate_gr(regts1, method = \"cgr\")"
)

parsed_commands <- lapply(commands, FUN = function(x) parse(text = x))
result <- lapply(parsed_commands, FUN = function(x) summary(microbenchmark(eval(x)),
                                                           unit = "us"))
result <- do.call(rbind, result)
result <- data.frame(commands, mean = result$mean)
print(result)

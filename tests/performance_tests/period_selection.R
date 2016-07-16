# test the performance of the function regts, create_regts and an alternative
library(regts)
library(zoo)
source("tests/performance_tests/time_commands.R")

regts1 <- regts(matrix(as.numeric(1:30), ncol = 1), start = "2010Q2")
ts1 <- regts:::unregts(regts1)
zoo1 <- as.zooreg(ts1)

p1 <- as.regperiod_range("2010Q4/2011Q1")

# start and end period for zoo
startp <- as.yearqtr(start_period(p1))
endp   <- as.yearqtr(end_period(p1))

commands <- c("window(ts1, start = c(2010, 4), end = c(2011, 1), extend = FALSE)",
              "window(ts1, start = c(2010, 4), end = c(2011, 1), extend = TRUE)",
              "window(regts1, start = c(2010, 4), end = c(2011, 1))",
              "regts:::window_regts(regts1, p1)",
              "regts1[p1, ]",
              "window(zoo1, start = startp, end = endp)"
)

print(time_commands(commands))

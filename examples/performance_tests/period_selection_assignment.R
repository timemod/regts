# test the performance of the function regts, create_regts and an alternative
library(regts)
library(zoo)
source("examples/performance_tests/time_commands.R")

regts1 <- regts(as.numeric(1:30), start = "2010Q2")
ts1 <- as.regts(regts1)
zoo1 <- as.zooreg(ts1)

p1 <- as.period_range("2010Q4/2011Q1")

# start and end period for zoo
startp <- as.yearqtr(start_period(p1))
endp   <- as.yearqtr(end_period(p1))

# period selection assignment in univariate ts

commands <- c("window(ts1, start = c(2010, 4), end = c(2011, 1), extend = FALSE) <- 2",
              "window(ts1, start = c(2010, 4), end = c(2011, 1), extend = TRUE) <- 2",
              "window(regts1, start = c(2010, 4), end = c(2011, 1)) <- 2",
              "regts1[p1] <- 2",
              "window(zoo1, start = startp, end = endp) <- 2"
)

print(time_commands(commands))

# periodeselectie met extensie
p2 <- as.period_range("2009Q4/2011Q1")
commands <- c("tmp <- ts1; window(tmp, start = c(2009, 4), end = c(2011, 1), extend = TRUE) <- 2",
              "tmp <- regts1; window(tmp, start = c(2009, 4), end = c(2011, 1), extend = TRUE) <- 2",
              "tmp <- regts1; tmp[p2] <- 2"
)

print(time_commands(commands))

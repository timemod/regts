library(regts)
library(testthat)
context("disagg")

q_per <- period_range("2017Q1/2019Q4")

l <- seq_len(nperiod(q_per))
cos_data <- 10 + l + 2 * cos(l * 10)
sin_data <- 10 + l + 2 * sin(l * 10)
a_q <- regts(cos_data,  period = q_per)
b_q <- regts(sin_data, period = q_per)

a_m <- disagg(a_q, nfrequency = 12)
print(a_m)

b_m <- disagg(b_q, nfrequency = 12)
print(b_m)

ab_q <- cbind(a_q, b_q)
ab_m <- disagg(ab_q, nfrequency = 12)
print(ab_m)

library(regts)

regts1 <- regts(matrix(1:8, ncol = 2), start = "2010Q2")
print(window_regts(regts1, as.period_range("2010Q3/2010Q4")))
print(window_regts(regts1, as.period_range("2010Q3/")))
print(window_regts(regts1, as.period_range("/2012Q1")))
print(window_regts(regts1, as.period_range("2008Q1")))



regts1

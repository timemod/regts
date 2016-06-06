library(regts)

dfq <- data.frame(period = c("2015Q1", "2015Q2", "2015Q3"), a = 1:3, b = 12:14)
dfq

print(as.regts(dfq, time_column = 1))

dfy <- data.frame(a = 1:3, b = 12:14)
rownames(dfy) <- c("2010", "2011", "2012")
print(as.regts(dfy))

dfq2 <- data.frame(period = c("2016Q1", "2015Q2", "2015Q3"), a = 1:3, b = 12:14)
dfq2
print(as.regts(dfq2, time_column = 1))

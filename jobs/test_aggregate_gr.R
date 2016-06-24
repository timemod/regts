library(regts)

ts <- regts(matrix(1:10, ncol = 2), start = "2010Q3")
ts


aggregate_gr(ts)

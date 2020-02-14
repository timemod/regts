library(regts)
library(tictoc)
rm(list = ls())

ncol  <- 40000
nt <- 2000
rts1 <- regts(matrix(as.numeric(1 : (ncol * nt)), ncol = ncol), start = "2018q1")
colnames(rts1) <- paste0("ts1_", 1:ncol)


tic("as.list")
x_l1 <- as.list(rts1)
toc()

tic("as.regts.list")
x <- as.regts(x_l1)
toc()
quit()

tic("do.call cbind regts")
x <- do.call(cbind, x_l1)
toc()

tic("do call ts.union")
x <- do.call(ts.union, x_l1)
toc()


x_l1$ts1_1 <- x_l1$ts_1["/2020"]
tic("as.list regts different ranges")
x <- do.call(cbind, x_l1)
toc()

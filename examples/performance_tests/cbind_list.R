library(regts)
library(tictoc)
rm(list = ls())

ncol  <- 20000
nt <- 2000
rts1 <- regts(matrix(as.numeric(1 : (ncol * nt)), ncol = ncol), start = "2018q1")
colnames(rts1) <- paste0("ts1_", 1:ncol)


tic("as.list")
x_l1 <- as.list(rts1)
toc()

tic("as.list")
x <- do.call(cbind, x_l1)
toc()

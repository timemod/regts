library(regts)
library(tictoc)
rm(list = ls())

ncol  <- 2000
nt <- 2000
rts1 <- regts(matrix(as.numeric(1 : (ncol * nt)), ncol = ncol), start = "2018q1")
colnames(rts1) <- paste0("ts1_", 1:ncol)

rts2 <- rts1
colnames(rts2) <- paste0("ts2_", 1:ncol)
rts2 <- rts2["/2300"]

tic("cbind.regts")
x_regts <- cbind(rts1, rts2)
toc()
cat("\n")

ts1 <- as.ts(rts1)
ts2 <- as.ts(rts2)
tic("cbind.ts")
x_regts <- cbind(ts1, ts2)
toc()
cat("\n")


tic("do.call regts")
x_regts <- do.call(cbind, list(rts1, rts2))
toc()
cat("\n")

tic("do.call ts.union regts")
x_regts <- do.call(ts.union, list(ts1, ts2))
toc()
quit()

rts1_lbls <- rts1
rts2_lbls <- rts2
ts_labels(rts1_lbls) <- colnames(rts1)
ts_labels(rts2_lbls) <- colnames(rts2)
tic("cbind.regts")
x_regts_lbls <- cbind(rts1_lbls, rts2_lbls)
toc()

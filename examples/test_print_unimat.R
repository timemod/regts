library(regts)
library(zoo)

data <- matrix(sqrt(1:30), ncol = 1)
colnames(data) <- "timeseries"
unimat_ts_y <- ts(data, start = 2011, frequency = 1)
unimat_ts_h <- ts(data, start = c(2011, 2), frequency = 2)
unimat_ts_q <- ts(data, start = c(2011, 2), frequency = 4)
unimat_ts_m <- ts(data, start = c(2011, 2), frequency = 12)

unimat_regts_y <- as.regts(unimat_ts_y)
unimat_regts_h <- as.regts(unimat_ts_h)
unimat_regts_q <- as.regts(unimat_ts_q)
unimat_regts_m <- as.regts(unimat_ts_m)

cat("\n*** ts objects ***\n")
printobj(unimat_ts_y)
printobj(unimat_ts_h)
printobj(unimat_ts_q)
printobj(unimat_ts_m)

cat("\n *** regts ***\n")
printobj(unimat_regts_y)
printobj(unimat_regts_h)
printobj(unimat_regts_q)
printobj(unimat_regts_m * 1)

# also check zoo!
cat("\n *** zoo ***\n")
printobj(as.zooreg(unimat_regts_y))
printobj(as.zooreg(unimat_regts_h))
printobj(as.zooreg(unimat_regts_q))
printobj(as.zooreg(unimat_regts_m))

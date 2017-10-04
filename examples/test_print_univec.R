library(regts)
library(zoo)

vec_ts_y <- ts(sqrt(1:30), start = 2011, frequency = 1)
vec_ts_h <- ts(sqrt(1:30), start = c(2011, 2), frequency = 2)
vec_ts_q <- ts(sqrt(1:30), start = c(2011, 2), frequency = 4)
vec_ts_m <- ts(sqrt(1:30), start = c(2011, 2), frequency = 12)

vec_regts_y <- as.regts(vec_ts_y)
vec_regts_h <- as.regts(vec_ts_h)
vec_regts_q <- as.regts(vec_ts_q)
vec_regts_m <- as.regts(vec_ts_m)

cat("\n*** ts objects ***\n")
printobj(vec_ts_y)
printobj(vec_ts_h)
printobj(vec_ts_q)
printobj(vec_ts_m)

cat("\n *** regts ***\n")
printobj(vec_regts_y)
printobj(vec_regts_h)
printobj(vec_regts_q)
printobj(vec_regts_m * 1)

# also check zoo!
cat("\n *** zoo ***\n")
printobj(as.zooreg(vec_regts_y))
printobj(as.zooreg(vec_regts_h))
printobj(as.zooreg(vec_regts_q))
printobj(as.zooreg(vec_regts_m))

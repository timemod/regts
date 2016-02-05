library(microbenchmark)
library(regts)

set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
regts1  <- regts(data, start = "2010Q2", names = namen)


# kolomselectie
tm <- microbenchmark(regts1[, 'ts_3400'], times = 1000)
tm

# selectie van kolom en rij met labels
tm <- microbenchmark(regts1["2012Q1/2012Q2", 'ts_3400'], times = 1000)
tm

# selectie met vooral gedefinieerde periode
p <- as.regperiod_range("2012Q1/2012Q2")
tm <- microbenchmark(regts1[p, 'ts_3400'], times = 1000)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(regts1[128:129, 3400], times = 1000)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(regts1[128:129, 'ts_3400'], times = 1000)
tm


#rijselectie
tm < microbenchmark(regts1[128, ], times = 1000)
tm

# als dataframe

df <- as.data.frame(regts1)

tm <- microbenchmark(df['2012 Q2', 'ts_3400'], times = 1000)
tm

tm <- microbenchmark(df[128, 3400], times = 1000)
tm

# als matrix
mat <- as.matrix(regts1)

tm <- microbenchmark(mat['2012 Q2', 'ts_3400'], times = 1000)
tm

tm <- microbenchmark(mat[128,3400], times = 10000)
tm




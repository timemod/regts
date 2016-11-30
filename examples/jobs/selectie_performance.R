library(zoo)
library(microbenchmark)

set.seed(12345)
aantal_variabelen <- 10000
aantal_perioden <- 142

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
zoo1  <- zooreg(data, start = as.yearqtr("1980Q2"), frequency = 4)
colnames(zoo1) <- namen

# testjes: selectie met label en selectie met index:
zoo1[as.yearqtr("2012Q1"), 'ts_3400']
zoo1[128, 3400]
window(zoo1[, 'ts_3400'], start = "2012Q2", end = "2012Q2")

# kolomselectie
tm <- microbenchmark(zoo1[, 'ts_3400'], times = 1000)
tm

# kolomselectie tweede methode
tm <- microbenchmark(zoo1$ts_3400, times = 1000)
tm

# selectie van kolom en rij met labels
tm <- microbenchmark(zoo1[as.yearqtr("2012Q1"), 'ts_3400'], times = 1000)
tm

# methode met window
tm <- microbenchmark(window(zoo1$ts_3400, start = "2012Q2", end = "2012Q2"), times = 1000)
tm

# selectie van kolom en rij met index
tm <- microbenchmark(zoo1[128, 3400], times = 1000)
tm


#rijselectie
tm < microbenchmark(zoo1[128, ], times = 1000)
tm

# als dataframe

df <- as.data.frame(zoo1)

tm <- microbenchmark(df['2012 Q2', 'ts_3400'], times = 1000)
tm

tm <- microbenchmark(df[128, 3400], times = 1000)
tm

mat <- as.matrix(zoo1)

tm <- microbenchmark(mat['2012 Q2', 'ts_3400'], times = 1000)
tm

tm <- microbenchmark(mat[128,3400], times = 10000)
tm
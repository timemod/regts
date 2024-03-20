library(regts)
library(tictoc)

set.seed(12345)
aantal_variabelen <- 10000
aantal_perioden <- 30

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010")
tic("regts")
lts <- as.list(regts1)
toc()

ts1 <- as.ts(regts1)
tic("ts")
lts <- as.list(ts1)
toc()

mat1 <- as_matrix(regts1)
tic("matrix (2)")
lmat <- lapply(seq_len(ncol(mat1)), FUN = \(i) mat1[, i])
names(lmat) <- colnames(regts1)
toc()

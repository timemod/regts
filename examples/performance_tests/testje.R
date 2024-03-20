library(regts)
library(tictoc)

set.seed(12345)
aantal_variabelen <- 10000
aantal_perioden <- 30

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q1")

tic("regts_to_list")
ret <- regts:::regts_to_list(regts1)
toc()



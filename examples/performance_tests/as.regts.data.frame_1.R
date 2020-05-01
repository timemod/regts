#cpblib::use_cpblib(cpb_only = TRUE)
library(regts)
library(tictoc)


set.seed(12345)
aantal_variabelen <- 50000
aantal_perioden <- 200

namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden), ncol = aantal_variabelen)
colnames(data) <- namen

regts1 <- regts(data, start = "2010Q2")

df_num <- as.data.frame(regts1)

tic("numeric data frame to timeseries")
ts1 <- as.regts(df_num)
toc()

df_txt <- df_num
df_txt[ , 1 : (ncol(df_num) / 2)] <- lapply(df_num[ , 1 : (ncol(df_num) / 2)],
                                            FUN = as.character)

tic("data frame with texts timeseries")
ts2 <- as.regts(df_txt)
toc()


all.equal(ts1, ts2)

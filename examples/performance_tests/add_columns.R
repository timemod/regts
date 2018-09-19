library(regts)

# check adding columns to a regts. Note: working with lists is much faster

regts1 <- regts(matrix(1, nrow = 200, ncol = 100), start = "2018q1",
                names = paste0("a", 1:100))

list1 <- as.list(regts1)

cat("adding columns to a timeseries\n")
x <- microbenchmark({for (i in seq_len(1000)) {
                        naam <- paste0("x", i)
                        regts1[ , naam] <- 2
                     }}, times = 10)
print(x)
cat("\n")


cat("adding timeseries to a list\n")
x <- microbenchmark({for (i in seq_len(1000)) {
  naam <- paste0("x", i)
  list1[[naam]] <- 2
}}, times = 10)
print(x)
cat("\n")

cat("combining timeseries\n")
x <- microbenchmark(regts2 <- do.call(cbind, list1))
print(x)
cat("\n")

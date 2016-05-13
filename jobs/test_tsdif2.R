library(regts)

# create two timeseries x1 and x2
x1 <- regts(matrix(data = rep(1:27), nc = 3), start = "2008Q4",
            names = c("a", "b", "c"))
x2 <- x1 + 0.001
colnames(x2) <- c("a", "b", "d")


dif1 <- tsdif(x1, x2)
print(dif1)

# use the function regts::cvgdif (convergence difference)
dif2 <- tsdif(x1, x2, fun = cvgdif)
print(dif2)

# ignore differences smaller than 1e-4
dif3 <- tsdif(x1, x2, tol = 1e-4, fun = cvgdif)
print(dif3)

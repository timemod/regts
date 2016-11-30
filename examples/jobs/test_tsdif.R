library(regts)

# maak reeksen aan
x1 <- regts(matrix(data = rep(1:8, 3), nc = 3), start = "2008Q4",
            names = c("a", "b", "c"))
x1[, "c"] <- 2 * x1[, "a"]

x2 <- regts(matrix(data = rep(2:7, 3), nc = 3), start = "2009Q1",
            names = c("a", "b", "d")) + 0.01
x1["2010Q1", "b"] <- NA
x2["2010Q1", "b"] <- NA
x2[, "d"] <- 2 * x2[, "b"]

x2 <- x1 + 0.001

dif1 <- tsdif(x1, x2)
print(dif1)

dif2 <- tsdif(x1, x2, tol = 0.1)
print(dif2)

dif3 <- tsdif(x1, x2 - 0, fun = cvgdif, tol = 5e-4)
print(dif3)

dif4 <- tsdif(x1[, 2:3], x2)
print(dif4)

dif5 <- tsdif(x1, x2[, 2:3])
print(dif5)

dif6 <- tsdif(x1["2008Q4",], x2["2008Q4",])
print(dif6)

dif7 <- tsdif(x1[, 'a'], x2, fun = cvgdif, tol = 5e-4)
print(dif7)



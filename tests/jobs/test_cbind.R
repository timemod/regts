library(regts)

x1 <- regts(matrix(data = rep(1:6, 3), nc = 2), start = "2008Q4",
            names = c("a", "b"))
x2 <- 1000 * x1
colnames(x2) <- c("piet", "b")

x <- cbind(x1, x2)
print(x)

x <- cbind(x1, x2, suffixes = c("", "x2"))
print(x)


x <- cbind(x1, matrix(1:18, nc =  2))
print(x)

x <- cbind(x1, matrix(1:18, nc =  2), allow_dupl = TRUE)
print(x)

x <- cbind(x1, matrix(1:18, nc =  2), matrix(1:18, nc =  2), allow_dupl = TRUE)
print(x)

x1 <- regts(matrix(data = rep(1:6, 3), nc = 2), start = "2008Q4")
x2 <- x1*100
print(cbind(x1, x2))



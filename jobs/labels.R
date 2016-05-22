library(regts)

x1 <- regts(matrix(data = rep(1:6, 3), nc = 2), start = "2008Q4",
            names = c("a", "b"))
ts_labels(x1) <- c("Timeseries  a", "Timeseries b")
print(ts_labels(x1))
print(ts_labels(x1[, 2]))
print(ts_labels(x1[, 'a']))


x1 <- regts(matrix(data = rep(1:6, 3), nc = 2), start = "2008Q4",
            names = c("a", "b"))
x1 <- update_labels(x1, list(b = "Timeseries  b"))
print(ts_labels(x1))

x1 <- regts(matrix(data = rep(1:6, 3), nc = 2), start = "2008Q4",
            names = c("a", "b"), labels = c("Variable a", "Variable b"))
print(ts_labels(x1))


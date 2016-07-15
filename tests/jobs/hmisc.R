library(Hmisc)

df <- data.frame(a=1:3, b = 2:4)
label(df, self = FALSE) <- paste("Var", c("a", "b"))

df2 <- df[, 'a', drop = FALSE] + df[, 'b', drop = FALSE]
View(df2)


df3 <- within(df2, {
  c <- 2 * a
  d <- c
  e <- d
})
View(df3)


ts1 <- regts(df, "2010Q2")
ts_labels(ts1) <- label(df, self = FALSE)
print(ts1)
View(ts1)

ts2 <- ts1[, 'a'] + ts1[, 'b']
View(ts2)
l <- as.list(ts2)
l2 <- within(l, {
    c <- 2 * a
    d <- c
    e <- d
})
ts3 <- do.call(cbind, l2)

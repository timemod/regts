library(regts)

x <-regts(matrix(1:9, ncol = 3), start = "2010Q2",
          names = c("a", "b", "c"))

write_ts_csv(x, file = "test1.csv")
write_ts_csv(x, file = "test1_t.csv", rowwise = FALSE)

library(regts)

nseries <- 10000
nperiod <- 200

set.seed(123)

x <-regts(matrix(rnorm(nseries * nperiod), ncol = nseries),
          start = "2010Q2", names = paste0("x", seq_len(nseries)))

x2 <- 2 * x
colnames(x2) <- paste0("aaaaaaaaaaaaaaaaaa", colnames(x2))

t1 <- system.time({
  write_ts_xlsx(x, file = "test3_xlsx.xlsx", sheet_name = "testje",
               number_format = "#.00")
  write_ts_xlsx(x2, file = "test3_xlsx.xlsx", sheet_name = "testje2",
                 append = TRUE)

})
print(t1)



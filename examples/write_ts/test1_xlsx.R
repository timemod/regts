library(regts)

x <-regts(matrix(1:9, ncol = 3), start = "2010Q2",
          names = c("a", "b", "c"))

write_ts_xlsx(x, file = "test1.xlsx", sheet_name = "testje",
              number_format = "#.00")

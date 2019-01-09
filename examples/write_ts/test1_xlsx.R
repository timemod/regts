library(regts)

x <-regts(matrix(1:9, ncol = 3), start = "2010Q2",
          names = c("a", "b", "c"))

library(openxlsx)

write_ts_xlsx2(x, file = "test1.xlsx", sheet_name = "testje",
               number_format = "#.00")
#write_ts_xlsx2(x, file = "test1_t.xlsx", rowwise = FALSE)

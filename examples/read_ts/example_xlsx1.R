library(regts)

xlsx_file <- "xlsx/example1.xlsx"

ts1 <- read_ts_xlsx(xlsx_file, skiprow = 1, skipcol = 1, labels = "after")
print(ts1)
#View(ts1)

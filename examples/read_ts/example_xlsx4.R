library(regts)
library(readxl)

xlsx_file <- "xlsx/example4.xlsx"

ts1 <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2, labels = "after")
print(ts1)
View(ts1)


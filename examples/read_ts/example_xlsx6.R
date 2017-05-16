library(regts)
library(readxl)

xlsx_file <- "xlsx/example6.xlsx"

ts1 <- read_ts_xlsx(xlsx_file, sheet = 2, labels = "after")
print(ts1)
View(ts1)


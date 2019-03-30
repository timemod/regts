library(regts)
library(readxl)

xlsx_file <- "xlsx/example2.xlsx"

ts1 <- read_ts_xlsx(xlsx_file, skipcol = 1, sheet = 2, labels = "after",
                    strict = FALSE)
print(ts1)
print(ts_labels(ts1))


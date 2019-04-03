library(regts)

xlsx_file <- "xlsx/example1.xlsx"

ts1 <- read_ts_xlsx(xlsx_file, skiprow = 1, labels = "after",
                    strict = FALSE)
print(ts1)
#View(ts1)

library(regts)
library(cellranger)

range <- cell_limits()
range$ul[1] <- 2
range$lr[2] <- 3
print(range)
x <- read_ts_xlsx("xlsx/example1.xlsx",  strict = FALSE,
                  labels = "after",
                  range = range)
View(x)

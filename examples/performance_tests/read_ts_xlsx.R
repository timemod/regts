library(regts)
library(tictoc)

tic("read_ts_xlsx")
input <- read_ts_xlsx("input_data.xlsx", na_string = "NA")
toc()


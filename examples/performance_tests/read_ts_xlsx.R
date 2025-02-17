library(regts)
library(tictoc)

input <- read_ts_xlsx("input_data.xlsx", na_string = "NA")

tic("columnwise")
input_df_cw <- as.data.frame(input)
toc()

tic("rowwise")
input_df_rw <- as.data.frame(input, rowwise = TRUE)
toc()

tic("long format")
input_df_l <- as.data.frame(input, long = TRUE)
toc()

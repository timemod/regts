#cpblib::use_cpblib(cpb_only = TRUE)
print(.libPaths())
library(regts)
library(data.table)
library(tictoc)

csv_file <- "input_data_dec.csv"

tic()
input <- read_ts_csv(csv_file, na_string = "NA", dec = "#")
toc()

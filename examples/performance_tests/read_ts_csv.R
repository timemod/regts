library(regts)
library(data.table)

csv_file <- "examples/performance_tests/input_data.csv"

print(system.time(
  input <- read_ts_csv(csv_file, na_string = "NA")
))

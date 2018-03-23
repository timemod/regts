library(regts)

print(system.time(
  input <- read_ts_csv("input_data.csv", na_string = "NA")
))

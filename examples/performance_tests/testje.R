library(regts)

print(system.time(
  input <- read_ts_xlsx("input_data.xlsx", na_string = "NA")
))

library(regts)

print(system.time(
  input <- read_ts_xlsx("examples/performance_tests/input_data.xlsx",
                        na_string = "NA")
))

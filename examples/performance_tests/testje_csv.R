library(regts)
library(data.table)

print(system.time({
  df <- fread("input_data.csv", na.strings = "NA", header = FALSE,
              colClasses = "character", data.table = FALSE)
  names <- df[-1, 1]
  data <- df[-1, -1]
  mat <- numeric_matrix(data)
  mat <- t(mat)
  rownames(mat) <- as.character(df[1, -1])
  colnames(mat) <- names
  input2 <- as.regts(mat)
}))



print(system.time(
  input <- read_ts_csv("input_data.csv", na_string = "NA")
))

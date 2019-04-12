library(regts)


x <- read_ts_xlsx("xlsx/example5.xlsx", sheet = 2, strict = FALSE,
                  skiprow =  0, skipcol = 1)
print(x)

library(regts)
perioden <- c("2010Q1", "b", "c")
tryCatch(x <- lapply(perioden[1], FUN = regperiod), error = function(e) {})

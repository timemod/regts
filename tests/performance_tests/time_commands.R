library(microbenchmark)

time_commands <- function(commands, times = 100L, unit = "us") {
    parsed_commands <- lapply(commands, FUN = function(x) parse(text = x))
    result <- lapply(parsed_commands,
            FUN = function(x) summary(microbenchmark(eval(x), times = times,
                                                     unit = unit)))
    result <- do.call(rbind, result)
    result <- data.frame(commands, mean = result$mean)
    return(result)
}

library(microbenchmark)

time_commands <- function(commands) {
    parsed_commands <- lapply(commands, FUN = function(x) parse(text = x))
    result <- lapply(parsed_commands,
            FUN = function(x) summary(microbenchmark(eval(x)), unit = "us"))
    result <- do.call(rbind, result)
    result <- data.frame(commands, mean = result$mean)
    return(result)
}

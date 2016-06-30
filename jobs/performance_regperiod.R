library(microbenchmark)
library(regts)
library(stringr)

create_regperiod_range <- function(start, end, frequency) {
    return (structure(c(start, end, frequency), class = "regperiod_range"))
}

parse_range <- function(x, frequency = NA) {
    pos <- regexpr("/", x)
    if (pos == -1) {
        p1 <- regperiod(x, frequency)
        p2 <- p1
    } else {
        part1 <- substr(x, start = 1, stop = pos - 1)
        part2 <- substr(x, start = pos + 1, stop = nchar(x))
        #part1 <- str_trim(part1, side = "right")
        #part2 <- str_trim(part2, side = "left")
        if (nchar(part1) > 0) {
            p1 <- regperiod(part1, frequency)
        } else {
            p1 <- NULL
        }
        if (nchar(part2) > 0) {
            p2 <- regperiod(part2, frequency)
        } else {
            p2 <- NULL
        }
    }
    return (create_regperiod_range(p1, p2, frequency(p1)))
}

t <- microbenchmark(regperiod("2010Q2"))
print(t)

t <- microbenchmark(as.regperiod_range("2010Q2/2011Q3"))
print(t)

t <- microbenchmark(parse_range("2010Q2/2011Q3"))
print(t)



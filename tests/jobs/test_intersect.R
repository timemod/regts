library(regts)
library(microbenchmark)

set.seed(12345)
aantal_variabelen <- 5000
aantal_perioden <- 200

#aantal_variabelen <- 5
#aantal_perioden <- 20


namen <- paste('ts', seq(aantal_variabelen), sep = "_")
data <- matrix(rnorm(n  = aantal_variabelen * aantal_perioden),
               ncol = aantal_variabelen)
colnames(data) <- namen
regts1 <- regts(data, start = "2010Q2")
list1 <- as.list(regts1)

myMakeNamesTs <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm)) {
                seq_along(l)
            } else {
                nm == ""
            }
    dep <- sapply(l[fixup], function(x) deparse(x)[1L])
    if (is.null(nm)) {
        return(dep)
    }
    if (any(fixup)) {
        nm[fixup] <- dep
    }
    return (nm)
}


t1 <- microbenchmark(do.call(stats:::.makeNamesTs, list1), times = 1)
print(t1)
t2 <- microbenchmark(do.call(myMakeNamesTs, list1), times = 1)
print(t2)




print(stats:::.makeNamesTs(ts1, ts2))
print(myMakeNamesTs(a= ts1, ts2))

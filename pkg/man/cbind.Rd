% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbind.regts.R
\name{cbind}
\alias{cbind}
\alias{cbind.regts}
\title{Bind two or more timeseries}
\usage{
\method{cbind}{regts}(..., union = TRUE, suffixes)
}
\arguments{
\item{...}{two or more univariate or multivariate timeseries,
or objects which can be coerced to timeseries}

\item{union}{if \code{TRUE}, then the period range of the result
is the union of the period ranges of the joined objects
(the result is padded with \code{NA}s if necessary). If \code{FALSE},
then the period range of the result is the intersection of the period ranges
of the joined objects.}

\item{suffixes}{Suffixes appended to the column names for all overlapping
columns. This argument is obligatory if the timeseries have overlapping
column names. Length suffixes must be equal to the number of joined
timeseries or objects.}
}
\description{
Bind two or more timeseries objects with a common frequency.
By default, the period range of the result is the union of the period ranges
of the individual timeseries. The result is padded with \code{NA}s if
necessary. If argument \code{union} is false, then the period range
of the result is the intersection of the period ranges.
}
\examples{
a <- regts(1:5, start = "2011Q1")
b <- regts(matrix(11:15, nc = 1), start = "2011Q2")
cbind(a, b)
cbind(a, b, union = FALSE)
x1 <- regts(matrix(1:27, nc = 3), start = "2008Q4", names = c("a", "b", "c"))
x2 <- regts(matrix(1:27, nc = 3), start = "2008Q4", names = c("a", "c", "d"))
cbind(x1, x2, suffixes = c("_1","_2"))
}
\seealso{
\code{\link{as.list}}
}

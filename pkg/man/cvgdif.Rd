% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tsdif.R
\name{cvgdif}
\alias{cvgdif}
\title{Calculate the 'convergence difference'}
\usage{
cvgdif(x1, x2)
}
\arguments{
\item{x1}{first numeric vector}

\item{x2}{second numeric vector}
}
\value{
the 'convergence difference' as described above
}
\description{
\code{cvgdif} calculates the difference  between two numeric vectors
\code{x1} and \code{x2} according to \code{|x1 - x2| / max(|x2|, 1)}.
This difference is equivalent to the convergence test employed in the
package \code{isismdl}.
}
\examples{
# create two timeseries x1 and x2
x1 <- regts(matrix(data = rep(1:27), nc = 3), start = "2008Q4",
           names = c("a", "b", "c"))
x2 <- x1 + 0.001
colnames(x2) <- c("a", "b", "d")

# calculate the differences
cvgdif(x1, x2)

}
\seealso{
\code{\link{tsdif}}
}

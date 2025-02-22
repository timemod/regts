% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/movav.R
\name{movav}
\alias{movav}
\alias{movavb}
\alias{movavc}
\title{Moving average of a timeseries}
\usage{
movavb(x, order, keep_range = TRUE)

movavc(x, order, keep_range = TRUE, method = c("centre", "left", "right"))
}
\arguments{
\item{x}{a \code{\link[stats]{ts}} or \code{\link{regts}} object}

\item{order}{the order of the moving average}

\item{keep_range}{If \code{TRUE} (the default), then  the output
timeseries has the same period range as the input timeseries.
Then the result timeseries will have \code{order} NA values. For
\code{movavb} these NAs will appear on the left side and for \code{movavc}
they will be distributed over both sides.
If \code{FALSE} then the result timeseries is \code{order} periods
shorter than the input timeseries.}

\item{method}{method used to handle the centered moving average for
even orders. Possible values are \code{"centre"} (the default),
\code{"left"} and \code{"right"}. See Details. This argument is ignored for
odd orders.}
}
\value{
a \code{regts} object with the moving average values
}
\description{
Function \code{movavb} computes the backward moving average and
function \code{movavc} the centered moving average.

For example, the backward moving average of order 3 is defined as

\code{A[t] = (x[t - 2] + x[t - 1] + x[t]) / 3},

while the centered moving average of order 3 is calculated as

\code{A[t] = (x[t - 1] + x[t] + x[t + 1]) / 3}.

The calculation of the centered moving average for even orders is somewhat
more complicated, see Details.
}
\details{
The centered moving average for even orders is usually computed by
using one more observation than the order and to use weights 0.5 for the
end points. For example, for order 4 we have

\code{A[t] = (0.5 x[t - 2] + x[t - 1] + x[t] + x[t + 1] + 0.5 x[t + 2]) / 4}.

In this way the observations are distributed evenly over the past
and future. An alternative approach is to use the same number
of observations as the order but use one more observation from the past
than from the future, or the other way around. These methods can be used
by specifying argument \code{method}. Possible methods are
\describe{
\item{\code{centre}}{Standard method
e.g.  (\verb{0.5 x[t - 2] + x[t - 1] + x[t] + x[t + 1] + 0.5 x[t + 2]) / 4}}
\item{\code{left  }}{Use one more observation from the past, e.g.
(\verb{x[t - 2] + x[t - 1] + x[t] + x[t + 1]) / 4}}
\item{\code{right }}{Use one more observation from the future, e.g.
(\verb{x[t - 1] + x[t] + x[t + 1] + x[t + 2]) / 4}}
}
}
\section{Functions}{
\itemize{
\item \code{movavb()}: Backward moving average

\item \code{movavc()}: Centered moving average

}}
\examples{
x <- regts(rnorm(10), start = "2018Q1")

movavb(x, order = 3)

movavc(x, order = 3, keep_range = FALSE)
}

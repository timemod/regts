% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rel2index.R
\name{rel2index/pct2index}
\alias{rel2index/pct2index}
\alias{rel2index}
\alias{pct2index}
\title{Calculate an index timeseries from a timeseries with relative or
percentage changes.}
\usage{
rel2index(x, base = NULL, scale = 100, keep_range = TRUE)

pct2index(x, base = NULL, scale = 100, keep_range = TRUE)
}
\arguments{
\item{x}{a \code{\link[stats]{ts}} or \code{\link{regts}} (can also be a
multivariate timeseries) with the relative of percentage changes.}

\item{base}{a \code{\link{period}} or a
\code{\link{period_range}} specifying the base period, or an object that can
be coerced to a \code{period} or \code{period_range}. The (average) value
of the timeseries at the base period is set to \code{scale} (by default 100).
By default the base period is the period before the first period of the
input timeseries \code{x}. For example,  if \code{x} starts at \verb{2018q1}, then the
default base period is \verb{2017q4}. If the base period is a \code{period_range},
then the average value of the index series will be equal to \code{scale}.}

\item{scale}{the (average) value of the index series at the base period
(by default 100). This may be a negative number.}

\item{keep_range}{if \code{TRUE} (the default), then the output
timeseries has the same period range as the input timeseries.
If \code{FALSE} then the result timeseries starts 1 period earlier.}
}
\description{
Function \code{rel2index} is the inverse of function \code{\link{growth}}.
The growth rate \code{x[t]} (also called the relative change) of a timeseries
\code{z[t]} is defined as

\if{html}{\out{<div class="sourceCode">}}\preformatted{x[t] = (z[t] - z[t-1]) / z[t-1].
}\if{html}{\out{</div>}}

Note that according to this definition the denominator is not the absolute
value of \code{z[t-1]}).
The function constructs an index series for \code{z[t]} given the values of
\code{x[t]}. See Details.
\cr\cr
Function \code{pct2index} computes the index series
from a timeseries of percentage changes.
Thus expression \code{pct2index(x)} gives the same result as
\code{100 * rel2index(x / 100)}.
}
\details{
If \code{x[t]} is given but \code{z[t]} is unknown, \code{z[t]} can be calculated from
\code{z[t-1]} using

\if{html}{\out{<div class="sourceCode">}}\preformatted{z[t] = z[t-1] * (1 + x[t]).
}\if{html}{\out{</div>}}

Given an initial value for \code{z} at some period (say \code{z[0]}), the equation
above can be used repeatedly to calculate the values of \code{z[t]} for \code{t > 0}.
Finally the index series is computed using

\if{html}{\out{<div class="sourceCode">}}\preformatted{i[t] =  scale * z[t] / mean(z[base]),
}\if{html}{\out{</div>}}

where \code{base} is the base period.

\code{z[0]} is not known, but the result \code{i[t]} does not depend on \code{z[0]} and
therefore it can be arbitrarily set to 1.
}
\section{Functions}{
\itemize{
\item \code{rel2index()}: Calculates an index timeseries from a
timeseries with relative changes

\item \code{pct2index()}: Calculates an index timeseries from a
timeseries with percentage changes

}}
\examples{
ts1 <- regts(abs(rnorm(10)), start = "2010Q2")
print(rel2index(ts1))
print(rel2index(ts1, base = "2010Q3", scale = 1, keep_range = TRUE))
}
\seealso{
\code{\link{index_ts}} and \code{\link{growth}}
}

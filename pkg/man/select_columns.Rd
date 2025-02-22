% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colname_utils.R
\name{select_columns}
\alias{select_columns}
\title{Select columns using a regular expression}
\usage{
select_columns(x, regex, drop = TRUE, ...)
}
\arguments{
\item{x}{an R object with column names (e.g. a \code{data.frame},
\code{matrix}, \code{ts} or \code{regts})}

\item{regex}{a regular expression used to select a column}

\item{drop}{if \code{TRUE}, the result is coerced to a vector if possible}

\item{...}{arguments passed to function \code{\link{grep}}}
}
\value{
the column selection of object \code{x}
}
\description{
This function selects columns of an R object with column names
(for example a \code{\link[base]{data.frame}}, \code{\link[base]{matrix}},
\code{\link[stats]{ts}} or \code{\link{regts}}).
The columns with names matching a given regular expression are selected.
This function employs base R function \code{\link{grep}}
}
\examples{
data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
              names = c("nlc", "ukc", "nly", "uky"))

# select all columns with names starting with nl
nl_data <- select_columns(data, "^nl")

# select all columns except column "nlc"
no_nlc <- select_columns(data, "^nlc$", invert = TRUE)
}

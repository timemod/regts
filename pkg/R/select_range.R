#' Select a period range from a timeseries
#'
#' This function selects a part of a \code{regts} or \code{\link[stats]{ts}}
#' object based on a \code{\link{period_range}}.
#' This function is convenient for use in a pipe.
#'
#' @param x A timeseries object (a \code{regts} or a \code{ts}).
#' @param range The period range to select, as a \code{\link{period_range}}
#' object or a character string that can be converted to a \code{period_range}.
#' @return A \code{regts} object containing the selected period range.
#' @examples
#' data <- regts(1:10, start = "2010Q1")
#' select_range(data, "2010Q2/2010Q3")
#'
#' # Select data from the beginning of the year 2011 using a pipe.
#' data |> select_range("2011/")
#' @seealso [select] and [select_columns].
#' @export
select_range <- function(x, range) {
  if (!is.ts(x)) {
    stop("Argument 'x' must be a 'ts' or 'regts' object.")
  }
  if (!is.period_range(range)) {
    range <- as.period_range(range)
  }
  return(window_regts(x, range))
}

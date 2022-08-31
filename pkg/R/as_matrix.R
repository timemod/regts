#' Convert a \code{ts} to a \code{matrix}
#'
#' This function converts a \code{\link[stats]{ts}} object
#' to a normal  \code{\link[base]{matrix}}, i.e. a matrix without timeseries
#' class and attributes.The periods are stored in the row or
#' column names of the returned matrix, depending on argument \code{rowwise}.
#'
#' @details
#' The function behaves differently than base method
#' \code{\link[base]{as.matrix}}. If the input timeseries is a matrix with
#' timeseries attributes, then \code{as.matrix} just returns the input value.
#' If the input timeseries is not a matrix (a univariate timeseries with vector
#' data), then \code{as.matrix} returns a matrix without row and column names
#' and without timeseries attributes. In contrast, \code{as_matrix} always
#' returns a matrix without timeseries attributes.
#' @param x a \code{\link[stats]{ts}} or \code{\link{regts}}
#' @param rowwise a logical value: should the timeseries be stored rowwise
#' or columnwise in the matrix? Defaults to \code{FALSE}
#' @param ... additional arguments to be passed to methods.
#' @return A \code{\link[base]{matrix}}
#' @examples
#' ts <- regts(matrix(1:4, ncol = 2) , start = 2015, names = c("a", "b"))
#' as_matrix(ts, rowwise = TRUE)
#' @name as_matrix
#' @export
as_matrix <- function(x, ...) {
  UseMethod("as_matrix")
}

#' @describeIn as_matrix Coerce a \code{\link[stats]{ts}} to a matrix without
#' timeseries class and attributes
#' @export
as_matrix.ts <- function(x, rowwise = FALSE, ...) {

  if (!is.matrix(x)) {
    name <- deparse(substitute(x))
    x <- univec2unimat(x, name)
  }

  periods <- as.character(get_periods(x))

  if (rowwise) {

    ret <- t(x)
    colnames(ret) <- periods


  } else {

    ret <- unclass(x)
    attr(ret, "tsp") <- NULL

    rownames(ret) <- periods

  }

  # remove ts_labels attribute
  attr(ret, "ts_labels") <- NULL

  return(ret)
}

#' Select columns using a regular expression
#'
#' This function selects columns of an R object with column names
#' (for example a \code{\link[base]{data.frame}}, \code{\link[base]{matrix}},
#' \code{\link[stats]{ts}} or \code{\link{regts}}).
#' The columns with names matching a given regular expression are selected.
#' This function employs base R function \code{\link{grep}}
#' @param x an R object with column names (e.g. a \code{data.frame},
#' \code{matrix}, \code{ts} or \code{regts})
#' @param regex a regular expression used to select a column
#' @param drop if \code{TRUE}, the result is coerced to a vector if possible
#' @param ... arguments passed to function \code{\link{grep}}
#' @return the column selection of object \code{x}
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("nlc", "ukc", "nly", "uky"))
#'
#' # select all columns with names starting with nl
#' nl_data <- select_columns(data, "^nl")
#'
#' # select all columns except column "nlc"
#' no_nlc <- select_columns(data, "^nlc$", invert = TRUE)
#' @export
select_columns <- function(x, regex, drop = TRUE, ...) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  sel <- grep(regex, cnames, ...)
  if (length(sel) == 0) {
    # the result with drop = TRUE is weird, therefore use drop = FALSE
    # if the selection was not succesfull
    return(x[ , sel, drop = FALSE])
  } else {
    return(x[ , sel, drop = drop])
  }
}

#' Append a tag to the column names of a matrix or data frame
#'
#' This function adds a suffix to all column names
#'  of an R object.
#' @param x an R object with column names (e.g. a \code{data.frame},
#' \code{matrix}, \code{ts} or \code{regts})
#' @param tag a character
#' @return an R object with modified columm names
#' @seealso \code{\link{change_colnames}}
#' @examples
#' data <- regts(matrix(1:8, ncol = 2), start = "2010Q2", names = c("a", "b"))
#'
#' #  append the column names with a tag "_input"
#' tag_colnames(data, "_input")
#' @export
tag_colnames <- function(x, tag) {
  if (!is.vector(tag) || length(tag) > 1) {
    stop("Argument 'tag' should be a vector with length 1")
  }
  tag <- as.character(tag)
  colnames(x) <- paste0(colnames(x), tag)
  return(x)
}

#' Change the column names of a matrix or data frame by applying a function.
#' @param x an R object with column names (e.g. a \code{data.frame},
#' \code{matrix}, \code{ts} or \code{regts})
#' @param fun a function
#' @param ... arguments passed to `fun`
#' @return an R object with modified columm names
#' @examples
#' data <- regts(matrix(1:8, ncol = 2), start = "2010Q2", names = c("a", "b"))
#'
#' # convert to column names tp upper case
#' change_colnames(data, toupper)
#' @seealso \code{\link{tag_colnames}}
#' @export
change_colnames <- function(x, fun, ...) {
  if (!is.function(fun)) {
    stop("Argument 'fun' is not a function")
  }
  colnames(x) <- fun(colnames(x), ...)
  return(x)
}

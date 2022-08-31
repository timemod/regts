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
    return(x[, sel, drop = FALSE])
  } else {
    return(x[, sel, drop = drop])
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

#' Rename the columns of a matrix or timeseries.
#'
#' Function `rename_cols` renames the columns of a matrix using the
#' `new_name = old_name` syntax.
#' This works similarly as functions \code{\link[dplyr]{rename}} of the
#' `dplyr` package, but also works
#' on matrix objects (a `matrix` or `ts`) objects.
#' @param .data An R object with column names (a `matrix`, `ts`, `regts`
#' or `data.frame`)
#' @param ... Use `new_name = old_name` to rename variables.
#' @return An object with the same type and contents as `.data`, except
#' that the columns are renamed.
#' @importFrom tidyselect eval_rename
#' @importFrom rlang expr
#' @seealso \code{\link{tag_colnames}} and \code{\link{change_colnames}}
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("a1", "a2", "b1", "b1"))
#' rename_cols(data, x = a1, y = b1)
#'
#' rename_cols(data, p = 1)
#' @export
rename_cols <- function(.data, ...) {

  if (!is.matrix(.data)) {
    stop(".data is not a matrix")
  }

  cnames <- colnames(.data)
  if (is.null(cnames)) {
    cnames <- rep("", ncol(.data))
  }

  # The following code is based on dplyr::rename. Because
  # tidyselect::eval_rename uses the 'names' attribute of the data and not the
  # colnames, we create an named vector with names equal to the column names,
  # eval_rename does not look at the contents of argument data, we simply
  # create a vector of NAs
  colname_data <- rep(NA, length(cnames))
  names(colname_data) <- cnames

  loc <- tidyselect::eval_rename(expr(c(...)), colname_data)

  names <- colnames(.data)
  names[loc] <- names(loc)
  colnames(.data) <- names
  return(.data)
}

#' Select or drop columns using a regular expression or exact names
#'
#' `select_columns` selects the columns of a \code{\link{regts}} object,
#' or any other R object with column names (for example a
#' \code{\link[base]{data.frame}}, \code{\link[base]{matrix}}),
#' whose names match a given regular expression.
#'
#' `drop_columns` does the opposite: it removes the columns whose names match
#' the regular expression, keeping all non-matching columns.
#'
#' Both `select_columns` and `drop_columns` employ base R function
#' \code{\link{grep}}.
#'
#' `select_cols_by_name` selects columns by exact name.
#'
#' `drop_cols_by_name` removes columns by exact name.
#' @param x an R object with column names (e.g. a `regts`, `matrix` or
#' `data.frame`).
#' @param regex A regular expression used to select or drop columns.
#' @param names A character vector of exact column names to select or drop.
#' @param strict A logical: if \code{TRUE} (the default), an error is raised
#' when any element of \code{names} is not a column name of \code{x}.
#' @param drop A logical: if \code{TRUE}, the result is coerced to a vector if
#' the result has a single column. (Only relevant for `select_columns`.)
#' @param ... arguments passed to function \code{\link{grep}}.
#' @return The column selection (`select_columns`, `select_cols_by_name`) or
#' the object with the specified columns removed
#' (`drop_columns`, `drop_cols_by_name`).
#' @examples
#'
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("nlc", "ukc", "nly", "uky"))
#'
#' # select all columns with names starting with "nl"
#' nl_data <- select_columns(data, "^nl")
#'
#' # select all columns except column "nlc" (using invert)
#' no_nlc <- select_columns(data, "^nlc$", invert = TRUE)
#'
#' # drop all columns with names starting with "nl"
#' no_nl <- drop_columns(data, "^nl")
#'
#' # drop only column "nlc"
#' no_nlc2 <- drop_columns(data, "^nlc$")
#'
#' # select columns "nlc" and "ukc" by exact name
#' sel <- select_cols_by_name(data, c("nlc", "ukc"))
#'
#' # drop columns "nlc" and "ukc" by exact name
#' dropped <- drop_cols_by_name(data, c("nlc", "ukc"))
#' @name select_columns
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

#' @rdname select_columns
#' @export
drop_columns <- function(x, regex, ...) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  sel <- grep(regex, cnames, invert = TRUE, ...)
  return(x[, sel, drop = FALSE])
}

#' @rdname select_columns
#' @export
select_cols_by_name <- function(x, names, strict = TRUE) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  if (strict) {
    missing <- setdiff(names, cnames)
    if (length(missing) > 0) {
      stop(paste0(
        "The following names are not column names of x: ",
        paste(missing, collapse = ", ")
      ))
    }
  }
  sel <- intersect(cnames, names)
  return(x[, sel, drop = FALSE])
}

#' @rdname select_columns
#' @export
drop_cols_by_name <- function(x, names, strict = TRUE) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  if (strict) {
    missing <- setdiff(names, cnames)
    if (length(missing) > 0) {
      stop(paste0(
        "The following names are not column names of x: ",
        paste(missing, collapse = ", ")
      ))
    }
  }
  keep <- setdiff(cnames, names)
  return(x[, keep, drop = FALSE])
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
#' @param ... 	Use `new_name = old_name` to rename selected
#' variables. Alternatively, specify a named character vector (see Examples).
#' @return An object with the same type and contents as `.data`, except
#' that the columns are renamed.
#' @importFrom tidyselect eval_rename
#' @importFrom rlang expr
#' @seealso \code{\link{tag_colnames}} and \code{\link{change_colnames}}
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("a1", "a2", "b1", "b1"))
#'
#' # Use new_name = old_name syntax
#' rename_cols(data, x = a1, y = b1)
#'
#' # Use named character vector
#' rename_cols(data, c(x = "a1", y = "b1"))
#'
#' # Rename the first column (new column name is 'p'.)
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

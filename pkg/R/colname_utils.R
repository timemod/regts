#' Select or drop columns by regex or name
#'
#' These functions select or remove columns from an R object with column names
#' (such as a [regts], [base::matrix] or [base::data.frame]).
#' Two approaches are supported: matching by regular expression
#' (`select_columns`, `drop_columns`) and matching by exact name
#' (`select_cols_by_name`, `drop_cols_by_name`).
#'
#' The regex-based functions use [base::grep] internally, so any argument
#' accepted by [base::grep] (e.g. `ignore.case`, `invert`) can be passed
#' via `...`.
#' The name-based functions return columns in the  order of
#' the `names` argument.
#' @param x An R object with column names (e.g. a `regts`, `matrix` or
#'   `data.frame`).
#' @param regex A regular expression used to match column names.
#' @param drop A logical: if `TRUE` (default), the result is coerced to a
#'   vector when only one column is selected. If no columns match, the result
#'   always has `drop = FALSE`. This argument is only available
#'   for `select_columns`, for other functions the result is never coerced
#'   to a vector.
#' @param names A character vector of exact columns names to select or drop.
#'   Duplicate names are silently removed.
#' @param strict A logical: if `TRUE` (default), an error is raised when any
#'   element of `names` is not present as a column name in `x`.
#' @param ... Additional arguments passed to [base::grep].
#' @return An object of the same type as `x` containing the selected columns
#'   (`select_columns`, `select_cols_by_name`), or `x` with the matched
#'   columns removed (`drop_columns`, `drop_cols_by_name`).
#' @examples
#' data <- regts(matrix(1:20, ncol = 4), start = "2010Q2",
#'               names = c("nlc", "ukc", "nly", "uky"))
#'
#' # Select all columns whose names start with "nl"
#' select_columns(data, "^nl")
#'
#' # Select all columns except "nlc" (using grep's invert argument)
#' select_columns(data, "^nlc$", invert = TRUE)
#'
#' # Drop all columns whose names start with "nl"
#' drop_columns(data, "^nl")
#'
#' # Select columns "nlc" and "ukc" by exact name
#' select_cols_by_name(data, c("nlc", "ukc"))
#'
#' # Drop columns "nlc" and "ukc" by exact name
#' drop_cols_by_name(data, c("nlc", "ukc"))
#' @name select_columns
NULL

#' @describeIn select_columns Select columns whose names match a regular
#'   expression.
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

#' @describeIn select_columns Drop columns whose names match a regular
#'   expression.
#' @export
drop_columns <- function(x, regex, ...) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  sel <- grep(regex, cnames, invert = TRUE, ...)
  return(x[, sel, drop = FALSE])
}

#' @describeIn select_columns Select columns by exact name.
#' @export
select_cols_by_name <- function(x, names, strict = TRUE) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  if (anyDuplicated(cnames)) {
    dupl <- cnames[duplicated(cnames)]
    warning("Duplicate column names (",
            paste(dupl, collapse = ", "),
            "). The first column(s) will be selected.")
  }
  names <- unique(names)
  if (strict) {
    missing <- setdiff(names, cnames)
    if (length(missing) > 0) {
      stop(paste0(
        "The following names are not column names of x: ",
        paste(missing, collapse = ", ")
      ))
    }
  }
  sel <- intersect(names, cnames)
  return(x[, sel, drop = FALSE])
}

#' @describeIn select_columns Drop columns by exact name.
#' @export
drop_cols_by_name <- function(x, names, strict = TRUE) {
  cnames <- colnames(x)
  if (is.null(cnames)) {
    stop("No column names available. No selection possible")
  }
  names <- unique(names)
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

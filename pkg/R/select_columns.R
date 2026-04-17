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
#'   is never coerced to a vector, but to an object with zero columns.
#    This argument is only available
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
#'   If `select_columns` was used with `drop = TRUE` and a single columns
#'   was selected, the result may be a different class than `x`, for
#'   example a `numeric` when `x`.
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
#' @seealso [select].
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

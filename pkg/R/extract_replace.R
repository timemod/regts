#' Extract of Replace Parts of a `regts` object.
#'
#' Operators acting on regts objects to extract or replace parts.
#'
#' @usage
#'
#' # Select parts of a regts.
#' x[i]
#' x[i, j, drop = TRUE]
#' x$name
#'
#' # Select parts of a regts as a plain vector or matrix:
#' x[[i]]
#' x[[i, j, drop = TRUE]]
#'
#' x[i] <- value
#' x[i, j] <- value
#' x[[j]] <- value
#' x$name <- "piet"
#'
#' @section Arguments:
#' \describe{
#'   \item{i}{Indices for the first dimension.
#'            A numeric vector or a \code{\link{period}},
#'            \code{\link{period_range}} or an character that be coerced to
#'            a `period` or `period_range` object. The frequency of the
#'            specified period or period range must be equal to or lower than
#'            the frequency of the timeseries}
#'   \item{j}{A numeric vector with column indices or a character vector with
#'            column names. Only used in the underlying data of the timeseries
#'            is a matrix.}
#'   \item{drop}{Relevant for extraction if the underlying data of the
#'               timeseries is a  matrix and if argument `j` has length 1. In
#'               that case, the timeseries is converted to a vector timeseries
#'               (dropping the column names) if `drop = TRUE`.
#'               This argument is only used for extraction, not for
#'               replacement. Specify `drop = FALSE` if you want to retain
#'               the column names.}
#' }
#'
#' @section Details:
#'
#' The `[` operator can be used to extact part of a `regts` object. When
#' combined with the assignment operator `<-`, it can also be used to
#' replace part of a regts object, or to extend a regts (see the examples below).
#' For `regts` objects, the `[[` works similarly as the `[`
#' operator, except that it always returns a normal vector or matrix and not a
#' timeseries.
#'
#' **Vector timeseries**
#'
#' A vector timeseries is  a univariate timeseries for which the underlying
#' data is a vector (and not a matrix). A vector timeseries only has one
#' dimension and only argument `i` is relevant.
#'
#' If `i` is a numeric vector, the result is a vector (not a timeseries)
#' with the observations at the specified indices.
#' If `i` is a `period`, `period_range`, or a character that can
#' be coerced to a `period` or `period_range`,  the result is a timeseries
#' with a period equal to the specified period (range). The result is padded
#' with NA values if the specified period or period range lies outside the
#' period range of the input timeseries.
#'
#' The result of the `[[` operator is always a vector.
#'
#'
#' **Matrix timeseries**
#'
#' For a matrix timeseries the underlying data is a matrix, usually with
#' column  names. The first selection argument `i` can be used to
#' select rows or a period range, similarly as argument `i` is used the
#' select elements of vector timeseries.
#' If `i` is a numerical vector, the result is a normal matrix (not a
#' timeseries), if `i` is a `period` or `period_range` the result is a
#' timeseries.
#'
#' For matrix timeseries, argument `j` can be used to select columns.
#' When used in combination with the `<-` operator I can be used to add
#' columns. If `j` has length 1, the result is a vector timeseries
#' unless `drop = FALSE`.

#' The result of the `[[` operator is always a plain matrix (not a timeseries).
#'
#' @examples
#'
#' # univariate timeseries
#' #######################
#'
#' x <- regts(1:5, start = "2018q1")
#' x[c(1, 3)]
#' x["2018q2"] # select period 2018q2
#' x[["2018q2"]] # get value in 2018q2 as numeric,
#'
#' x["2018"]  # select all quarters in the year 2018
#' x[["2018"]] # get values for the year 2018 as numeric vector
#'
#' # extend the period of the timeseries:
#' x["2020q1"] <- 12
#' x
#'
#' # multivariate timeseries
#' #######################
#'
#' x <- regts(matrix(1:10, ncol = 2), names = c("a", "b"), start = "2018q1")
#'
#' x[c(1, 3)]
#' x["2018q2"] # select period 2018q2
#' x[["2018q2"]] # get value in 2018q2 as numeric,"
#'
#' x["2018", "b"]   # select all quarters of column b in the year 2018
#' x[["2018", "b"]] # get values for the year 2018 as numeric vector
#'
#' # now keep column names
#' x["2018", "b", drop = FALSE]
#' x[["2018", "b", drop = FALSE]]
#'
#' # Extend the period range of the timeseries:
#' x["2019q3"] <- 12
#' x
#'
#' # Extend the period range and add a new column
#' x["2019q4", "c"] <- 12
#' x
#'
#' x$b   # Select columb b
#'
#' x$d <- 2  # Add a new column d, with value 2
#' x
#' @name extract
#' @aliases [ [[ [<- $ $<


NULL

# Use this command to prevent error from lintr about
# No visible binding for global variable .Generic
utils::globalVariables(".Generic")

# Selection on the right-hand-side (e.g. x["2010Q2", ]).
#' @importFrom stats is.ts
#' @export
"[.regts" <- function(x, i, j, drop = TRUE) {

  j_missing <- missing(j)

  # save function call for error handling
  func_call <- sys.call()

  tryCatch({
    if (missing(i)) {
      lbls <- ts_labels(x)
      if (!is.null(lbls) && !missing(j)) {
        lbls <- lbls[j]
      }
      if (is.matrix(x) && nrow(x) == 1 && (missing(j) || length(j) > 1)) {
        # the result is very weird is the timeseries has a single row
        # and if drop = TRUE is used
        ret <- NextMethod(.Generic, drop = FALSE)
      } else {
        ret <- NextMethod(.Generic)
      }
      ret <- as.regts(ret)
      if (!is.null(lbls)) attr(ret, "ts_labels") <- unname(lbls)
      return(ret)
    } else {
      # row selection present
      if (is.character(i) || inherits(i, "period") ||
            inherits(i, "period_range")) {
        # first select columns
        if (!missing(j)) {
          x <- x[, j, drop = drop]
        }
        # the row selector is a period_range. Use window_regts
        return(window_regts(x, as.period_range(i)))
      } else  {
        # numeric / logical row selection: the result is a  matrix or vector
        # (no longer a ts)
        return(NextMethod(.Generic))
      }
    }
  }, warning = function(w) {
    warning(w)
  }, error = function(err) {
    if (!j_missing && is.character(j) &&
          err$message == "subscript out of bounds") {
      missing_cols <- setdiff(j, colnames(x))
      message <- paste0("Undefined columns: ",
                        paste(missing_cols, collapse = ", "), ".")
      message_lines <- strwrap(message, width = 80)
      message <- paste(message_lines, collapse = "\n")
      stop(simpleError(message, call = func_call))
    } else {
      stop(err)
    }
  })
}

# Selection on the left-hand side: replace a part of a regts
# (e.g. x["2010Q2", ] <- 2).
#' @importFrom stats is.mts
#' @export
"[<-.regts" <- function(x, i, j, value) {

  if (is.null(value) && is.matrix(x)) {
    # remove columns
    if (!missing(i)) {
      stop("Row selection not allowed when the replacement is NULL")
    }
    if (missing(j)) {
      # x[] <- NULL: remove all columns
      return(x[, numeric(0)])
    } else {
      if (length(j) == 0) {
        return(x)
      }
      if (is.numeric(j)) {
        return(x[, -j, drop = FALSE])
      } else if (is.logical(j)) {
        return(x[, !j, drop = FALSE])
      } else {
        colsel <- match(as.character(j), colnames(x))
        colsel <- colsel[!is.na(colsel)]
        if (length(colsel) > 0) {
          return(x[, -colsel, drop = FALSE])
        } else {
          # no matching columns, do nothing
          return(x)
        }
      }
    }
  }

  if (!missing(j) && is.character(j)) {
    # Check if j contains names of columns not present in x.
    # Add missing columns if necessary
    cnames <- colnames(x)
    if (is.null(cnames)) {
      stop("object has no column names")
    }
    new_colnames <- setdiff(j, cnames)
    if (length(new_colnames) > 0) {
      x <- add_columns(x, new_colnames)
    }
  }

  if (!missing(i) && (is.character(i) || inherits(i, "period") ||
                        inherits(i, "period_range"))) {

    # call C++ function get_period_range
    ts_range <- get_period_range(x)

    sel_range <- convert_selection_range(as.period_range(i), ts_range)
    if (sel_range[1] < ts_range[1] || sel_range[2] > ts_range[2]) {
      ts_range <- c(min(sel_range[1], ts_range[1]),
                    max(sel_range[2], ts_range[2]), ts_range[3])
      x <- window_regts(x, ts_range)
    }
    i <- seq(sel_range[1] - ts_range[1] + 1,
             length.out = nperiod__(sel_range))
    # if argument j is missing, then we have to add an empty
    # column selection. x[i] does not return the same as x[i, ].
    if (missing(j) && is.mts(x)) {
      x[i, ] <- value
      return(x)
    }
  }
  return(NextMethod("[<-"))
}

#' @export
"$.regts" <- function(object, x) {
  if (!is.matrix(object)) stop("$ operator not possible for vector timeseries")
  cnames <- colnames(object)
  if (is.null(cnames)) stop("$ operator not possible for regts without ",
                            "column names")
  i <- match(x, cnames)
  if (is.na(i)) {
    return(NULL)
  } else {
    return(object[, i])
  }
}

#' @export
"$<-.regts" <- function(object, x, value) {
  if (!is.matrix(object)) stop("$ operator not possible for vector timeseries")
  cnames <- colnames(object)
  if (is.null(cnames)) stop(paste("$ operator not possible for regts without",
                                  "column names"))

  if (is.null(value)) {
    i <- pmatch(x, cnames)
    if (!is.na(i)) {
      object <- object[, -i, drop = FALSE]
    }
  } else {
    object[, x] <- value
  }

  return(object)
}

# Extract a part of a `regts` as a plain matrix or vector.
#' @export
"[[.regts" <- function(x, ...) {
  attr(x, "ts_labels") <- NULL # remove labels
  ret <- `[`(x, ...)
  if (is.regts(ret)) {
    ret <- unclass(ret)
    attr(ret, "tsp") <- NULL
  }
  return(ret)
}

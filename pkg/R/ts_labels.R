#' Timeseries labels
#'
#'Retrieve or set labels for the timeseries. Timeseries labels
#'can be used to give a description of the contents of the timeseries.
#'
#'@param x a \code{\link{regts}}
#'@param value a character vector with the labels or \code{NULL}. The length
#'should be equal to the number of columns. Specify \code{NULL} to remove all
#'labels.
#'@return The retrieval function \code{ts_labels()} returns a named character
#'vector: the names are the timeseries names (the column names) and the values
#'the corresponding labels. The replacement method returns a \code{\link{regts}}
#'object with labels.
#'@examples
#'ts <- regts(matrix(1:6, ncol = 2), start = "2016Q2", names = c("a", "b"))
#'ts_labels(ts) <- c("Timeseries a", "Timeseries b")
#'print(ts_labels(ts))
#'
#' # print the column names and labels as a nice data.frame
#' print(as.data.frame(ts_labels(ts)))
#' @describeIn ts_labels Retrieve timeseries labels
#' @seealso
#' \code{\link{regts}}, \code{\link{update_ts_labels}}
#' @export
ts_labels <- function(x) {
  labels <- attr(x, "ts_labels")
  if (!is.null(labels)) {
    names(labels) <- colnames(x)
  }
  return(labels)
}

#' @describeIn ts_labels Sets the timeseries labels
#' @export
`ts_labels<-` <- function(x, value) {
  if (!is.null(value)) {
    if (!is.character(value)) {
      stop("value should be a character vector")
    }
    if (length(value) != NCOL(x)) {
      stop(paste("The length of the labels argument should be equal",
                 "to the number of columns"))
    }
  }
  # Internally, the labels should be stored as un unnamed vector,
  # since the column names may be modified with `colnames<-` function.
  # Use as.vector to remove name attributes if present.
  attr(x, "ts_labels") <- as.vector(value)
  return (x)
}

#' Update one or more timeseries labels in a multivariate \code{\link{regts}} object
#'
#' @param x a multivariate \code{regts} object
#' @param labels a named character vector. The names are the names
#' of the timeseries (columns) whose label will be updated.
#' Specify \code{NULL} to remove all labels.
#' @examples
#' ts <- regts(matrix(1:6, ncol = 2), start = "2016Q2", names = c("a", "b"),
#'              labels = c("Timeseries a", "???"))
#' ts <- update_ts_labels(ts, c(b = "Timeseries b"))
#' print(ts_labels(ts))
#'
#' @seealso \code{\link{ts_labels}}
#' @export
update_ts_labels <- function(x, labels) {
  if (is.null(labels)) {
    ts_labels(x) <- NULL
    return (x)
  }
  if (is.null(colnames(x))) {
    stop(paste("x does not have column names. update_labels requires a",
               "regts object with named columns"))
  }
  lbls <- ts_labels(x)
  if (is.null(lbls)) {
    lbls <- rep("", NCOL(x))
    names(lbls) <- colnames(x)
  }
  sel <- which(colnames(x) %in% names(labels))
  lbls[sel] <- labels[colnames(x)[sel]]
  ts_labels(x) <- lbls
  return (x)
}

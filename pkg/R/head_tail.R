#' Printing head of a regts
#'
#' With this function the head of a regts can be printed. The generic function
#' \code{\link{head}} is extended with parameters \code{method} and \code{size}.
#' @param method if \code{first} then the first part of the timeseries is printed,
#' otherwise (\code{last}) the last part of the timeseries is printed.
#' @param size is the number of timeseries that are printed. Default = 10.
#' @param ...	arguments passed to \code{\link{head}}.
#' @examples
#' data <- regts(matrix(1:200, ncol = 20), start = "2010Q2",
#'               names = paste0("abc", 1:20))
#' head(data)
#' head(data, method = "last", size = 5)
#' @seealso
#' \code{\link{tail.regts}}

#' @export
head.regts <- function(x, method = c("first","last"), size = 10, ...) {

  method <- match.arg(method)

  if (method == "first") {
    x <- as.data.frame(x[,1:size])
  }
  else {
    p <- max(NCOL(x)-size+1,0)
    x <- as.data.frame(x[,p:NCOL(x)])
  }
  NextMethod("head", .Generic)
}

#' Printing tail of a regts
#'
#' With this function the tail of a regts can be printed. The generic function
#' \code{\link{tail}} is extended with parameters \code{method} and \code{size}.
#' @param method if \code{first} then the first part of the timeseries is printed,
#' otherwise (\code{last}) the last part of the timeseries is printed.
#' @param size is the number of timeseries that are printed. Default = 10.
#' @param ...	arguments passed to \code{\link{tail}}.
#' @examples
#' data <- regts(matrix(1:200, ncol = 20), start = "2010Q2",
#'               names = paste0("abc", 1:20))
#' tail(data)
#' tail(data, method = "last", size = 5)
#' @seealso
#' \code{\link{head.regts}}
#' @export
tail.regts <- function(x, method = c("first","last"), size = 10, ...) {

  method <- match.arg(method)

  if (method == "first") {
    x <- as.data.frame(x[,1:size])
  }
  else {
    p <- max(NCOL(x)-size+1,0)
    x <- as.data.frame(x[,p:NCOL(x)])
  }
  NextMethod("tail", .Generic)
}


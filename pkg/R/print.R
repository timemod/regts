#' printobj print the name, class and value of an object
#'
#' This function prints the name, class and value of its argument.
#' The value is printed using the standard \code{\link{print}} function.
#' It returns the value of the argument invisbly
#' @param  x an R object
#' @param ... further arguments passed to \code{print}
#' @export
#' @examples
#' x <- regts(1:5, start = "2017Q2")
#' printobj(x)
#' printobj(2 * x)
printobj <- function(x, ...) UseMethod("printobj")

#' @export
printobj.ts <- function(x, ...) {
  cat(paste0("variable : ", deparse(substitute(x))), "\n")
  cat(paste0("class    : ", paste(class(x), collapse = " "), "\n"))
  cat(paste0("period   : ", as.character(get_period_range(x)), "\n"))
  return(print(x, ...))
}

#' @export
printobj.default <- function(x, ...) {
  cat(paste0("variable : ", deparse(substitute(x))), "\n")
  cat(paste0("class    : ", paste(class(x), collapse = " "), "\n"))
  return(print(x, ...))
}

#' @export
print.regts <- function(x, ...) {

  # do not print ts_labels
  ts_labels(x) <- NULL

  f <- frequency(x)

  if (is.matrix(x)) {
    print_mat <- x[1:nrow(x), , drop = FALSE]
    first_period <- start_period.ts(x)
    periods <- sapply(0 : (NROW(x) - 1),
                      FUN = function(i) as.character(first_period + i))
    rownames(print_mat) <- periods
    return(print(print_mat, ...))
  } else  if (f  == 4 || f == 12) {
    # print.ts prints quartely and monthly timeseries nicely, therefore use the
    # standard method for ts objects
    return(NextMethod(.Generic))
  } else {
    return(print_vec_regts(x, ...))
  }
}

print_vec_regts <- function(x, ...) {
  first_period <- start_period.ts(x)
  periods <- sapply(0 : (NROW(x) - 1),
                    FUN = function(i) as.character(first_period + i))
  values <- as.numeric(x)
  names(values) <- periods
  return(print(values))
}

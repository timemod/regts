#' Print the name, class and value of an object
#'
#' This function prints the name, class and value of its argument.
#' The value is printed using the standard \code{\link{print}} function.
#' It returns the value of the argument invisibly
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
    # print.ts prints quarterly and monthly timeseries nicely, therefore use the
    # standard method for ts objects
    return(NextMethod(.Generic))
  } else if (f == 1) {
    return(print_vec_year(x, ...))
  } else {
    return(print_vec_regts(x, ...))
  }
}

# function for nice printing of yearly vector timeseries
print_vec_year <- function(x, ...) {

  start_y <- get_year(start_period(x))
  end_y <- get_year(end_period(x))

  row_header_width <- max(nchar(as.character(start_y:end_y)))

  # get width available for printing timeseries values
  width <- options()$width -  (row_header_width + 1)

  formatted_data <- format(x, ...)

  # Determine the columns width. All strings of the vector
  # returned by function format have the same length, except if argument trim
  # has been specified. For safety we determine the maximum length of the formatted
  # data, and use this to calculate the number of columns.
  col_width <- max(nchar(formatted_data))
  # the columns width should be at least 4, because of the columns names.
  # we columns names for column 2 and further will be " + 2", "+ 3", etc.
  col_width <- max(4, col_width)

  ny <- end_y - start_y + 1

  # number of columns for the output data
  ncol <- floor(width / (col_width + 1))
  ncol <- max(1, ncol)
  ncol <- min(ncol, ny)

  # if the number of columns is 5 or more, then we use either 5 or 10 columns
  if (ncol < ny) {
    if (ncol >= 5 && ncol < 10) {
      ncol <- 5
    } else if (ncol > 10) {
      ncol <- 10
    }
  }

  if ((ncol == 5 || ncol == 10) && ny > ncol) {
    # fore decades and half-decades we want to start at the beginning
    # of a decade/half decade
    start_y_mat <- floor(start_y / ncol) * ncol
    end_y_mat   <- ceiling((end_y + 1)/ ncol) * ncol - 1
  } else {
    start_y_mat <- start_y
    ny_mat <- ceiling(ny / ncol) * ncol
    end_y_mat <- start_y_mat +  ny_mat - 1
  }

  gap1 <- start_y - start_y_mat
  gap2 <- end_y_mat - end_y
  mat_data <- c(rep("", gap1), formatted_data, rep("", gap2))
  mat <- matrix(mat_data,  ncol = ncol, byrow = TRUE)
  rownames(mat) <- seq(from = start_y_mat, by = ncol, length.out = nrow(mat))
  if (ncol > 1) {
    colnames(mat) <- c("", paste0(" +", 1:(ncol - 1)))
  } else {
    colnames(mat) <- ""
  }
  print(mat, quote = FALSE, right = TRUE, ...)
  return(invisible(x))
}

# Function for printing vector timeseries, currently only used
# for frequencies not equal to 1, 4 and 12).
# The resulting is not very nice, this could be improved.
print_vec_regts <- function(x, ...) {
  first_period <- start_period.ts(x)
  periods <- sapply(0 : (NROW(x) - 1),
                    FUN = function(i) as.character(first_period + i))
  values <- as.numeric(x)
  names(values) <- periods
  return(print(values))
}



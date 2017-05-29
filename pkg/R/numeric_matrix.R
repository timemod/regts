#' Convert all non-numeric columns of a dataframe to numerical columns.
#'
#' This function converts all non-numeric columns of a data frame to
#' numerical columns. Integer columns are left unchanged. If the data frame
#' contains character columns with texts that cannot be converted to
#' a numerical value, then this function gives a warning with a list
#' of texts that could not be converted.
#' @param x a data frame
#' @param dec decimal separator in number strings. The default is \code{"."}
# TODO: also handle thousand separator
#' @examples
#' df <- data.frame(a = c("1.123", "x", NA), b = c("1", "", "john"),
#'                  c = 10 : 12)
#' numeric_data_frame(df)
#' @export
numeric_matrix <- function(x, dec = ".") {

  # NOTE: we do not use function data.matrix, this is too slow

  # save row and column names
  row_names <- rownames(x)
  col_names <- colnames(x)

  #  get rid of factors, Dates etc.
  convert_col <- function(x) {
    if (is.numeric(x) | is.character(x) | is.logical(x)) {
      return(x)
    } else {
      return(as.character(x))
    }
  }
  x <- as.data.frame(lapply(x, FUN = convert_col), stringsAsFactors = FALSE)

  to_numeric_1 <- function(x) {
    if (is.numeric(x)) {
      return(x)
    } else {
      return(as.numeric(x))
    }
  }

  to_numeric_2 <- function(x) {
    if (is.numeric(x)) {
      return(x)
    } else if (is.character(x)) {
        # TODO: also handle thousand separator / grouping separator
        return(as.numeric(sub(dec, ".", x, fixed = TRUE)))
    } else {
      return(as.numeric(x))
    }
  }

  if (dec != ".") {
    to_numeric <- to_numeric_2
  } else {
    to_numeric <- to_numeric_1
  }

  warn_msg <- "NAs introduced by coercion"

  warn_function <- function(w) {
    msg <- w$message
    if (msg == warn_msg) {
      convert_problem <<- TRUE
    } else {
      warning(msg)
    }
  }


  convert_cmd <- parse(text = paste("x2 <- sapply(x,",
                                    "FUN = to_numeric)"))

  convert_problem <- FALSE

  tryCatch(eval(convert_cmd), warning = warn_function)

  if (convert_problem) {

    x2 <- suppressWarnings(eval(convert_cmd))

    # print message about texts that could not be converted

    is_char <- sapply(x, FUN = is.character)
    x_test <- x[, is_char]
    has_text_f <- function(x) {
      x <- trimws(x)
      return(!(is.na(x) | x == ""))
    }
    has_text <-sapply(x_test, FUN = has_text_f)
    problem <- has_text & is.na(x2[, is_char])
    weird_texts <- x_test[problem]
    nweird <- length(weird_texts)
    NWEIRD_MAX <- 10
    nmax <- min(NWEIRD_MAX, nweird)
    weird_texts <- paste0("\"", weird_texts[1:nmax], "\"")

    if (nweird <= NWEIRD_MAX) {
      warning(paste0("NAs introduced by coercion\n",
                    "The following texts could not be converted to numeric:\n",
                    paste0(weird_texts, collapse = "\n")))
    } else {
      warning(paste0("NAs introduced by coercion.\n",
                     nweird, " texts could not be converte to numeric.\n",
                     "The first ", NWEIRD_MAX, " texts that gave problems are:\n",
                     paste0(weird_texts, collapse = "\n")))
    }
  }

  # restore row and column names
  rownames(x2) <- row_names
  colnames(x2) <- col_names

  return(x2)
}

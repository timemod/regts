#' Transpose a \code{data.frame}
#'
#' The function transposes a \code{\link[base]{data.frame}}. If the data frame contains
#' labels that have been set by the function \code{\link[Hmisc]{label}} of the
#' package \code{Hmisc}, then the first column of the returned data frame will
#' contain the labels. Conversely, you can specify the column that contains
#' the labels of the transposed data frame.
#'
#' @param x a data frame
#' @param colname_column the name or the index of the column that contains
#' the column names of the transposed data frame. By default the
#' row names of the original data frame are used as column names of the new data
#'  frame.
#' @param label_column a numeric or character vector with the
#' indices or the names of the columns that contains
#' the row labels. If this is a vector of
#' length larger than 1, then the texts in the columns are combined
#' to create single labels for the columns of the transposed data frame.
#' By default labels are ignored.
#' @return the transposed data frame
#' @examples
#' df <- data.frame(variables = c("a", "b"),
#'                  labels = c("Variabele a", "Variable b"),
#'                  x = 1:2, y = 10:11)
#' df_t <- transpose_df(df, colname_column = 1, label_column = 2)
#' print(df_t)
#' print(transpose_df(df_t))
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @export
transpose_df  <- function(x, colname_column, label_column) {

  is_data_table <- is.data.table(x)
  x <- as.data.frame(x)

  if (!missing(colname_column)) {
    if (is.character(colname_column)) {
      colname_column <- which(colnames(x) %in% colname_column)
    }
    new_colnames <- as.character(x[[colname_column]])
    columns_to_remove <- colname_column
  } else {
    columns_to_remove <- integer()
  }

  if (!missing(label_column) && length(label_column) > 0) {
    if (is.character(label_column)) {
      label_column <- which(colnames(x) %in% label_column)
    }
    labels <- x[, label_column, drop = FALSE]
    l <- lapply(labels, get_strings)
    labels <- do.call(paste, l)
    labels <- trimws(labels)
    columns_to_remove <- c(columns_to_remove, label_column)
    has_labels <- TRUE
  } else {
    has_labels <- FALSE
  }

  # remove columns
  if (length(columns_to_remove) > 0) {
    x <- x[-columns_to_remove]
  }

  old_labels <- as.character(Hmisc::label(x))

  # We need a special treatments for data frames with both numerical and
  # character columns. The function t (transpose) first converts data frame x
  # to a matrix with function as.matrix and then transposes that matrix.
  # If some columns in x are character vectors then as.matrix returns a
  # character matrix. However, during this conversion precision is lost:
  # only about 6 digits are preserved. Therefore, in that case we should convert
  # the numerical columns to character columns with function as.character.
  # as.character does not loose precision.
  is_num <- sapply(x, FUN = is.numeric)
  is_text <- sapply(x, FUN = function(x) {is.character(x) | is.factor(x)})
  if (any(is_text) && any(is_num)) {
    x[is_num] <- lapply(x[is_num], FUN = as.character)
  }

  ret <- as.data.frame(t(x), stringsAsFactors = FALSE)

  if (!missing(colname_column)) {
    colnames(ret) <- new_colnames
  }
  if (has_labels) {
    Hmisc::label(ret, self = FALSE) <- labels
  }

  if (any(nchar(old_labels, type = "bytes") > 0)) {
    ret <- cbind(labels = old_labels, ret, stringsAsFactors = FALSE)
  }

  if (is_data_table) {
    # data.tables do not support rownames, so create another column with names
    names <- rownames(ret)
    rownames(ret) <- NULL
    ret <- cbind(names = names, ret, stringsAsFactors = FALSE)
    ret <- as.data.table(ret)
  }

  return (ret)
}

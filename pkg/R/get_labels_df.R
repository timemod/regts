# Returns a character vector with the labels of the data frame.
get_labels_df <- function(df) {

  get_column_label <- function(x) {
    ret <- attr(x, "label")
    if (is.null(ret)) {
      return("")
    } else {
      return(ret)
    }
  }

  return(sapply(df, FUN = get_column_label, USE.NAMES = FALSE))
}

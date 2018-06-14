# Internal functions for adding labels to a data frame and retrieving them.
# Labels are visisble in the data viewer.

# Sets the labels on a data frame.
set_labels_df <- function(df, labels) {

  set_column_label <- function(i) {
    attr(df[[i]], "label") <<- labels[i]
    return()
  }
  lapply(seq_len(ncol(df)), FUN = set_column_label)
  return(df)
}

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

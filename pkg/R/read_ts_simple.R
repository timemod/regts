# a simplified version of read_ts that ignores row and column names
# this version can be used by read_ts_csv or read_ts_xlsx
read_ts_simple <- function(df, columnwise, frequency = NA,
                           labels = c("no", "after", "before")) {

  labels <- match.arg(labels)

  # remove all columns with only NAs
  all_na <- unlist(lapply(df, FUN = function(x) {!all(is.na(x))}))
  df <- df[ , all_na, drop = FALSE]

  if (missing(columnwise)) {
    columnwise <- !any(is_period_text(get_strings(df[1,]), frequency))
  }

  if (!columnwise) {
    df <- as.data.frame(t(df), stringsAsFactors = FALSE)
  }

  period_info <- find_period_column_simple(df, frequency)
  time_column <- period_info$col_nr
  is_period <- period_info$is_period
  first_data_row <- Position(function(x) {x}, is_period)

  # compute the row with variable names. 0 means: column names
  # and the label rows
  if (labels != "before") {
    name_row <- 1
    if (first_data_row > 2) {
      label_rows <- 2:(first_data_row -1)
    } else {
      label_rows <- integer(0)
    }
  } else {  # labels == before
    name_row <- first_data_row - 1
    if (first_data_row > 2) {
      label_rows <- 1:(first_data_row - 2)
    } else {
      labels_rows <- integer(0)
    }
  }

  # remove column withouts names to the right of the time_columns
  keep_cols <- get_strings(df[name_row, ]) != ""
  keep_cols[time_column] <- TRUE
  if (time_column > 1) {
    keep_cols[1:(time_column - 1)] <- FALSE
  }
  df <- df[, keep_cols, drop = FALSE]

  if (labels != "no" && length(label_rows) > 0) {
    lbl_data <- df[label_rows, , drop = FALSE]
    lbl_data <- lbl_data[ , -1, drop = FALSE]
    if (length(label_rows) == 1) {
      labels <- get_strings(lbl_data)
    } else {
      labels <- as.data.frame(t(lbl_data))
      l <- lapply(labels, get_strings)
      labels <- do.call(paste, l)
      labels <- trimws(labels)
    }
  }

  colnames(df) <- df[name_row, , drop = FALSE]

  # remove rows without period
  df <- df[is_period, , drop = FALSE]

  ret <- as.regts(df, time_column = 1, frequency = frequency)
  if (labels != "no" && any(labels != "")){
    ts_labels(ret) <- labels
  }

  return(ret)

}

find_period_column_simple <- function(df, frequency) {

  for (i in 1:ncol(df)) {
    is_period <- is_period_text(get_strings(df[, i]), frequency)
    if (any(is_period)) {
      col_index <- i
      row_nr <- Position(function(x) {x}, is_period)
      return(list(col_nr = i, is_period = is_period))
    }
  }

  stop("No periods found!")
}

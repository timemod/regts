# Internal function to read timeseries columnwise from a dataframe.
# This function is used in function read_ts_csv and read_ts_xlsx
read_ts_columnwise <- function(df, frequency = NA,
                               labels = c("no", "after", "before"),
                               dec =  ".") {

  labels <- match.arg(labels)

  # remove all columns with only NAs
  all_na <- sapply(df, FUN = function(x) {!all(is.na(x))})
  df <- df[ , all_na, drop = FALSE]

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
      label_rows <- integer(0)
    }
  }

  if (length(label_rows) == 0) {
    labels <- "no"
  }

  # remove column withouts names to the right of the time_columns
  keep_cols <- get_strings(df[name_row, ]) != ""
  keep_cols[time_column] <- TRUE
  if (time_column > 1) {
    keep_cols[1:(time_column - 1)] <- FALSE
  }
  df <- df[, keep_cols, drop = FALSE]

  if (labels != "no") {
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

  # put time column in the row names, then the conversion of
  # data frame to numeric is more efficient
  rownames(df) <- df[, 1]
  df <- df[, -1, drop = FALSE]
  df <- numeric_data_frame(df, dec = dec)

  # set numeric = FALSE, because we already know that df is numeric
  ret <- as.regts(df, frequency = frequency, numeric = TRUE)
  if (labels != "no" && any(labels != "")) {
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

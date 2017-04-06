# internal function to read timeseries rowwise from a data frame with
# the time index in the column header. This function assumes that
# all columns in df are numeric.
read_ts_rowwise <- function(df, frequency,
                            labels = c("no", "after", "before")) {

  labels <- match.arg(labels)

  is_period <- is_period_text(get_strings(colnames(df)), frequency)
  first_prd_col <- Position(function(x) {x}, is_period)

  if (labels == "before") {
    name_col <- first_prd_col - 1
    label_cols <- seq_len(name_col - 1)
  } else {
    name_col <- 1
    if (first_prd_col >= 3) {
      label_cols <- 2 : (first_prd_col - 1)
    } else {
      label_cols <- numeric(0)
    }
  }

  # remove columns to be ignored
  col_sel <- is_period
  col_sel[1:(first_prd_col - 1)] <- TRUE
  if (labels == "no") {
    # remove label colums
    col_sel[label_cols] <- FALSE
    label_cols <- numeric(0)
  }
  df <- df[, col_sel, drop = FALSE]


  # transpose
  df <- transpose_df(df, colname_column = name_col, label_column = label_cols)

  # here use numeric = FALSE, because here we already know that
  # the timeseries is numeric
  return(as.regts(df, frequency = frequency, numeric = FALSE))
}

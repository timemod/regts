# internal function to read timeseries rowwise from a data frame with
# the time index in the column header.
# is numeric = TRUE, then the timeseries are converted to numeric
read_ts_rowwise <- function(df, frequency, labels = c("no", "after", "before"),
                            name_fun, dec = ".") {

  labels <- match.arg(labels)

  # Sometimes, in Exinteger numbers are stored internally as
  # for example "2010.0". The corresponding column name then becomes "2010.0".
  # This is the case for the Excel files written by Isis with
  # the "nice" method. Therefore we have to remove the redundant .0 in this situation.
  if (is.na(frequency) || frequency == 1) {
    cnames <- colnames(df)
    sel <- grep("^\\d{1,4}\\.0+$", cnames)
    cnames[sel] <- gsub("\\.0+$", "", cnames[sel])
    colnames(df) <- cnames
  }

  is_period <- is_period_text(get_strings(colnames(df)), frequency)
  first_prd_col <- Position(function(x) {x}, is_period)
  if (is.na(first_prd_col)) {
    stop("No periods found when reading rowwise timeseries")
  }
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
  df <- df[ , col_sel, drop = FALSE]


  data_cols <- (max(c(name_col, label_cols)) + 1) : ncol(df)

  # convert all data columns to numerical columns, taking the decimal separator
  # into account
  mat <- numeric_matrix(df[, data_cols, drop = FALSE], dec = dec)
  names <- df[, name_col]
  mat <- t(mat)
  colnames(mat) <- names

  # remove columns with empty names
  mat <- mat[ , which(!(get_strings(colnames(mat)) == "")), drop = FALSE]

  # convert the matrix to a regts, using numeric = FALSE, because we already
  # know that df is numeric
  ret <- as.regts(mat, frequency = frequency, numeric = FALSE)

  if (labels != "no" && length(label_cols) > 0) {
    lbls <- df[, label_cols, drop = FALSE]
    l <- lapply(lbls[names%in% colnames(ret), , drop = FALSE], get_strings)
    l$names <- NULL
    lbls <- do.call(paste, l)
    lbls <- trimws(lbls)
    if (any(labels != "")) {
      ts_labels(ret) <- lbls
    }
  }

  # apply function to columnnames if given
  if (!missing(name_fun)) {
    if (!is.function(name_fun)) {
      stop("argument name_fun is not a function")
    }
    colnames(ret) <- name_fun(colnames(ret))
  }

  return(ret)
}

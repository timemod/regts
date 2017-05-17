#' Write timeseries to a csv file
#'
#' This function writes timeseries to a csv file.
#' The csv file is actually written by function \code{\link[data.table]{fwrite}}
#' of package \code{data.table}.
#' @param x a \code{ts} or \code{regts} object
#' @param file a \code{regts} object
#' @param sep The separator between columns. Default is ",".
#' @param dec The decimal separator, by default ".". Cannot be the same as sep.
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present.
#' @importFrom data.table fwrite
#' @export
write_ts_csv <- function(x, file, rowwise = TRUE, sep = ",", dec = ".",
                        labels = c("after", "before", "no")) {

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  df <- write_ts_df(x, rowwise, labels, labels_missing)

  fwrite(df, file, sep = sep, dec  = dec)

  return(invisible(NULL))
}

#' Write timeseries to an xlsx file
#'
#' This function writes timeseries to an xlsx  file.
#' The xlsx file is actually written by function \code{\link[xlsx]{write.xlsx}}
#' of package \code{data.table}.
#' @param x a \code{ts} or \code{regts} object
#' @param file a \code{regts} object
#' @param sheet_name the sheet name
#' @param labels should labels we written, and if so before the names or after
#' the names? By default, labels are written after the names if present.
#' @importFrom xlsx write.xlsx2
#' @export
write_ts_xlsx <- function(x, file, sheet_name = "Sheet1", rowwise = TRUE,
                         labels = c("after", "before", "no")) {

  labels_missing <- missing(labels)

  if (!labels_missing) {
    labels <- match.arg(labels)
  }

  df <- write_ts_df(x, rowwise, labels, labels_missing)

  dft <<- df
  print(df)

  write.xlsx2(df, file, sheetName = sheet_name, row.names = FALSE)

  # TODO: nice formatting
  # b <- createWorkbook()
  # sheet  <- createSheet(wb, sheetName="data")
  #
  # # number format
  # nf <- CellStyle(wb, dataFormat=DataFormat("0.000"))
  # dfColIndex <- rep(list(nf), dim(zoo1)[2])
  # names(dfColIndex) <- seq(1, dim(zoo1)[2], by = 1)
  #
  # # cell style for the column headers
  # colNamesStyle <- CellStyle(wb, alignment = Alignment(h = "ALIGN_RIGHT"))
  #
  # # wegschrijven data
  # addDataFrame(zoo1, sheet, colStyle = dfColIndex, colnamesStyle = colNamesStyle)
  #
  # autoSizeColumn(sheet, seq(1, dim(zoo1)[2] + 1))
  # createFreezePane(sheet, 2, 2)

  return(invisible(NULL))
}


# write timeseries to a data frame that can be written to a csv or excel file
write_ts_df <- function(x, rowwise, labels, labels_missing) {

  if (!is.ts(x)) {
    stop(paste("Argument x is not a timeseries object but a ", class(x)))
  }

  if (labels_missing || labels != "no") {
    lbls <- ts_labels(x)
    if (labels_missing) {
      if (is.null(lbls)) {
        labels <- "no"
      } else {
        labels <- "after"
      }
    }
  }

  # remove the labels, we don't need them any more
  ts_labels(x) <- NULL

  df <- as.data.frame(x)

  if (rowwise) {
    df <- transpose_df(df)
    names <- rownames(df)
    if (labels == "no") {
      df <- cbind(names = names, df, stringsAsFactors = FALSE)
    } else if (labels == "after") {
      df <- cbind(names = names, labels = lbls, df, stringsAsFactors = FALSE)
    } else if (labels == "before") {
      df <- cbind(labels = lbls, names = names, df,stringsAsFactors = FALSE)
    }
  } else {
    df <- cbind(period = rownames(df), df)
    if (labels == "after") {
      df <- rbind(c(NA_character_, lbls), df, stringsAsFactors = FALSE)
    } else if (labels == "before") {
      stop("For columnwise timeseries labels option \"before\" is not allowed")
    }
  }

  rownames(df) <- NULL
  return(df)
}



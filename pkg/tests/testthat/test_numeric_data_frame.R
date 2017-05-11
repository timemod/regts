context("numeric_data_frame")

test_that("normal data frames", {

  df <- data.frame(a = c("1.123", "", NA), b = c("1", "  ", "45"),
                   c = 10:12, stringsAsFactors = FALSE)

  df_fac  <- data.frame(a = c("1.123", "", NA), b = c("1", "  ", "45"),
                        c = 10:12, stringsAsFactors = TRUE)



  expect_warning(df_num <- numeric_data_frame(df), NA)
  expect_warning(df_fac_num <- numeric_data_frame(df_fac), NA)

  df_ref <- data.frame(a = c(1.123, NA, NA), b = c(1, NA, 45),
                       c = 10:12)
  # the next statements converts row names to character (weird?),
  # the same is used in numeric_data_frame.
  rownames(df_ref) <- rownames(df_ref)

  expect_identical(df_num, df_ref)
  expect_identical(df_fac_num, df_ref)

})

test_that("weird data frames", {

  posixcts <- rep(as.POSIXct("1969-12-31 23:59:59"), 3)
  dates <- as.Date(posixcts)
  df      <- data.frame(a = c("1.123", "x", NA),
                        b = posixcts,
                        c = dates, stringsAsFactors = FALSE)

  msg <-paste0("NAs introduced by coercion\n",
               "The following texts could not be converted to numeric:\n",
               "\"x\"\n\"1969-12-31 23:59:59\"\n\"1969-12-31 23:59:59\"",
               "\n\"1969-12-31 23:59:59\"\n\"1969-12-31\"\n",
               "\"1969-12-31\"\n\"1969-12-31\"")

  msg <- "NAs introduced by coercion\nThe following texts could not be converted to numeric:\n\"x\"\n\"1969-12-31 23:59:59\"\n\"1969-12-31 23:59:59\"\n\"1969-12-31 23:59:59\"\n\"1969-12-31\"\n\"1969-12-31\"\n\"1969-12-31\""
  expect_warning(df_num <- numeric_data_frame(df), msg)

  df_ref <- data.frame(a = c(1.123, NA, NA), b = rep(NA_real_, 3),
                       c = rep(NA_real_, 3))
  # the next statements converts row names to character (weird?),
  # the same is used in numeric_data_frame.
  rownames(df_ref) <- rownames(df_ref)

  expect_identical(df_num, df_ref)
})



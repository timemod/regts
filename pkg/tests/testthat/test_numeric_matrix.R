library(testthat)
library(regts)

rm(list = ls())

context("internal function numeric_data_frame")

test_that("normal data frames", {

  df <- data.frame(a = c("1.123", "", NA), b = c("1", "  ", "45"),
                   c = 10:12, stringsAsFactors = FALSE)

  df_fac  <- data.frame(a = c("1.123", "", NA), b = c("1", "  ", "45"),
                        c = 10:12, stringsAsFactors = TRUE)


  expect_warning(mat_num <- regts:::numeric_matrix(df), NA)
  expect_warning(mat_fac_num <- regts:::numeric_matrix(df_fac), NA)

  mat_ref <- as.matrix(data.frame(a = c(1.123, NA, NA), b = c(1, NA, 45),
                       c = 10:12))

  expect_identical(mat_num, mat_ref)
  expect_identical(mat_fac_num, mat_ref)
})

test_that("weird data frames", {

  posixcts <- rep(as.POSIXct("1969-12-31 23:59:59"), 3)
  dates <- as.Date(posixcts)
  df      <- data.frame(a = c("1.123", "x", NA),
                        b = posixcts,
                        c = dates, stringsAsFactors = FALSE)

  msg <-paste0("NAs introduced by coercion.\n",
               "The following texts could not be converted to numeric:\n",
               "\"x\"\n\"1969-12-31 23:59:59\"\n\"1969-12-31\"")

  expect_warning(mat_num <- regts:::numeric_matrix(df), msg)

  mat_ref <- as.matrix(data.frame(a = c(1.123, NA, NA), b = rep(NA_real_, 3),
                       c = rep(NA_real_, 3)))

  expect_identical(mat_num, mat_ref)
})

test_that("multiple problems texts", {

  df <- data.frame(a = paste("aap" , 1:20))

  msg <-paste0("NAs introduced by coercion.\n",
               "20 texts could not be converted to numeric.\n",
               "The first 10 texts that gave problems are:\n",
               paste(paste0("\"aap ", 1:10, "\""), collapse = "\n"))


  expect_warning(dum <- regts:::numeric_matrix(df), msg)
})



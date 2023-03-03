library(regts)
library(testthat)

rm(list = ls())


data <- matrix(1: 9, ncol = 3)
names <- c("a", "b", "c")
regts1 <- regts(data, start = "2018Q2", names = names,
                labels = paste("Timeseries", names))

empty_ts <- regts(matrix(NA_integer_, nrow = nrow(regts1), ncol = 0),
                start = "2018Q2", labels = character(0))

test_that("replacement NULL for regts with character selector", {

  t1 <- regts1
  t1[, "b"] <- NULL
  t1_ref <- regts1
  t1_ref$b <- NULL

  expect_identical(t1, t1_ref)
  expect_identical(as.character(ts_labels(t1)),
                   paste("Timeseries", c("a", "c")))

  t2 <- regts1
  t2[, c("a", "c")] <- NULL
  expect_identical(t2, regts1[, "b", drop = FALSE])

  t3 <- regts1
  t3[, c("a", "b", "c", "a", "xxx")] <- NULL
  expect_identical(t3, empty_ts)

  t4 <- regts1
  t4[,  "xxxx"] <- NULL
  expect_identical(t4, regts1)

  t5 <- regts1
  t5[,  character(0)] <- NULL
  expect_identical(t5, regts1)

})

test_that("replacement NULL for regts with integer selector", {

  t1 <- regts1
  t1[, 2] <- NULL
  t1_ref <- regts1
  t1_ref$b <- NULL

  expect_identical(t1, t1_ref)
  expect_identical(as.character(ts_labels(t1)),
                   paste("Timeseries", c("a", "c")))

  t2 <- regts1
  t2[, c(1, 3)] <- NULL
  expect_identical(t2, regts1[, "b", drop = FALSE])

  t3 <- regts1
  t3[, c(1, 3, 2, 1, 3, 2)] <- NULL
  expect_identical(t3, empty_ts)

  t4 <- regts1
  t4[, numeric(0)] <- NULL
  expect_identical(t4, regts1)
})

test_that("replacement NULL for regts with logical selector", {

  t1 <- regts1
  t1[, c(FALSE, TRUE, FALSE)] <- NULL
  t1_ref <- regts1
  t1_ref$b <- NULL

  expect_identical(t1, t1_ref)
  expect_identical(as.character(ts_labels(t1)),
                   paste("Timeseries", c("a", "c")))

  t2 <- regts1
  t2[, c(TRUE, FALSE, TRUE)] <- NULL
  expect_identical(t2, regts1[, "b", drop = FALSE])

  t3 <- regts1
  t3[, c(TRUE, TRUE, TRUE)] <- NULL
  expect_identical(t3, empty_ts)

  t4 <- regts1
  t4[, TRUE] <- NULL
  expect_identical(t4, empty_ts)

  t5 <- regts1
  t5[, logical(0)] <- NULL
  expect_identical(t5, regts1)
})


test_that("weird timeseries", {

  rts_no_names <- regts1
  colnames(rts_no_names) <- NULL
  t1 <- rts_no_names
  t1[, "c"] <- NULL
  expect_identical(t1, rts_no_names)

  rts_dupl_names <- regts1
  colnames(rts_dupl_names)[3] <- "a"
  t2 <- rts_dupl_names
  # only the first match should be removed. data frames behave similarly
  t2[, "a"] <- NULL
  expect_identical(t2, rts_dupl_names[, -1])
})

test_that("errors", {
  t1 <- regts1
  msg <- "Row selection not allowed when the replacement is NULL"
  expect_error(t1[1, c("a", "b")] <- NULL, msg)

  t2 <- regts1$b
  msg <- "replacement has length zero"
  expect_error(t2[2] <- NULL, msg)

  t3 <- regts1
  expect_silent(t3[, NA_character_] <- NULL)
})


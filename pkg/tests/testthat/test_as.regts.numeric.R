library(regts)
library(testthat)

rm(list = ls())



test_that("no names or names with invalid periods", {

  ref_ts <- regts(1:3, start = "1")

  vec1 <- 1:3
  result1 <- as.regts(vec1)
  expect_equal(result1, ref_ts)

  vec2 <- vec1
  names(vec2) <- c("a", "2018q2", "2018q3")

  result2 <- as.regts(vec2)
  expect_equal(unname(result2), ref_ts)
  expect_equal(names(result2), c("a", "2018q2", "2018q3"))


  expect_error(as.regts(vec2, fun = period),
               "Illegal period a")
  expect_error(as.regts(vec2, strict = FALSE),
               "Illegal period a")
})


test_that("named vector with valid periods", {

  ref_ts <- regts(1:3, start = "2018q1")

  vec1 <- as.integer(1:3)
  names(vec1) <- c("2018q1", "2018q2", "2018q3")

  result1 <- as.regts(vec1)
  expect_equal(result1, ref_ts)

  vec2 <- vec1[c(1, 3)]
  names(vec2) <- c("2018q1", "2018q3")

  expect_error(as.regts(vec2),
               paste("Missing periods found \\(2018Q2\\).",
                    "Set parameter strict to FALSE!"))

  expected_result2 <- ref_ts
  expected_result2[2] <- NA
  result2 <- as.regts(vec2, strict = FALSE)
  expect_equal(result2, expected_result2)

  vec3 <- vec1
  names(vec3) <- c("2018q1", "2018q2", "2018m3")
  expect_error(as.regts(vec3),
               "The names contain periods with different frequencies.")

})

test_that("argument fun", {

  ref_ts <- regts(1:3, start = "2018q1")

  vec1 <- as.numeric(1:3)
  names(vec1) <- c("q1", "q2", "q3")

  result1 <- as.regts(vec1, fun = function(x)
                               {period(paste0("2018", x))})
  expect_equal(result1, ref_ts)
})


library(testthat)
library(regts)


test_that("equal periods", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  r1 <- update_ts(x1, x2, "replace")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")
  r2 <- update_ts(x2, x1, "replace")

  expect_identical(u1, v1)
  expect_identical(u1, r1)

  expect_identical(u2, v2)
  expect_identical(u2, r2)

  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(u1, n2[, sort(colnames(n2))])

})

test_that("equal periods, NA values in x1", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x1["2000", "a"] <- NA
  x1["2000/2001", "c"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  expect_identical(u1, n2[, sort(colnames(n2))])
  # compare u2 ( = x1 + d)
  expect_identical(u2[, colnames(x1)], x1)
  # extend x1 and compare again
  x1_d <- cbind(x1,  x2[, "d"])
  colnames(x1_d) <- sort(colnames(u2))
  expect_identical(x1_d, u2[, sort(colnames(u2))])
  expect_identical(n1, v2[, sort(colnames(v2))])
  expect_identical(v1, u1)

})


test_that("equal periods, NA values in x2", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x2["2003", "d"] <- NA
  x2["2000/2001", "c"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  # compare u1 ( = x2 + b)
  expect_identical(u1[, colnames(x2)], x2)
  # extend x2 and compare again
  x2_b <- cbind(x2,  x1[, "b"])
  colnames(x2_b)[4] <- "b"
  x2_b <- x2_b[, sort(colnames(x2_b)), drop = FALSE]
  expect_identical(u1, x2_b)

  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(n1, v2[, sort(colnames(v2))])
  expect_identical(v1, n2[, sort(colnames(n2))])

})


test_that("equal periods, NA values in x1 and x2", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x1["2001", c("b","d")] <- NA
  x2["2000/2001", "c"] <- NA
  x2["2002", "d"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  # compare u1 ( = x2 + b)
  expect_identical(u1[, colnames(x2)], x2)
  # extend x2 and compare again
  x2_b <- cbind(x2,  x1[, "b"])
  colnames(x2_b)[4] <- "b"
  x2_b <- x2_b[, sort(colnames(x2_b)), drop = FALSE]
  expect_identical(u1, x2_b)
  expect_identical(x1, u2[, sort(colnames(u2))])
  expect_identical(n1, v2[, sort(colnames(v2))])
  expect_identical(v1, n2[, sort(colnames(n2))])

})

test_that("overlapping periods, no NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")
  r1 <- update_ts(x1, x2, "replace")
  r2 <- update_ts(x2, x1, "replace")

  expect_identical(u1, n2[, sort(colnames(n2))])
  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(v1, n2[, sort(colnames(n2))])
  expect_identical(n1, v2[, sort(colnames(v2))])
  n2["2000/2001",c("a","c")] <- NA
  expect_identical(r1, n2[, sort(colnames(n2))])
  n1["2004/2006",c("a","c")] <- NA
  expect_identical(n1, r2[, sort(colnames(r2))])

})

test_that("overlapping periods, NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  x1["2000", c("a","b","c")] <- NA
  x2["2003/2006", "c"] <- NA
  x2["2004", "d"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  # only element ["2003", "c"] is different between u1 and n2
  expect_identical(u1[, c("a","b","d")], n2[, c("a","b","d")])
  u1["2003", "c"] <- n2["2003", "c"]
  expect_identical(u1, n2[, sort(colnames(n2))])
  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(v1, n2[, sort(colnames(n2))])
  expect_identical(n1["2001/2006", ], v2[, sort(colnames(v2))])

})

test_that("period x1 encloses period x2, no NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2006",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2005",
              names = c("a", "c", "d"))

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  expect_identical(u1, n2[, sort(colnames(n2))])
  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(v1, n2[, sort(colnames(n2))])
  expect_identical(n1, v2[, sort(colnames(v2))])

})


test_that("period x1 encloses period x2, NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2006",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2005",
              names = c("a", "c", "d"))
  x1["2000", c("a","b","c")] <- NA

  x2["2004", "d"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  prd_v2 <- get_period_range(v2)

  expect_identical(u1, n2[, sort(colnames(n2))])
  expect_identical(n1, u2[, sort(colnames(u2))])
  expect_identical(v1, n2[, sort(colnames(n2))])
  expect_identical(n1[prd_v2, ], v2[, sort(colnames(v2))])

})

test_that("no overlapping periods, NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2006/2008",
              names = c("a", "c", "d"))

  x1["2000", c("a","b","c")] <- NA
  x1[, "b"] <- NA
  x2["2006/2008", "c"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  expect_identical(u1, u2[, sort(colnames(n2))])
  expect_identical(n1, n2[, sort(colnames(n2))])
  # updval removes leading/trailing rows and columns with only NA values
  prd_v2 <- get_period_range(v2)
  expect_identical(v1[prd_v2, colnames(v2)], v2[, sort(colnames(v2))] )
  v1 <- na_trim(v1)
  v1 <- remove_na_columns(v1)
  expect_identical(v1, v2[, sort(colnames(v2))])

})

test_that("no overlapping columns, NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 2), period = "2002/2005",
              names = c("d", "e"))

  x1["2000", c("a","b","c")] <- NA
  x1["2002", "b"] <- NA
  x2["2004/2005", "d"] <- NA

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  expect_identical(u1, u2[, sort(colnames(u2))])
  expect_identical(n1, n2[, sort(colnames(n2))])
  prd_v2 <- get_period_range(v2)
  expect_identical(v1[prd_v2, ], v2[, sort(colnames(v2))])

})

test_that("no column names", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003")

  x2 <- regts(matrix(data = rep(2), nc = 2), period = "2002/2005")

  u1 <- update_ts(x1, x2, "upd")
  n1 <- update_ts(x1, x2, "updna")
  v1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x2, x1, "upd")
  n2 <- update_ts(x2, x1, "updna")
  v2 <- update_ts(x2, x1, "updval")

  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(n1, v2)
  expect_identical(n2, v1)

})

test_that("join_second parameter", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  u1 <- update_ts(x1, x2, "upd", join_second = TRUE)
  u2 <- update_ts(x1, x2, "upd", join_second = FALSE)

  expect_identical(u2, u1[, colnames(u2)])
  u2$d <- u1$d
  expect_identical(u2, u1)


})


test_that("join_second parameter, NA values", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  x1["2000", c("a","b","c")] <- NA
  x1[, "b"] <- NA

  u1 <- update_ts(x1, x2, "updna", join_second = TRUE)
  u2 <- update_ts(x1, x2, "updna", join_second = FALSE)

  expect_identical(u2, u1[, colnames(u2)])
  u2$d <- u1$d
  expect_identical(u2, u1)

})

test_that("update an univariate timeseries: matrix and vector", {

  x1 <- regts(matrix(data = 2, nc = 1), period = "2002/2005",
              names = c("a"))
  a <- regts(rep(2), period = "2002/2005")

  x2 <- regts(matrix(data = 1, nc = 1), period = "2000/2003",
              names = c("b"))
  b <- regts(rep(1), period = "2000/2003")

  x3 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))

  u1 <- update_ts(x1, x2, "upd")
  u2 <- update_ts(x1, b,  "upd")
  v1 <- update_ts(a , x2, "upd")
  v2 <- update_ts(a , b,  "upd")
  n1 <- update_ts(x1, x3, "upd")
  n2 <- update_ts(a , x3, "upd")

  expect_identical(u1, u2)
  expect_identical(v1, v2)
  expect_identical(n1, n2)
})

test_that("update with univariate timeseries: matrix and vector", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))

  x2 <- regts(matrix(data = 2, nc = 1), period = "2002/2005",
              names = c("a"))
  a <- regts(rep(2), period = "2002/2005")

  u1 <- update_ts(x1, x2, "upd")
  u2 <- update_ts(x1, a,  "upd")
  v1 <- update_ts(x1, x2, "updval")
  v2 <- update_ts(x1, a,  "updval")
  n1 <- update_ts(x1, x2, "updna")
  n2 <- update_ts(x1, a,  "updna")

  expect_identical(u1, u2)
  expect_identical(v1, v2)
  expect_identical(n1, n2)
})

test_that("update with several NA timeseries and method updval", {

  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))

  x2 <- regts(matrix(data = NA, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))

  x3 <- regts(matrix(data = NA, nc = 1), period = "2002/2005",
              names = c("a"))
  a <- regts(NA, period = "2002/2005")

  u1 <- update_ts(x1, x2, "updval")
  u2 <- update_ts(x1, x3, "updval")
  u3 <- update_ts(x1, a,  "updval")

  expect_identical(u1, x1)
  expect_identical(u1, u2)
  expect_identical(u1, u3)
})

test_that("errors", {
  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = 2, nc = 3), period = "2002q1/2006q4",
              names = c("a", "c", "d"))
  nr <- 2
  expect_error(update_ts(x1, nr, "replace"),
               "Argument x2 \\(nr\\) is not a timeseries")

  expect_error(update_ts(x1, x2, "upd"),
               "Timeseries x1 and x2 \\(x1 and x2\\) have different frequencies")
})

test_that("labels", {

  names1 <- c("a", "b", "c")
  labels1 <- paste("Timeseries", names1)
  x1 <- regts(matrix(data = 1, nc = 3), period = "2000/2003",
              names = names1, labels = labels1)

  names2 <- c("b", "c", "d")
  labels2 <- paste("Variable", names2)
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2003",
              names = names2, labels = labels2)
  x2["2001", "c"] <- NA
  x2[, "d"] <- NA

  # first use standard updating
  result <- update_ts(x1, x2)

  expected_result <- x1
  expected_result[, "d"] <- NA
  expected_result[, c("b", "c")] <- 2
  expected_result["2001", "c"] <- NA
  expected_result <- update_ts_labels(expected_result, c(d = "Variable d"))
  expect_identical(result, expected_result)

  # now with method = updval
  result2 <- update_ts(x1, x2, method  = "updval")

  expected_result2 <- x1
  expected_result2[, c("b", "c")] <- 2
  expected_result2["2001", "c"] <- 1
  expect_identical(result2, expected_result2)
})

test_that("single period, no missing names", {

  # this test failed in regts version <= 1.0.0

  x1 <- regts(matrix(data = 1, nc = 2), period = "2000",
              names = c("a", "b"))
  x2 <- regts(matrix(data = 2, nc = 1), period = "2000",
              names = c("b"))

  result <- update_ts(x1, x2)

  expect_identical(result$a, x1$a)
  expect_identical(result$b, x2$b)
})


test_that("timeseries with zero columns", {

  x1 <- regts(matrix(0, nr = 3, nc = 0), period = "2000/2003")
  x2 <- regts(matrix(data = 2, nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  expect_equal(update_ts(x1, x2), x2)
  expect_equal(update_ts(x1, x1), x1)
  expect_equal(update_ts(x2, x1), x2)

  # update ts with zero columns with univariate timeseries
  expected_result <- x2[, "a", drop = FALSE]
  colnames(expected_result) <- "x2$a"
  expect_equal(update_ts(x1, x2$a), expected_result)

  # update  univariate timeseries with ts with zero columns
  expect_equal(update_ts(x2$a, x1), expected_result)

  x2_NA <- x2
  x2_NA["2003"] <- NA
  x2_NA$a <- NA
  expect_equal(update_ts(x1, x2_NA), x2_NA)
  expect_equal(update_ts(x1, x2_NA, method = "updval"),
               x2["2000/2002", c("c", "d")])
})

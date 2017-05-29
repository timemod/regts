context("update regts with methods: replace, tsupd, tsupdna, tsupdval")

test_that("equal periods", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(u1, v1)
  expect_identical(u2, v2)

})

test_that("equal periods, NA values in x1", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x1["2000", "a"] <- NA
  x1["2000/2001", "c"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  # compare u2 ( = x1 + d)
  expect_identical(u2[, colnames(x1)], x1)
  # extend x1 and compare again
  x1_d <- cbind(x1,  x2[, "d"])
  colnames(x1_d) <- colnames(u2)
  expect_identical(u2, x1_d)
  expect_identical(n1, v2)
  expect_identical(v1, u1)

})


test_that("equal periods, NA values in x2", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x2["2003", "d"] <- NA
  x2["2000/2001", "c"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  # compare u1 ( = x2 + b)
  expect_identical(u1[, colnames(x2)], x2)
  # extend x2 and compare again
  x2_b <- cbind(x2,  x1[, "b"])
  colnames(x2_b)[4] <- "b"
  x2_b <- x2_b[, sort(colnames(x2_b)), drop = FALSE]
  expect_identical(u1, x2_b)

  expect_identical(u2, n1)
  expect_identical(n1, v2)
  expect_identical(v1, n2)

})


test_that("equal periods, NA values in x1 and x2", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2003",
              names = c("a", "c", "d"))

  x1["2001", c("b","d")] <- NA
  x2["2000/2001", "c"] <- NA
  x2["2002", "d"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  # compare u1 ( = x2 + b)
  expect_identical(u1[, colnames(x2)], x2)
  # extend x2 and compare again
  x2_b <- cbind(x2,  x1[, "b"])
  colnames(x2_b)[4] <- "b"
  x2_b <- x2_b[, sort(colnames(x2_b)), drop = FALSE]
  expect_identical(u1, x2_b)
  expect_identical(u2, x1)
  expect_identical(n1, v2)
  expect_identical(n2, v1)

})

test_that("overlapping periods, no NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(v1, n2)
  expect_identical(v2, n1)

})

test_that("overlapping periods, NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2002/2006",
              names = c("a", "c", "d"))

  x1["2000", c("a","b","c")] <- NA
  x2["2003/2006", "c"] <- NA
  x2["2004", "d"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  # only element ["2003", "c"] is different between u1 and n2
  expect_identical(u1[, c("a","b","d")], n2[, c("a","b","d")])
  u1["2003", "c"] <- n2["2003", "c"]
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(v1, n2)
  expect_identical(v2, n1["2001/2006", ])

})

test_that("period x1 encloses period x2, no NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2006",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2002/2005",
              names = c("a", "c", "d"))

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(v1, n2)
  expect_identical(v2, n1)

})


test_that("period x1 encloses period x2, NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2006",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2002/2005",
              names = c("a", "c", "d"))
  x1["2000", c("a","b","c")] <- NA

  x2["2004", "d"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  prd_v2 <- get_period_range(v2)

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(v1, n2)
  expect_identical(v2, n1[prd_v2, ])

})

test_that("no overlapping periods, NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2006/2008",
              names = c("a", "c", "d"))

  x1["2000", c("a","b","c")] <- NA
  x1[, "b"] <- NA
  x2["2006/2008", "c"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, u2)
  expect_identical(n1, n2)
  # tsupdval removes leading/trailing rows and columns with only NA values
  prd_v2 <- get_period_range(v2)
  expect_identical(v1[prd_v2, colnames(v2)], v2 )
  v1 <- na_trim(v1)
  v1 <- remove_na_columns(v1)
  expect_identical(v1, v2)

})

test_that("no overlapping columns, NA values", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 2), period = "2002/2005",
              names = c("d", "e"))

  x1["2000", c("a","b","c")] <- NA
  x1["2002", "b"] <- NA
  x2["2004/2005", "d"] <- NA

  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, u2)
  expect_identical(n1, n2)
  prd_v2 <- get_period_range(v2)
  expect_identical(v2, v1[prd_v2, ])

})

test_that("no column names", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003")

  x2 <- regts(matrix(data = rep(2), nc = 2), period = "2002/2005")


  r1 <- tsupdate(x1, x2, "replace")
  u1 <- tsupdate(x1, x2, "tsupd")
  n1 <- tsupdate(x1, x2, "tsupdna")
  v1 <- tsupdate(x1, x2, "tsupdval")
  r2 <- tsupdate(x2, x1, "replace")
  u2 <- tsupdate(x2, x1, "tsupd")
  n2 <- tsupdate(x2, x1, "tsupdna")
  v2 <- tsupdate(x2, x1, "tsupdval")

  expect_identical(r1, x2)
  expect_identical(r2, x1)
  expect_identical(u1, n2)
  expect_identical(u2, n1)
  expect_identical(n1, v2)
  expect_identical(n2, v1)

})

test_that("errors", {
  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2003",
              names = c("a", "b", "c"))
  x2 <- regts(matrix(data = rep(2), nc = 3), period = "2002q1/2006q4",
              names = c("a", "c", "d"))
  nr <- 2
  expect_error(tsupdate(x1, nr, "replace"),
               "Argument x2 \\(nr\\) is not a multivariate timeseries")

  expect_error(tsupdate(x1, x2, "tsupd"),
               "Timeseries x1 and x2 \\(x1 and x2\\) have different frequencies")
})



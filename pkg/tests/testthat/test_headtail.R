
test_that("head, tail & topleft for regts", {

	data <- regts(matrix(1:200, ncol = 20), start = "2010Q1",
                  names = paste0("abc", 1:20))
	data2 <- as.regts(as.data.frame(data))
	period1 <- period_range("2010Q1/2011Q2")
	period2 <- period_range("2011Q1/2012Q2")

	expect_identical(head(data), head(data2))
	expect_identical(head(data), tail(data[period1, ]))
	expect_identical(head(data[period2, ]), tail(data))

  expect_identical(topleft(data), head(data[, 1:10 ]))
  expect_identical(topleft(data), tail(data[period1, 1:10 ]))
  expect_identical(topleft(data, ncol = 6), head(data[, 1:6 ]))
  expect_identical(topleft(data, ncol = 5), topleft(data2, ncol = 5))
})

test_that("topleft for small regts", {

  data <- regts(matrix(1:12, ncol = 3), start = "2010",
                names = paste0("a", 1:3))
  expect_identical(topleft(data), head(data))
})

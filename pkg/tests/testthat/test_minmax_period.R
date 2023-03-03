
test_that("min & max for period arguments", {

	y1 <- period("2010")
	y2 <- period("2012")
	q1 <- period("2010Q1")
	q2 <- period("2010Q2")
	q3 <- period("1999Q3")

	expect_identical(min(y1, y2), y1)
	expect_identical(max(y1, y2), max(y2))
	expect_identical(min(y1, y2), min(y2, y1))

	expect_identical(min(q1, q3), min(q2, q3))
	expect_identical(max(q1, q2), max(q2, q3))
	expect_identical(max(q1, q2, q3), q2)
	expect_identical(min(q1, q2, q3), q3)

	expect_error(min(y1, q1), "All periods must have the same frequency")
	expect_error(max(y1, "2010"), "Inputs must all be periods")
	expect_error(sum(y1, "2010"), "sum is not supported for period objects")
})

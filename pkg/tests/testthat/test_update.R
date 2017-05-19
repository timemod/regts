context("update regts")

test_that("replace, tsupd", {

  x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2002",
            names = c("a", "b", "c"))
	x2 <- regts(matrix(data = rep(2), nc = 3), period = "2000/2001",
            names = c("a", "c", "d"))

	r1 <- tsupdate(x1, x2, "replace")
	r2 <- tsupdate(x2, x1, "replace")

	u1 <- tsupdate(x1, x2, "tsupd")
	u2 <- tsupdate(x2, x1, "tsupd")

# NA's toevoegen
	xNa1 <- x1
	xNa1["2000", 1] <- NA
	xNa2 <- x2
	xNa2["2000/2001", 2]  <- NA

	n1 <- tsupdate(xNa1, xNa2, "tsupdna")
	n2 <- tsupdate(xNa2, xNa1, "tsupdna")

})

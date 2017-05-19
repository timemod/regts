context("head, tail & topleft")

test_that("head, tail & topleft for regts", {

    x1 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2002",
            names = c("a", "b", "c"))
	x2 <- regts(matrix(data = rep(1), nc = 3), period = "2000/2001",
            names = c("a", "c", "d"))

	r1 <- tsupdate(x1, x2, "replace")
	r2 <- tsupdate(x2, x1, "replace")

	u1 <- tsupdate(x1, x2, "tsupd")
	u2 <- tsupdate(x2, x1, "tsupd")

# NA's toevoegen


})

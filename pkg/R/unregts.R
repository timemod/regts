# the function unregts converts a regts to a normal ts.
unregts <- function(x) {
  classes <- class(x)
  class(x) <- classes[classes != "regts"]
  return (x)
}

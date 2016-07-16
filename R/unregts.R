# the function unregts converts a regts to a normal ts. This function is
# only used in the package regts for testing purposes. Therefore, it is not
# exported.
unregts <- function(x) {
    class(x) <- class(x)[-1]
    return (x)
}

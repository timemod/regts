regts_default_options <- list(
  regts_max_difnames = 500,
  regts_max_maxdif = 10
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(regts_default_options) %in% names(op))
  if (any(toset)) options(regts_default_options[toset])
  return(invisible())
}

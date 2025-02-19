cran_repo <- "https://cloud.r-project.org"

lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib_dir)) {
  dir.create(lib_dir, recursive = TRUE)
}

cat("\nLibrary paths:\n")
print(.libPaths())

if (!require(devtools)) {
  install.packages("devtools", repos = cran_repo)
}
devtools::install_deps("pkg", dependencies = TRUE, repos = cran_repo)

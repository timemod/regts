#!/usr/bin/Rscript
repo <- "https://cloud.r-project.org"

options(repos = c(CRAN = repo)) 

# If the user library does not exist, then create it
# and add it to the library path.
user_lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib_dir)) {
  dir.create(user_lib_dir, recursive = TRUE)
  .libPaths(user_lib_dir)
}

if (!require(pak)) {
  install.packages("pak")
}
pak::local_install_dev_deps("pkg", upgrade = FALSE)

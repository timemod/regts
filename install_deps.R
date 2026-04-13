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

# install extra packages needed to install isismdl with the install script.
extra_packages <- c("pak", "devtools", "lintr")
for (extra_package in extra_packages) {
  if (!require(extra_package, character.only = TRUE, quietly = TRUE)) {
    install.packages(extra_package, repos = repo)
  }
}

pak::local_install_dev_deps("pkg", upgrade = FALSE)

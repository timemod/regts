source("install_deps.R")

# make sure all packages (including devtools and lintr) are up-to-date.
user_lib_dir <- Sys.getenv("R_LIBS_USER")
update.packages(lib.loc = user_lib_dir, ask = FALSE)

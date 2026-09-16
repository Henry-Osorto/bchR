# Run from any directory: Rscript --vanilla <path>/dev/document.R
# Uses the caller's R libraries; does not install dependencies or connect to BCH.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.unsetenv("BCH_API_KEY")
roxygen2::roxygenise(package_dir)

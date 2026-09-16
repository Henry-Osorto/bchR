# Local documentation only. Does not deploy to GitHub Pages or open a browser.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.unsetenv("BCH_API_KEY")
pkgdown::build_site(pkg = package_dir, preview = FALSE, new_process = FALSE)

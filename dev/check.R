# Build vignettes and check in a fresh directory outside the source tree.
# Optional BCHR_CHECK_DIR chooses a persistent parent for evidence.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), winslash = "/", mustWork = TRUE)
out_root <- Sys.getenv("BCHR_CHECK_DIR", unset = tempdir())
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
out_root <- normalizePath(out_root, winslash = "/", mustWork = TRUE)
compare <- function(x) if (.Platform$OS.type == "windows") tolower(x) else x
if (identical(compare(out_root), compare(package_dir)) ||
    startsWith(compare(out_root), paste0(compare(package_dir), "/"))) {
  stop("BCHR_CHECK_DIR must be outside the package source directory.")
}
Sys.unsetenv("BCH_API_KEY")
Sys.setenv(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false")
if (!rmarkdown::pandoc_available()) {
  stop("Pandoc is required for the offline vignette. Run via RStudio or configure RSTUDIO_PANDOC.")
}
out <- tempfile("bchR-check-", tmpdir = out_root)
dir.create(out)
out <- normalizePath(out, winslash = "/", mustWork = TRUE)
setwd(out)
r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")
run <- function(arguments, log) {
  status <- system2(r, arguments, stdout = log, stderr = log)
  if (!identical(status, 0L)) stop("Command failed; inspect ", file.path(out, log))
}
message("Build and check output: ", out)
run(c("CMD", "build", shQuote(package_dir)), "build.log")
tarballs <- list.files(out, pattern = "\\.tar\\.gz$", full.names = TRUE)
stopifnot(length(tarballs) == 1L)
run(c("CMD", "check", "--no-manual", shQuote(tarballs)), "check.log")
message("Build/check commands completed. Review check.log for the final status: ", out)

# Run from any directory: Rscript --vanilla <path>/dev/test.R
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.unsetenv("BCH_API_KEY")
results <- testthat::test_local(package_dir, reporter = "summary", stop_on_failure = FALSE)
tab <- as.data.frame(results)
out <- file.path(package_dir, "dev", "artifacts")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
report <- list(
  executed_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  r_version = R.version.string, platform = R.version$platform,
  tests = nrow(tab), expectations_passed = sum(tab$passed),
  failures = sum(tab$failed), errors = sum(tab$error),
  warnings = sum(tab$warning), skipped = sum(tab$skipped),
  by_test = tab[setdiff(names(tab), "result")]
)
jsonlite::write_json(report, file.path(out, "test-summary.json"),
                    pretty = TRUE, auto_unbox = TRUE, na = "null")
if (report$failures > 0 || report$errors > 0 || report$warnings > 0) {
  stop("Tests have failures, errors or warnings; inspect dev/artifacts/test-summary.json.")
}

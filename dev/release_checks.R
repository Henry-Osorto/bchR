# bchR release checks --------------------------------------------------------
# Run this file from an R session on a machine with R and package development
# tooling installed. It intentionally does not read or print BCH_API_KEY.

pkg <- normalizePath(
  Sys.getenv("BCHR_PACKAGE_DIR", unset = "."),
  winslash = "/",
  mustWork = TRUE
)

required <- c("devtools", "roxygen2", "testthat", "callr")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Install the following development packages before running checks: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

old_prompt <- getOption("bchR.prompt_api_key")
on.exit(options(bchR.prompt_api_key = old_prompt), add = TRUE)
options(bchR.prompt_api_key = FALSE)

cat("1/5 roxygen2 documentation\n")
devtools::document(pkg = pkg)

cat("2/5 testthat suite\n")
devtools::test(pkg = pkg, reporter = "summary")

cat("3/5 devtools::check()\n")
check_result <- devtools::check(
  pkg = pkg,
  document = FALSE,
  cran = TRUE,
  manual = FALSE,
  error_on = "never"
)
print(check_result)

cat("4/5 build source tarball with R CMD build\n")
built <- devtools::build(
  pkg = pkg,
  path = tempdir(),
  manual = FALSE,
  vignettes = FALSE,
  quiet = FALSE
)
cat("Built:", built, "\n")

cat("5/5 independent R CMD check --as-cran\n")
check_dir <- tempfile("bchR-Rcheck-")
dir.create(check_dir)
result <- callr::rcmd(
  cmd = "check",
  cmdargs = c(
    "--as-cran",
    "--no-manual",
    normalizePath(built, winslash = "/", mustWork = TRUE)
  ),
  wd = check_dir,
  fail_on_status = FALSE,
  echo = TRUE,
  spinner = FALSE
)

if (!identical(result$status, 0L)) {
  stop(
    "R CMD check --as-cran returned non-zero status: ",
    result$status,
    call. = FALSE
  )
}

cat("All executable release checks completed. Inspect check output before release.\n")

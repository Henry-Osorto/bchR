# Structural checks for repository resources; no network or API credentials.
# This checks YAML syntax and local links, not remote workflow execution.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1L])
package_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
Sys.unsetenv("BCH_API_KEY")
setwd(package_dir)
required <- c("DESCRIPTION", "NAMESPACE", "LICENSE", "README.md", "NEWS.md",
  "bchR.Rproj", "CITATION.cff", "inst/CITATION", ".gitignore", ".Rbuildignore",
  ".gitattributes", "CONTRIBUTING.md", "SECURITY.md", "CODE_OF_CONDUCT.md",
  "vignettes/bchR.Rmd", ".github/workflows/R-CMD-check.yaml",
  ".github/workflows/pkgdown.yaml")
stopifnot(all(file.exists(required)))
desc <- read.dcf("DESCRIPTION")
stopifnot(desc[1, "Package"] == "bchR")
yamls <- c(list.files(".github", pattern = "ya?ml$", recursive = TRUE, full.names = TRUE),
           "_pkgdown.yml", "CITATION.cff")
for (file in yamls) {
  stopifnot(is.list(yaml::read_yaml(file)))
  message("YAML parsed: ", file)
}
cff <- yaml::read_yaml("CITATION.cff")
stopifnot(identical(cff$version, unname(desc[1, "Version"])),
          identical(cff[["cff-version"]], "1.2.0"))
# YAML 1.1 parsers interpret the unquoted GitHub key 'on' as TRUE.
# Validate the stable job fields here; do not treat this as a workflow schema validator.
ci <- yaml::read_yaml(".github/workflows/R-CMD-check.yaml")
stopifnot(length(ci$jobs[["R-CMD-check"]]$strategy$matrix$config) == 3L)
code_files <- c(list.files("R", full.names = TRUE), list.files("man", full.names = TRUE),
                list.files("tests", recursive = TRUE, full.names = TRUE))
stale <- vapply(code_files, function(file) {
  any(grepl("bchclientdev", readLines(file, warn = FALSE), fixed = TRUE))
}, logical(1))
stopifnot(!any(stale))
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  archive <- normalizePath(args[[1L]], mustWork = TRUE)
  entries <- utils::untar(archive, list = TRUE)
  forbidden <- "(^|/)(work|dev|docs|project-docs|research-artifacts|\\.git|\\.github|\\.Renviron)(/|$)"
  stopifnot(!any(grepl(forbidden, entries)),
            all(paste0("bchR/", c("DESCRIPTION", "LICENSE", "NAMESPACE", "README.md",
                                  "NEWS.md", "inst/CITATION", "inst/doc/bchR.html")) %in% entries))
  message("Source archive contains expected resources and excludes development/private directories.")
}
if (file.exists("docs/index.html")) {
  pages <- list.files("docs", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  broken <- character()
  for (page in pages) {
    html <- xml2::read_html(page)
    links <- xml2::xml_attr(xml2::xml_find_all(html, "//a[@href]"), "href")
    links <- links[!grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|//|#)", links)]
    targets <- sub("[#?].*$", "", links)
    targets <- unique(targets[nzchar(targets)])
    missing <- targets[!file.exists(file.path(dirname(page), utils::URLdecode(targets)))]
    if (length(missing)) broken <- c(broken, paste(page, missing, sep = " -> "))
  }
  if (length(broken)) stop("Broken local documentation links:\n", paste(broken, collapse = "\n"))
  message("Local HTML links: OK (", length(pages), " pages; external URLs and anchors not tested).")
}
message("Repository resource checks: OK. Remote execution and authenticated API remain unverified.")

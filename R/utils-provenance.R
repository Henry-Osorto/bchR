# Provenance helpers ----------------------------------------------------------

#' Return the current bchR version for provenance
#'
#' @return Package version as a character scalar. During source-only development
#'   where no installed package version can be resolved, returns `"development"`.
#' @noRd
.bch_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("bchR")),
    error = function(e) "development"
  )
}

#' Add secret-free provenance attributes
#'
#' @param x Result object.
#' @param path BCH endpoint path components.
#' @param indicator_id Optional canonical BCH indicator ID.
#' @param retrieved_at Retrieval timestamp.
#'
#' @return `x` with provenance attributes attached.
#' @noRd
.bch_add_provenance <- function(
    x,
    path,
    indicator_id = NULL,
    retrieved_at = Sys.time()
) {
  attr(x, "retrieved_at") <- retrieved_at
  attr(x, "package_version") <- .bch_package_version()

  if (!is.null(indicator_id)) {
    attr(x, "indicator_id") <- indicator_id
  }

  attr(x, "api_endpoint") <- .bch_safe_endpoint(path)
  x
}

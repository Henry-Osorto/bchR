# BCH Web API configuration --------------------------------------------------

#' BCH Web API base URL
#'
#' Returns the fixed base URL used by the internal HTTP layer of `bchR`.
#' Authentication information is deliberately excluded from this value.
#'
#' @return A length-one character string containing the BCH Web API base URL.
#'
#' @noRd
bch_api_base_url <- function() {
  "https://bchapi-am.azure-api.net/api/v1"
}


#' Build the bchR User-Agent string
#'
#' @return A length-one character string identifying `bchR`.
#' @noRd
.bch_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("bchR")),
    error = function(e) "development"
  )

  paste0("bchR/", version, " (R package)")
}


#' Validate BCH API path components
#'
#' `path` is represented as one or more path components rather than as a
#' pre-built URL. This keeps endpoint construction inside the HTTP layer.
#'
#' @param path Character vector of path components.
#'
#' @return `path`, invisibly validated and coerced to character.
#' @noRd
.bch_validate_path <- function(path) {
  if (!is.character(path) || length(path) < 1L || anyNA(path)) {
    stop(
      "path must be a non-empty character vector of URL path components.",
      call. = FALSE
    )
  }

  path <- trimws(path)

  if (any(!nzchar(path))) {
    stop(
      "path components must not be empty or contain only whitespace.",
      call. = FALSE
    )
  }

  # Callers must supply components, not fragments of complete URLs. httr2 is
  # responsible for escaping valid component content.
  if (any(grepl("[/\\?#]", path, perl = TRUE))) {
    stop(
      "path must contain URL path components, not '/', '?' or '#'.",
      call. = FALSE
    )
  }

  path
}


#' Validate non-authentication query parameters
#'
#' @param query Named list of scalar query parameters. `NULL` values are
#'   permitted and omitted by `httr2`.
#'
#' @return A validated named list.
#' @noRd
.bch_validate_query <- function(query) {
  if (!is.list(query)) {
    stop("query must be a named list.", call. = FALSE)
  }

  if (length(query) == 0L) {
    return(query)
  }

  nms <- names(query)

  if (is.null(nms) || anyNA(nms) || any(!nzchar(nms))) {
    stop("Every query parameter must have a non-empty name.", call. = FALSE)
  }

  if (anyDuplicated(tolower(nms))) {
    stop("query parameter names must be unique.", call. = FALSE)
  }

  reserved <- tolower(nms) %in% c("clave", "formato")

  if (any(reserved)) {
    stop(
      "'clave' and 'formato' are managed internally by bchR and must not be supplied in query.",
      call. = FALSE
    )
  }

  valid_value <- vapply(
    query,
    function(value) {
      if (is.null(value)) {
        return(TRUE)
      }

      is.atomic(value) &&
        length(value) == 1L &&
        !is.na(value)
    },
    logical(1)
  )

  if (!all(valid_value)) {
    stop(
      "Each query value must be NULL or a non-missing atomic scalar.",
      call. = FALSE
    )
  }

  query
}


#' Build a secret-free endpoint for provenance
#'
#' Constructs the same endpoint path and non-secret query parameters used by
#' the HTTP layer, but deliberately excludes `BCH_API_KEY`. This helper is the
#' preferred source for future provenance attributes.
#'
#' @param path Character vector of path components.
#' @param query Named list of non-authentication query parameters.
#'
#' @return A complete URL that never contains the `clave` parameter.
#' @noRd
.bch_safe_endpoint <- function(path, query = list()) {
  path <- .bch_validate_path(path)
  query <- .bch_validate_query(query)

  request <- httr2::request(bch_api_base_url())
  request <- do.call(
    httr2::req_url_path_append,
    c(list(req = request), as.list(path))
  )

  safe_query <- c(list(formato = "Json"), query)
  request <- do.call(
    httr2::req_url_query,
    c(list(.req = request), safe_query)
  )

  bch_redact_url(httr2::req_get_url(request))
}

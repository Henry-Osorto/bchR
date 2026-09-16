# Authentication -----------------------------------------------------------

.bch_api_key_env <- "BCH_API_KEY"
.bch_api_key_max_bytes <- 4096L

#' Check whether a BCH API key is configured
#'
#' This internal helper never returns or prints the configured key.
#'
#' @keywords internal
bch_has_api_key <- function() {
  key <- Sys.getenv(.bch_api_key_env, unset = NA_character_)
  .bch_api_key_is_valid(key)
}

#' Resolve a BCH API key without persisting it
#'
#' @param key Optional session-only key. When `NULL`, `BCH_API_KEY` is read.
#'
#' @return A length-one character vector for immediate internal use.
#' @keywords internal
bch_api_key <- function(key = NULL) {
  from_environment <- is.null(key)

  if (from_environment) {
    key <- Sys.getenv(.bch_api_key_env, unset = NA_character_)
  }

  if (!bch_is_scalar_string(key)) {
    if (from_environment) {
      bch_abort(
        paste0(
          "No BCH API key is configured. Set `", .bch_api_key_env,
          "` in your local environment; do not place the key in code."
        ),
        "bch_auth_missing",
        call = NULL
      )
    }

    bch_abort(
      "The BCH API key must be one non-empty character string.",
      "bch_auth_invalid",
      call = NULL
    )
  }

  if (!.bch_api_key_is_valid(key)) {
    bch_abort(
      paste0(
        "The BCH API key has an invalid shape. It must not contain ",
        "control characters, surrounding whitespace, or more than ",
        .bch_api_key_max_bytes, " bytes."
      ),
      "bch_auth_invalid",
      call = NULL
    )
  }

  key
}

.bch_api_key_is_valid <- function(key) {
  if (!bch_is_scalar_string(key) || is.na(key) || !nzchar(key)) {
    return(FALSE)
  }

  identical(key, trimws(key)) &&
    nchar(key, type = "bytes") <= .bch_api_key_max_bytes &&
    !grepl("[[:cntrl:]]", key)
}

#' Add a redacted BCH credential header
#'
#' @param req An `httr2_request`.
#' @param key Optional session-only key; see [bch_api_key()].
#'
#' @return The modified request. `httr2` redacts the `clave` header when the
#'   request is printed.
#' @keywords internal
bch_authenticate_request <- function(req, key = NULL) {
  if (!inherits(req, "httr2_request")) {
    bch_abort(
      "`req` must be an httr2 request.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  key <- bch_api_key(key)

  tryCatch(
    httr2::req_headers_redacted(req, clave = key),
    error = function(cnd) {
      # Deliberately discard the original condition: it may retain a request.
      bch_abort(
        "The BCH authentication header could not be configured.",
        "bch_auth_invalid",
        call = NULL
      )
    }
  )
}

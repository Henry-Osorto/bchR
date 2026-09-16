# HTTP request and retry policy --------------------------------------------

.bch_api_base_url <- "https://bchapi-am.azure-api.net/api/v1"
.bch_default_timeout <- 30
.bch_default_max_attempts <- 3L
.bch_client_version <- "0.0.0.9001"
.bch_default_user_agent <- paste0(
  "BCH-R-client/", .bch_client_version, " ",
  "(+https://github.com/Henry-Osorto)"
)

.bch_sensitive_query_names <- c(
  "clave", "key", "api_key", "apikey", "token", "access_token",
  "authorization", "subscription_key", "subscription-key"
)

#' Build a request to the fixed BCH production API
#'
#' The credential is sent only as a redacted `clave` header. It is never
#' placed in the URL or query string.
#'
#' @param path Relative path below `/api/v1`, either as one slash-separated
#'   string or a character vector of path segments.
#' @param query Named list of non-sensitive scalar query parameters.
#' @param key Optional session-only key; see [bch_api_key()].
#' @param timeout Total request timeout in seconds.
#'
#' @return An `httr2_request` configured for a JSON `GET` request.
#' @keywords internal
bch_api_request <- function(path, query = list(), key = NULL,
                            timeout = .bch_default_timeout) {
  key <- bch_api_key(key)
  path_segments <- .bch_path_segments(path)
  query <- .bch_validate_query(query, key = key)
  timeout <- .bch_validate_timeout(timeout)

  if (.bch_contains_secret(path_segments, key)) {
    bch_abort(
      "A credential-like value was found in the endpoint path.",
      "bch_unsafe_endpoint",
      call = NULL
    )
  }

  req <- httr2::request(.bch_api_base_url)
  req <- do.call(
    httr2::req_url_path_append,
    c(list(req), as.list(path_segments))
  )
  req <- do.call(
    httr2::req_url_query,
    c(list(req), c(query, list(formato = "Json")))
  )
  req <- httr2::req_method(req, "GET")
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_user_agent(req, .bch_default_user_agent)
  req <- httr2::req_options(
    req,
    followlocation = FALSE,
    maxredirs = 0L
  )
  req <- httr2::req_timeout(req, timeout)

  # HTTP statuses are classified by `bch_perform()` rather than by httr2.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- bch_authenticate_request(req, key = key)

  .bch_assert_safe_request(req)
  req
}

#' Perform a BCH request with bounded, testable retries
#'
#' @param req A request created by [bch_api_request()].
#' @param max_attempts Maximum total attempts, including the first.
#' @param base_delay Initial exponential-backoff delay in seconds.
#' @param max_delay Maximum delay between attempts in seconds.
#'   A server-requested wait above this budget defers the request with an
#'   informative error; it is never shortened.
#' @param perform Injected transport function. Tests must supply a fake.
#' @param sleep Injected sleep function.
#' @param jitter Injected function receiving the uncapped backoff and returning
#'   a non-negative jitter in seconds.
#'
#' @return A successful JSON `httr2_response` with a `bch_attempts` attribute.
#' @keywords internal
bch_perform <- function(req,
                        max_attempts = .bch_default_max_attempts,
                        base_delay = 0.5,
                        max_delay = 30,
                        perform = .bch_perform_quietly,
                        sleep = Sys.sleep,
                        jitter = .bch_default_jitter) {
  .bch_assert_safe_request(req)
  max_attempts <- .bch_validate_positive_integer(
    max_attempts,
    "`max_attempts`"
  )
  base_delay <- .bch_validate_delay(base_delay, "`base_delay`")
  max_delay <- .bch_validate_delay(max_delay, "`max_delay`")

  if (!is.function(perform) || !is.function(sleep) || !is.function(jitter)) {
    bch_abort(
      "`perform`, `sleep`, and `jitter` must be functions.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  endpoint <- .bch_request_endpoint(req)

  # Also check here because an internal request may have been modified since
  # construction. Custom credential headers must never follow a redirect.
  if (!identical(req$options$followlocation, FALSE)) {
    bch_abort("BCH requests must disable redirects.", "bch_unsafe_endpoint")
  }

  for (attempt in seq_len(max_attempts)) {
    outcome <- tryCatch(
      list(response = perform(req), failed = FALSE),
      error = function(cnd) {
        # Only transport failures are candidates for retry. Programming errors
        # in a custom transport must not be repeatedly submitted to a server.
        list(failed = TRUE,
             retryable = inherits(cnd, c("httr2_failure", "curl_error")))
      }
    )

    if (isTRUE(outcome$failed)) {
      if (isTRUE(outcome$retryable) && attempt < max_attempts) {
        delay <- .bch_retry_delay(
          response = NULL,
          attempt = attempt,
          base_delay = base_delay,
          max_delay = max_delay,
          jitter = jitter
        )
        sleep(delay)
        next
      }

      bch_abort(
        paste0(
          "The BCH API could not be reached after ", attempt,
          if (attempt == 1L) " attempt." else " attempts."
        ),
        "bch_network_error",
        endpoint = endpoint,
        attempts = attempt,
        retryable = isTRUE(outcome$retryable),
        call = NULL
      )
    }

    response <- outcome$response
    if (!inherits(response, "httr2_response")) {
      bch_abort(
        "The HTTP transport returned an invalid result.",
        "bch_transport_error",
        endpoint = endpoint,
        attempts = attempt,
        call = NULL
      )
    }

    status <- httr2::resp_status(response)
    classification <- bch_classify_http(status)

    if (isTRUE(classification$ok)) {
      .bch_validate_json_response(response, endpoint = endpoint)
      attr(response, "bch_attempts") <- attempt
      return(response)
    }

    if (isTRUE(classification$retryable) && attempt < max_attempts) {
      delay <- .bch_retry_delay(
        response = response,
        attempt = attempt,
        base_delay = base_delay,
        max_delay = max_delay,
        jitter = jitter
      )
      sleep(delay)
      next
    }

    bch_abort(
      paste0(classification$message, " (HTTP ", status, ")."),
      classification$class,
      status = status,
      endpoint = endpoint,
      attempts = attempt,
      retryable = classification$retryable,
      call = NULL
    )
  }

  # Defensive only: every loop branch returns, retries, or aborts.
  bch_abort(
    "The BCH request ended in an unexpected state.",
    "bch_transport_error",
    endpoint = endpoint,
    call = NULL
  )
}

#' Parse a successful BCH response as JSON
#'
#' @param response A response returned by [bch_perform()].
#'
#' @return Parsed JSON as nested R objects.
#' @keywords internal
bch_response_json <- function(response) {
  if (!inherits(response, "httr2_response")) {
    bch_abort(
      "`response` must be an httr2 response.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  endpoint <- .bch_response_endpoint(response)
  .bch_validate_json_response(response, endpoint = endpoint)

  tryCatch(
    httr2::resp_body_json(
      response,
      check_type = FALSE,
      simplifyVector = FALSE,
      bigint_as_char = TRUE
    ),
    error = function(cnd) {
      # Do not retain the response, body, or original parser condition.
      bch_abort(
        "The BCH API returned malformed JSON.",
        "bch_json_error",
        endpoint = endpoint,
        call = NULL
      )
    }
  )
}

#' Classify an HTTP status without inspecting a response body
#'
#' @param status Integer HTTP status code.
#'
#' @return A list with `ok`, `class`, `retryable`, and a safe `message`.
#' @keywords internal
bch_classify_http <- function(status) {
  if (
    length(status) != 1L || !is.numeric(status) || is.na(status) ||
      !is.finite(status) || status < 100 || status > 599 || status != floor(status)
  ) {
    bch_abort(
      "`status` must be one integer HTTP status between 100 and 599.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  status <- as.integer(status)

  if (status >= 200L && status <= 299L) {
    return(list(
      ok = TRUE,
      class = NA_character_,
      retryable = FALSE,
      message = "Success"
    ))
  }
  if (status >= 300L && status <= 399L) {
    return(list(
      ok = FALSE,
      class = "bch_http_redirect",
      retryable = FALSE,
      message = "The BCH API returned a redirect, which was not followed"
    ))
  }
  if (status %in% c(401L, 403L)) {
    return(list(
      ok = FALSE,
      class = "bch_http_auth",
      retryable = FALSE,
      message = "The BCH API rejected the credential or its permissions"
    ))
  }
  if (status == 404L) {
    return(list(
      ok = FALSE,
      class = "bch_http_not_found",
      retryable = FALSE,
      message = "The requested BCH resource was not found"
    ))
  }
  if (status == 408L) {
    return(list(
      ok = FALSE,
      class = "bch_http_timeout",
      retryable = TRUE,
      message = "The BCH API timed out while processing the request"
    ))
  }
  if (status == 429L) {
    return(list(
      ok = FALSE,
      class = "bch_http_rate_limit",
      retryable = TRUE,
      message = "The BCH API rate limit was reached"
    ))
  }
  if (status >= 500L) {
    return(list(
      ok = FALSE,
      class = "bch_http_server",
      retryable = status %in% c(500L, 502L, 503L, 504L),
      message = "The BCH API reported a server error"
    ))
  }
  if (status >= 400L) {
    return(list(
      ok = FALSE,
      class = "bch_http_client",
      retryable = FALSE,
      message = "The BCH API rejected the request"
    ))
  }

  list(
    ok = FALSE,
    class = "bch_http_unexpected",
    retryable = FALSE,
    message = "The BCH API returned an unexpected HTTP status"
  )
}

.bch_path_segments <- function(path) {
  if (!is.character(path) || length(path) == 0L || anyNA(path)) {
    bch_abort(
      "`path` must contain one or more non-missing character segments.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  if (length(path) == 1L) {
    path <- sub("^/+", "", path)
    path <- sub("/+$", "", path)
    path <- strsplit(path, "/", fixed = TRUE)[[1L]]
  }

  if (!length(path)) {
    bch_abort("`path` must identify an endpoint.", "bch_invalid_argument")
  }

  unsafe <-
    !nzchar(path) |
    path %in% c(".", "..") |
    grepl("[?#%\\\\]", path) |
    grepl("://", path, fixed = TRUE) |
    grepl("[[:cntrl:]]", path)

  if (any(unsafe)) {
    bch_abort(
      "`path` contains an unsafe or invalid endpoint segment.",
      "bch_unsafe_endpoint",
      call = NULL
    )
  }

  path
}

.bch_validate_query <- function(query, key) {
  if (!is.list(query)) {
    bch_abort(
      "`query` must be a named list.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  if (length(query) == 0L) {
    return(query)
  }

  if (is.null(names(query))) {
    bch_abort(
      "`query` must be a named list.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  query_names <- names(query)
  normalized_names <- tolower(query_names)

  if (
    any(!nzchar(query_names)) || anyDuplicated(normalized_names) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", query_names))
  ) {
    bch_abort(
      "`query` names must be unique, non-empty, and URL-safe.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  if (any(normalized_names %in% .bch_sensitive_query_names)) {
    bch_abort(
      "Credentials and authorization values are forbidden in the query string.",
      "bch_unsafe_endpoint",
      call = NULL
    )
  }

  formato <- which(normalized_names == "formato")
  if (length(formato) == 1L) {
    value <- query[[formato]]
    if (
      !bch_is_scalar_string(value) || is.na(value) ||
        !identical(tolower(value), "json")
    ) {
      bch_abort(
        "The BCH client supports only JSON responses.",
        "bch_invalid_argument",
        call = NULL
      )
    }
    query <- query[-formato]
  }

  valid_value <- vapply(
    query,
    function(value) {
      is.atomic(value) && length(value) == 1L && !is.na(value) &&
        !is.raw(value) &&
        (!is.numeric(value) || is.finite(value))
    },
    logical(1)
  )

  if (!all(valid_value)) {
    bch_abort(
      "Every query value must be one finite, non-missing atomic value.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  if (.bch_contains_secret(unlist(query, use.names = FALSE), key)) {
    bch_abort(
      "A credential-like value was found in the query string.",
      "bch_unsafe_endpoint",
      call = NULL
    )
  }

  query
}

.bch_contains_secret <- function(x, key) {
  if (length(x) == 0L) {
    return(FALSE)
  }

  any(vapply(
    as.character(x),
    function(value) grepl(key, value, fixed = TRUE),
    logical(1)
  ))
}

.bch_validate_timeout <- function(timeout) {
  if (
    length(timeout) != 1L || is.na(timeout) || !is.numeric(timeout) ||
      !is.finite(timeout) || timeout < 0.001 || timeout > 300
  ) {
    bch_abort(
      "`timeout` must be one finite number from 0.001 through 300 seconds.",
      "bch_invalid_argument",
      call = NULL
    )
  }
  as.numeric(timeout)
}

.bch_validate_positive_integer <- function(x, name) {
  if (
    length(x) != 1L || !is.numeric(x) || is.na(x) ||
      !is.finite(x) || x < 1 || x > 10 || x != floor(x)
  ) {
    bch_abort(
      paste0(name, " must be one integer from 1 through 10."),
      "bch_invalid_argument",
      call = NULL
    )
  }
  as.integer(x)
}

.bch_validate_delay <- function(x, name) {
  if (
    length(x) != 1L || is.na(x) || !is.numeric(x) ||
      !is.finite(x) || x < 0 || x > 300
  ) {
    bch_abort(
      paste0(name, " must be one finite number from 0 through 300 seconds."),
      "bch_invalid_argument",
      call = NULL
    )
  }
  as.numeric(x)
}

.bch_assert_safe_request <- function(req) {
  if (!inherits(req, "httr2_request")) {
    bch_abort(
      "`req` must be an httr2 request.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  url <- req$url
  fixed_origin <- identical(url, .bch_api_base_url) ||
    startsWith(url, paste0(.bch_api_base_url, "/"))
  has_sensitive_query <- grepl(
    "[?&](clave|key|api_key|apikey|token|access_token|authorization|subscription[_-]key)=",
    url,
    ignore.case = TRUE,
    perl = TRUE
  )

  if (!fixed_origin || has_sensitive_query) {
    bch_abort(
      "The request does not target the fixed, credential-safe BCH endpoint.",
      "bch_unsafe_endpoint",
      call = NULL
    )
  }

  invisible(req)
}

.bch_request_endpoint <- function(req) {
  sub("[?#].*$", "", req$url)
}

.bch_response_endpoint <- function(response) {
  url <- response$url
  if (!bch_is_scalar_string(url)) {
    return(.bch_api_base_url)
  }

  endpoint <- sub("[?#].*$", "", url)
  if (
    identical(endpoint, .bch_api_base_url) ||
      startsWith(endpoint, paste0(.bch_api_base_url, "/"))
  ) {
    endpoint
  } else {
    .bch_api_base_url
  }
}

.bch_validate_json_response <- function(response, endpoint) {
  content_type <- tryCatch(
    httr2::resp_content_type(response),
    error = function(cnd) NULL
  )
  is_json <- bch_is_scalar_string(content_type) &&
    grepl("^(application|text)/([A-Za-z0-9.+-]*\\+)?json$", content_type)

  if (!is_json) {
    bch_abort(
      "The BCH API response did not declare a JSON content type.",
      "bch_http_content_type",
      endpoint = endpoint,
      call = NULL
    )
  }

  invisible(response)
}

.bch_default_jitter <- function(backoff) {
  stats::runif(1L, min = 0, max = backoff * 0.25)
}

.bch_retry_delay <- function(response, attempt, base_delay, max_delay,
                             jitter) {
  retry_after <- NA_real_
  if (!is.null(response)) {
    retry_after <- suppressWarnings(
      tryCatch(
        httr2::resp_retry_after(response),
        error = function(cnd) NA_real_
      )
    )
  }

  if (length(retry_after) == 1L && is.finite(retry_after) && retry_after >= 0) {
    if (retry_after > max_delay) {
      bch_abort(
        "The server requires a longer wait. Retry after the indicated delay.",
        "bch_retry_deferred", retry_after_seconds = retry_after,
        retryable = TRUE, attempts = as.integer(attempt),
        endpoint = .bch_response_endpoint(response)
      )
    }
    return(retry_after)
  }

  backoff <- min(max_delay, base_delay * (2 ^ (attempt - 1L)))
  extra <- tryCatch(jitter(backoff), error = function(cnd) NA_real_)

  if (
    length(extra) != 1L || is.na(extra) || !is.numeric(extra) ||
      !is.finite(extra) || extra < 0
  ) {
    bch_abort(
      "`jitter` must return one finite, non-negative number.",
      "bch_invalid_argument",
      call = NULL
    )
  }

  min(max_delay, backoff + extra)
}

.bch_perform_quietly <- function(req) {
  httr2::req_perform(req, verbosity = 0)
}

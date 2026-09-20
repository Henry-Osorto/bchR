# Internal BCH Web API HTTP client ------------------------------------------

# Transient HTTP statuses that bchR retries automatically.
.bch_transient_statuses <- c(429L, 500L, 502L, 503L, 504L)


#' Signal an internal bchR HTTP-layer error
#'
#' Uses only base R conditions to avoid adding an error-handling dependency.
#'
#' @param message Fixed, secret-free user-facing message.
#' @param subclass Specific bchR condition subclass.
#'
#' @return This function does not return; it signals an error.
#' @noRd
.bch_http_abort <- function(message, subclass) {
  classes <- unique(c(subclass, "bchR_http_error", "error", "condition"))

  condition <- structure(
    list(
      message = message,
      call = NULL
    ),
    class = classes
  )

  stop(condition)
}


#' Validate HTTP-layer numeric controls
#'
#' @param timeout_sec Positive finite request timeout in seconds.
#' @param max_tries Integer number of total attempts, including the first.
#' @param simplify_vector Whether JSON vectors should be simplified.
#'
#' @return `TRUE`, invisibly.
#' @noRd
.bch_validate_http_controls <- function(timeout_sec, max_tries, simplify_vector) {
  if (
    !is.numeric(timeout_sec) ||
      length(timeout_sec) != 1L ||
      is.na(timeout_sec) ||
      !is.finite(timeout_sec) ||
      timeout_sec <= 0
  ) {
    stop("timeout_sec must be a positive finite numeric value.", call. = FALSE)
  }

  if (
    !is.numeric(max_tries) ||
      length(max_tries) != 1L ||
      is.na(max_tries) ||
      !is.finite(max_tries) ||
      max_tries < 2 ||
      max_tries != as.integer(max_tries)
  ) {
    stop("max_tries must be an integer greater than or equal to 2.", call. = FALSE)
  }

  if (
    !is.logical(simplify_vector) ||
      length(simplify_vector) != 1L ||
      is.na(simplify_vector)
  ) {
    stop("simplify_vector must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}


#' Identify transient BCH HTTP responses
#'
#' @param response An `httr2_response`.
#'
#' @return A single logical value.
#' @noRd
.bch_is_transient_response <- function(response) {
  httr2::resp_status(response) %in% .bch_transient_statuses
}


#' Build an authenticated BCH GET request
#'
#' Constructs a request from path components and query parameters using
#' `httr2`. The API key is retrieved exclusively with `bch_api_key()` and is
#' inserted only at the final query-construction step.
#'
#' The returned request contains the credential in its URL because the BCH API
#' currently authenticates with the `clave` query parameter. Consequently this
#' function is internal and its return value must never be printed, returned by
#' a public function, persisted, or included in a condition message.
#'
#' @param path Character vector of endpoint path components.
#' @param query Named list of non-authentication query parameters.
#' @param timeout_sec Positive request timeout in seconds.
#' @param max_tries Number of total attempts for transient responses/failures.
#'
#' @return An internal `httr2_request` object.
#'
#' @noRd
.bch_build_request <- function(
    path,
    query = list(),
    timeout_sec = 60,
    max_tries = 5L
) {
  path <- .bch_validate_path(path)
  query <- .bch_validate_query(query)

  .bch_validate_http_controls(
    timeout_sec = timeout_sec,
    max_tries = max_tries,
    simplify_vector = TRUE
  )

  # Build every non-secret component before retrieving the API key. This keeps
  # the credential in memory for the shortest practical interval.
  request <- httr2::request(bch_api_base_url())
  request <- do.call(
    httr2::req_url_path_append,
    c(list(req = request), as.list(path))
  )

  request <- request |>
    httr2::req_method("GET") |>
    httr2::req_user_agent(.bch_user_agent()) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = as.integer(max_tries),
      retry_on_failure = TRUE,
      is_transient = .bch_is_transient_response
    ) |>
    # Disable httr2's default conversion of 4xx/5xx responses into errors so
    # bchR can provide stable, credential-safe, API-specific conditions.
    httr2::req_error(is_error = function(response) FALSE)

  key <- bch_api_key()
  on.exit(key <- NULL, add = TRUE)

  request_query <- c(
    list(formato = "Json"),
    query,
    list(clave = key)
  )

  request <- tryCatch(
    do.call(
      httr2::req_url_query,
      c(list(.req = request), request_query)
    ),
    error = function(e) {
      .bch_http_abort(
        "Could not construct the BCH Web API request.",
        "bchR_request_build_error"
      )
    }
  )

  key <- NULL
  request
}


#' Perform a BCH request without verbose output
#'
#' Kept separate to make request execution mockable in unit tests. Explicit
#' `verbosity = 0` is important because the BCH credential is transported in a
#' query parameter rather than in a redacted authentication header.
#'
#' @param request Internal `httr2_request`.
#'
#' @return An `httr2_response`.
#' @noRd
.bch_perform_request <- function(request) {
  httr2::req_perform(
    request,
    verbosity = 0
  )
}


#' Determine whether a transport failure is a timeout
#'
#' The diagnostic message is inspected only after redaction and is never
#' forwarded to the user.
#'
#' @param error An R condition.
#'
#' @return A single logical value.
#' @noRd
.bch_is_timeout_error <- function(error) {
  classes <- class(error)
  messages <- conditionMessage(error)

  parent <- error$parent
  if (!is.null(parent) && inherits(parent, "condition")) {
    classes <- c(classes, class(parent))
    messages <- c(messages, conditionMessage(parent))
  }

  class_timeout <- any(
    grepl("timeout|timedout|timed_out", classes, ignore.case = TRUE)
  )

  safe_message <- tolower(
    paste(bch_redact_url(messages), collapse = " ")
  )

  message_timeout <- grepl(
    "timed[ -]?out|timeout|time[ -]?out",
    safe_message,
    perl = TRUE
  )

  isTRUE(class_timeout || message_timeout)
}


#' Convert a low-level request failure into a safe bchR condition
#'
#' @param error Original condition. Its raw message is never exposed.
#'
#' @return This function does not return; it signals an error.
#' @noRd
.bch_stop_transport_error <- function(error) {
  if (.bch_is_timeout_error(error)) {
    .bch_http_abort(
      paste0(
        "The request to the BCH Web API timed out. ",
        "Try again later or increase timeout_sec."
      ),
      "bchR_timeout_error"
    )
  }

  .bch_http_abort(
    paste0(
      "Could not connect to the BCH Web API. ",
      "Check the internet connection and BCH service availability."
    ),
    "bchR_connection_error"
  )
}


#' Perform a request while sanitizing warnings and failures
#'
#' @param request Internal `httr2_request`.
#'
#' @return An `httr2_response`.
#' @noRd
.bch_perform_safely <- function(request) {
  captured_warnings <- character()

  response <- tryCatch(
    withCallingHandlers(
      .bch_perform_request(request),
      warning = function(w) {
        captured_warnings <<- c(
          captured_warnings,
          bch_redact_url(conditionMessage(w))
        )
        invokeRestart("muffleWarning")
      }
    ),
    httr2_failure = function(e) {
      .bch_stop_transport_error(e)
    },
    error = function(e) {
      # Defensive fallback: never forward an arbitrary lower-level error
      # message because a request condition may contain the complete URL.
      .bch_stop_transport_error(e)
    }
  )

  if (length(captured_warnings) > 0L) {
    warning(
      paste(unique(captured_warnings), collapse = "\n"),
      call. = FALSE
    )
  }

  response
}


#' Detect an indicator-data endpoint
#'
#' @param path Character path components.
#'
#' @return A single logical value.
#' @noRd
.bch_is_indicator_data_path <- function(path) {
  length(path) >= 3L &&
    identical(tolower(path[[1L]]), "indicadores") &&
    identical(tolower(path[[length(path)]]), "cifras")
}


#' Validate BCH HTTP status codes
#'
#' The request is configured so HTTP errors are returned as responses. This
#' helper translates those responses into stable bchR-specific conditions
#' without reading or printing the response URL.
#'
#' @param response An `httr2_response`.
#' @param path Original endpoint path components.
#'
#' @return `response`, invisibly, for successful 2xx statuses.
#' @noRd
.bch_check_http_status <- function(response, path) {
  status <- httr2::resp_status(response)

  if (status >= 200L && status < 300L) {
    return(invisible(response))
  }

  if (status %in% c(401L, 403L)) {
    .bch_http_abort(
      paste0(
        "BCH Web API authentication or authorization failed (HTTP ",
        status,
        "). Verify BCH_API_KEY and confirm that the BCH API subscription is active."
      ),
      "bchR_authentication_error"
    )
  }

  if (status == 404L) {
    if (.bch_is_indicator_data_path(path)) {
      .bch_http_abort(
        paste0(
          "The requested BCH indicator was not found (HTTP 404). ",
          "Verify the indicator ID."
        ),
        "bchR_not_found_error"
      )
    }

    .bch_http_abort(
      "The requested BCH Web API resource was not found (HTTP 404).",
      "bchR_not_found_error"
    )
  }

  if (status == 429L) {
    .bch_http_abort(
      paste0(
        "The BCH Web API rate limit was exceeded (HTTP 429) after automatic retries. ",
        "Try again later."
      ),
      "bchR_rate_limit_error"
    )
  }

  if (status %in% c(500L, 502L, 503L, 504L)) {
    .bch_http_abort(
      paste0(
        "The BCH Web API is temporarily unavailable (HTTP ",
        status,
        ") after automatic retries. Try again later."
      ),
      "bchR_server_error"
    )
  }

  if (status >= 400L && status < 500L) {
    .bch_http_abort(
      paste0("The BCH Web API rejected the request (HTTP ", status, ")."),
      "bchR_client_error"
    )
  }

  .bch_http_abort(
    paste0("The BCH Web API returned an unexpected HTTP status (", status, ")."),
    "bchR_http_status_error"
  )
}


#' Parse a BCH JSON response
#'
#' A physically empty/whitespace-only response and JSON `null` are treated as
#' empty responses. A valid empty JSON array (`[]`) is preserved and left for
#' the calling data function to interpret as "no records".
#'
#' @param response Successful `httr2_response`.
#' @param simplify_vector Passed to [httr2::resp_body_json()].
#'
#' @return Parsed JSON content.
#' @noRd
.bch_parse_json_response <- function(response, simplify_vector = TRUE) {
  if (!httr2::resp_has_body(response)) {
    .bch_http_abort(
      "The BCH Web API returned an empty response.",
      "bchR_empty_response_error"
    )
  }

  raw_body <- httr2::resp_body_raw(response)

  if (length(raw_body) == 0L) {
    .bch_http_abort(
      "The BCH Web API returned an empty response.",
      "bchR_empty_response_error"
    )
  }

  body_text <- tryCatch(
    rawToChar(raw_body),
    error = function(e) NA_character_
  )

  if (!is.na(body_text) && !nzchar(trimws(body_text))) {
    .bch_http_abort(
      "The BCH Web API returned an empty response.",
      "bchR_empty_response_error"
    )
  }

  parsed <- tryCatch(
    httr2::resp_body_json(
      response,
      check_type = FALSE,
      simplifyVector = simplify_vector
    ),
    error = function(e) {
      .bch_http_abort(
        "The BCH Web API returned malformed or non-JSON content.",
        "bchR_json_error"
      )
    }
  )

  if (is.null(parsed)) {
    .bch_http_abort(
      "The BCH Web API returned an empty JSON response.",
      "bchR_empty_response_error"
    )
  }

  parsed
}


#' Retrieve a resource from the BCH Web API
#'
#' Internal GET client used by public `bchR` data-access functions. It builds
#' requests from path components, obtains `BCH_API_KEY` exclusively through
#' `bch_api_key()`, retries transient responses, validates the final HTTP
#' status, and parses JSON without ever returning the authenticated request or
#' response object.
#'
#' @param path Character vector containing endpoint path components, e.g.
#'   `"indicadores"` or `c("indicadores", "609", "cifras")`.
#' @param query Named list of additional non-authentication query parameters.
#'   The reserved parameters `formato` and `clave` are managed internally.
#' @param timeout_sec Positive finite request timeout in seconds.
#' @param max_tries Integer number of total request attempts for transient HTTP
#'   statuses and low-level transport failures. Must be at least 2.
#' @param simplify_vector Logical passed to [httr2::resp_body_json()]. The BCH
#'   endpoints used by the initial package return tabular JSON, so the default
#'   is `TRUE`.
#'
#' @return Parsed JSON content. The authenticated request/response object is
#'   never returned because its URL contains `BCH_API_KEY`.
#'
#' @noRd
bch_get <- function(
    path,
    query = list(),
    timeout_sec = 60,
    max_tries = 5L,
    simplify_vector = TRUE
) {
  path <- .bch_validate_path(path)
  query <- .bch_validate_query(query)

  .bch_validate_http_controls(
    timeout_sec = timeout_sec,
    max_tries = max_tries,
    simplify_vector = simplify_vector
  )

  request <- .bch_build_request(
    path = path,
    query = query,
    timeout_sec = timeout_sec,
    max_tries = max_tries
  )

  response <- .bch_perform_safely(request)

  .bch_check_http_status(
    response = response,
    path = path
  )

  .bch_parse_json_response(
    response = response,
    simplify_vector = simplify_vector
  )
}

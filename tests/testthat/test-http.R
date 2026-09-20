testthat::test_that("bch_api_base_url returns the fixed BCH API root", {
  testthat::expect_identical(
    bch_api_base_url(),
    "https://bchapi-am.azure-api.net/api/v1"
  )
})


testthat::test_that("path validation rejects URL fragments", {
  testthat::expect_error(.bch_validate_path(character()))
  testthat::expect_error(.bch_validate_path(c("indicadores", NA_character_)))
  testthat::expect_error(.bch_validate_path("indicadores/609/cifras"))
  testthat::expect_error(.bch_validate_path("indicadores?x=1"))
  testthat::expect_identical(
    .bch_validate_path(c("indicadores", "609", "cifras")),
    c("indicadores", "609", "cifras")
  )
})


testthat::test_that("query validation reserves authentication and format parameters", {
  testthat::expect_identical(.bch_validate_query(list()), list())
  testthat::expect_error(.bch_validate_query(list(clave = "x")))
  testthat::expect_error(.bch_validate_query(list(CLAVE = "x")))
  testthat::expect_error(.bch_validate_query(list(formato = "Json")))
  testthat::expect_error(.bch_validate_query(list(x = c(1, 2))))
  testthat::expect_error(.bch_validate_query(list(x = NA_character_)))
  testthat::expect_identical(
    .bch_validate_query(list(x = "one", y = 2, z = NULL)),
    list(x = "one", y = 2, z = NULL)
  )
})


testthat::test_that("HTTP controls are validated", {
  testthat::expect_silent(.bch_validate_http_controls(60, 5L, TRUE))
  testthat::expect_error(.bch_validate_http_controls(0, 5L, TRUE))
  testthat::expect_error(.bch_validate_http_controls(Inf, 5L, TRUE))
  testthat::expect_error(.bch_validate_http_controls(60, 1L, TRUE))
  testthat::expect_error(.bch_validate_http_controls(60, 2.5, TRUE))
  testthat::expect_error(.bch_validate_http_controls(60, 5L, NA))
})


testthat::test_that("transient response predicate matches the required statuses", {
  transient <- c(429L, 500L, 502L, 503L, 504L)
  permanent <- c(200L, 400L, 401L, 403L, 404L, 418L)

  for (status in transient) {
    response <- httr2::response(status_code = status)
    testthat::expect_true(.bch_is_transient_response(response))
  }

  for (status in permanent) {
    response <- httr2::response(status_code = status)
    testthat::expect_false(.bch_is_transient_response(response))
  }
})


testthat::test_that("authenticated request is GET and built with httr2", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    request <- .bch_build_request(
      path = c("indicadores", "609", "cifras"),
      timeout_sec = 12,
      max_tries = 2L
    )

    testthat::expect_identical(
      httr2::req_get_method(request),
      "GET"
    )

    url <- httr2::req_get_url(request)

    testthat::expect_match(url, "/api/v1/indicadores/609/cifras", fixed = TRUE)
    testthat::expect_match(url, "formato=Json", fixed = TRUE)
    testthat::expect_match(url, "clave=", fixed = TRUE)
    testthat::expect_true(grepl(secret, url, fixed = TRUE))

    # req_user_agent() is stored as the libcurl `useragent` option, not as
    # a custom header returned by req_get_headers().
    testthat::expect_identical(
      request$options$useragent,
      .bch_user_agent()
    )
    testthat::expect_match(request$options$useragent, "^bchR/")
  })
})


testthat::test_that("safe provenance endpoint never contains the API key", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    endpoint <- .bch_safe_endpoint(
      c("indicadores", "609", "cifras")
    )

    testthat::expect_match(endpoint, "formato=Json", fixed = TRUE)
    testthat::expect_false(grepl("clave=", endpoint, fixed = TRUE))
    testthat::expect_false(grepl(secret, endpoint, fixed = TRUE))
  })
})


testthat::test_that("request construction fails clearly when API key is absent", {
  .bchr_with_no_test_key({
    testthat::expect_error(
      .bch_build_request("indicadores", max_tries = 2L),
      "BCH_API_KEY is not configured",
      fixed = TRUE
    )
  })
})


testthat::test_that("HTTP authentication failures have a dedicated condition", {
  for (status in c(401L, 403L)) {
    response <- httr2::response(status_code = status)

    testthat::expect_error(
      .bch_check_http_status(response, "indicadores"),
      class = "bchR_authentication_error"
    )
  }
})


testthat::test_that("indicator 404 is distinguished from a general missing resource", {
  indicator_response <- httr2::response(status_code = 404L)

  testthat::expect_error(
    .bch_check_http_status(
      indicator_response,
      c("indicadores", "999999", "cifras")
    ),
    "indicator was not found",
    class = "bchR_not_found_error"
  )

  testthat::expect_error(
    .bch_check_http_status(indicator_response, "other-resource"),
    "resource was not found",
    class = "bchR_not_found_error"
  )
})


testthat::test_that("rate-limit and server responses have dedicated conditions", {
  testthat::expect_error(
    .bch_check_http_status(httr2::response(429L), "indicadores"),
    class = "bchR_rate_limit_error"
  )

  for (status in c(500L, 502L, 503L, 504L)) {
    testthat::expect_error(
      .bch_check_http_status(httr2::response(status), "indicadores"),
      class = "bchR_server_error"
    )
  }
})


testthat::test_that("unexpected client status remains distinguishable", {
  testthat::expect_error(
    .bch_check_http_status(httr2::response(418L), "indicadores"),
    class = "bchR_client_error"
  )
})


testthat::test_that("timeout failure is converted without leaking its raw message", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    original <- structure(
      list(
        message = paste0(
          "Operation timed out requesting https://example.invalid/?clave=",
          secret
        ),
        call = NULL
      ),
      class = c("httr2_failure", "httr2_error", "error", "condition")
    )

    condition <- tryCatch(
      .bch_stop_transport_error(original),
      error = identity
    )

    testthat::expect_s3_class(condition, "bchR_timeout_error")
    testthat::expect_false(
      grepl(secret, conditionMessage(condition), fixed = TRUE)
    )
    testthat::expect_false(
      grepl("clave=", conditionMessage(condition), fixed = TRUE)
    )
  })
})


testthat::test_that("generic transport failure is converted without raw details", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    original <- structure(
      list(
        message = paste0(
          "Could not resolve host for https://example.invalid/?clave=",
          secret
        ),
        call = NULL
      ),
      class = c("httr2_failure", "httr2_error", "error", "condition")
    )

    condition <- tryCatch(
      .bch_stop_transport_error(original),
      error = identity
    )

    testthat::expect_s3_class(condition, "bchR_connection_error")
    testthat::expect_false(
      grepl(secret, conditionMessage(condition), fixed = TRUE)
    )
  })
})


testthat::test_that("empty HTTP body is reported explicitly", {
  response <- httr2::response(
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = raw()
  )

  testthat::expect_error(
    .bch_parse_json_response(response),
    class = "bchR_empty_response_error"
  )
})


testthat::test_that("JSON null is treated as an empty JSON response", {
  response <- httr2::response(
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = charToRaw("null")
  )

  testthat::expect_error(
    .bch_parse_json_response(response),
    class = "bchR_empty_response_error"
  )
})


testthat::test_that("malformed JSON is reported explicitly", {
  response <- httr2::response(
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = charToRaw("{not-valid-json")
  )

  testthat::expect_error(
    .bch_parse_json_response(response),
    class = "bchR_json_error"
  )
})


testthat::test_that("valid empty JSON array remains a valid parsed response", {
  response <- httr2::response(
    status_code = 200L,
    headers = list("content-type" = "application/json"),
    body = charToRaw("[]")
  )

  result <- .bch_parse_json_response(response, simplify_vector = TRUE)
  testthat::expect_length(result, 0L)
})


testthat::test_that("bch_get performs a complete mocked GET without network access", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")
    captured_url <- NULL
    captured_method <- NULL

    mock <- function(req) {
      captured_url <<- httr2::req_get_url(req)
      captured_method <<- httr2::req_get_method(req)

      httr2::response_json(
        status_code = 200L,
        body = list(
          list(Id = 1L, Nombre = "Example")
        )
      )
    }

    httr2::local_mocked_responses(mock)

    result <- bch_get(
      path = "indicadores",
      timeout_sec = 5,
      max_tries = 2L,
      simplify_vector = TRUE
    )

    testthat::expect_identical(captured_method, "GET")
    testthat::expect_match(captured_url, "formato=Json", fixed = TRUE)
    testthat::expect_true(grepl(secret, captured_url, fixed = TRUE))
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_identical(result$Id, 1L)
    testthat::expect_identical(result$Nombre, "Example")
  })
})


testthat::test_that("bch_get converts a mocked 401 into a safe authentication error", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    httr2::local_mocked_responses(
      list(httr2::response(status_code = 401L))
    )

    condition <- tryCatch(
      bch_get(
        path = "indicadores",
        timeout_sec = 5,
        max_tries = 2L
      ),
      error = identity
    )

    testthat::expect_s3_class(condition, "bchR_authentication_error")
    testthat::expect_false(
      grepl(secret, conditionMessage(condition), fixed = TRUE)
    )
    testthat::expect_false(
      grepl("clave=", conditionMessage(condition), fixed = TRUE)
    )
  })
})

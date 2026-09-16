test_that("API keys are resolved and validated without disclosure", {
  withr::local_envvar(BCH_API_KEY = NA)

  err <- expect_error(
    bch_api_key(),
    class = "bch_auth_missing"
  )
  expect_null(err$call)

  withr::local_envvar(BCH_API_KEY = "fixture-key")
  expect_true(bch_has_api_key())
  expect_identical(bch_api_key(), "fixture-key")

  expect_error(
    bch_api_key(" fixture-key"),
    class = "bch_auth_invalid"
  )
  expect_error(
    bch_api_key("fixture\nkey"),
    class = "bch_auth_invalid"
  )
  expect_error(
    bch_api_key(list("fixture-key")),
    class = "bch_auth_invalid"
  )
})

test_that("request construction fixes the host and keeps credentials out of URLs", {
  secret <- "TEST-ONLY-SECRET-78421"
  req <- bch_api_request(
    c("indicadores", "609", "cifras"),
    query = list(desde = "2024-01-01"),
    key = secret,
    timeout = 12
  )

  expect_s3_class(req, "httr2_request")
  expect_true(startsWith(
    req$url,
    "https://bchapi-am.azure-api.net/api/v1/indicadores/609/cifras"
  ))
  expect_match(req$url, "formato=Json", fixed = TRUE)
  expect_false(grepl(secret, req$url, fixed = TRUE))
  expect_false(grepl("clave=", req$url, fixed = TRUE))
  expect_identical(req$method, "GET")
  expect_identical(req$options$followlocation, FALSE)
  expect_identical(req$options$maxredirs, 0L)
  expect_equal(req$options$timeout_ms, 12000)

  printed <- paste(capture.output(print(req)), collapse = "\n")
  expect_match(printed, "clave\\s*: <REDACTED>")
  expect_false(grepl(secret, printed, fixed = TRUE))
})

test_that("sensitive or non-JSON query parameters are rejected", {
  secret <- "TEST-ONLY-SECRET-12194"

  expect_error(
    bch_api_request("indicadores", list(clave = secret), key = secret),
    class = "bch_unsafe_endpoint"
  )
  expect_error(
    bch_api_request("indicadores", list(x = secret), key = secret),
    class = "bch_unsafe_endpoint"
  )
  expect_error(
    bch_api_request(secret, key = secret),
    class = "bch_unsafe_endpoint"
  )
  expect_error(
    bch_api_request("indicadores", list(formato = "xml"), key = secret),
    class = "bch_invalid_argument"
  )
})

test_that("HTTP status classification is explicit", {
  cases <- list(
    `200` = c(NA_character_, "FALSE", "TRUE"),
    `302` = c("bch_http_redirect", "FALSE", "FALSE"),
    `401` = c("bch_http_auth", "FALSE", "FALSE"),
    `404` = c("bch_http_not_found", "FALSE", "FALSE"),
    `408` = c("bch_http_timeout", "TRUE", "FALSE"),
    `429` = c("bch_http_rate_limit", "TRUE", "FALSE"),
    `503` = c("bch_http_server", "TRUE", "FALSE")
  )

  for (status in names(cases)) {
    classified <- bch_classify_http(as.integer(status))
    expected <- cases[[status]]
    expect_identical(classified$class, expected[[1L]])
    expect_identical(classified$retryable, identical(expected[[2L]], "TRUE"))
    expect_identical(classified$ok, identical(expected[[3L]], "TRUE"))
  }
})

test_that("manual retries are deterministic under injected transport", {
  secret <- "TEST-ONLY-SECRET-29344"
  req <- bch_api_request("indicadores", key = secret)
  attempts <- 0L
  delays <- numeric()

  perform <- function(request) {
    attempts <<- attempts + 1L
    if (attempts == 1L) {
      return(httr2::response(
        status_code = 503L,
        url = .bch_api_base_url,
        headers = list(
          `content-type` = "application/json",
          `retry-after` = "0"
        ),
        body = charToRaw('{"temporary":true}')
      ))
    }

    httr2::response(
      status_code = 200L,
      url = .bch_api_base_url,
      headers = list(`content-type` = "application/json; charset=utf-8"),
      body = charToRaw('{"Id":609}')
    )
  }

  response <- bch_perform(
    req,
    perform = perform,
    sleep = function(seconds) delays <<- c(delays, seconds),
    jitter = function(backoff) 0
  )

  expect_s3_class(response, "httr2_response")
  expect_identical(httr2::resp_status(response), 200L)
  expect_identical(attr(response, "bch_attempts"), 2L)
  expect_identical(attempts, 2L)
  expect_equal(delays, 0)
  expect_identical(bch_response_json(response)$Id, 609L)
})

test_that("redirects are never followed and HTTP errors are sanitized", {
  secret <- "TEST-ONLY-SECRET-85531"
  req <- bch_api_request("indicadores", key = secret)
  attempts <- 0L

  perform <- function(request) {
    attempts <<- attempts + 1L
    httr2::response(
      status_code = 302L,
      url = paste0(.bch_api_base_url, "?echo=", secret),
      headers = list(
        location = paste0("https://attacker.invalid/?clave=", secret),
        `content-type` = "text/plain"
      ),
      body = charToRaw(secret)
    )
  }

  err <- expect_error(
    bch_perform(
      req,
      perform = perform,
      sleep = function(seconds) stop("must not sleep"),
      jitter = function(backoff) 0
    ),
    class = "bch_http_redirect"
  )

  rendered <- paste(capture.output(str(err)), collapse = "\n")
  expect_identical(attempts, 1L)
  expect_null(err$call)
  expect_false(grepl(secret, conditionMessage(err), fixed = TRUE))
  expect_false(grepl(secret, rendered, fixed = TRUE))
  expect_false(any(c("request", "response") %in% names(err)))
  expect_false(grepl("?", err$endpoint, fixed = TRUE))
})

test_that("transport failures discard unsafe upstream conditions", {
  secret <- "TEST-ONLY-SECRET-99420"
  req <- bch_api_request("indicadores", key = secret)

  err <- expect_error(
    bch_perform(
      req,
      max_attempts = 1L,
      perform = function(request) stop(paste("upstream leaked", secret)),
      sleep = function(seconds) NULL,
      jitter = function(backoff) 0
    ),
    class = "bch_network_error"
  )

  rendered <- paste(capture.output(str(err)), collapse = "\n")
  expect_null(err$call)
  expect_false(grepl(secret, conditionMessage(err), fixed = TRUE))
  expect_false(grepl(secret, rendered, fixed = TRUE))
  expect_false(any(c("parent", "request", "response") %in% names(err)))
})

test_that("successful non-JSON responses are rejected without body disclosure", {
  secret <- "TEST-ONLY-SECRET-64013"
  req <- bch_api_request("indicadores", key = secret)

  err <- expect_error(
    bch_perform(
      req,
      max_attempts = 1L,
      perform = function(request) {
        httr2::response(
          status_code = 200L,
          url = .bch_api_base_url,
          headers = list(`content-type` = "text/html"),
          body = charToRaw(secret)
        )
      },
      sleep = function(seconds) NULL,
      jitter = function(backoff) 0
    ),
    class = "bch_http_content_type"
  )

  expect_false(grepl(secret, conditionMessage(err), fixed = TRUE))
  expect_false(any(c("request", "response") %in% names(err)))
})

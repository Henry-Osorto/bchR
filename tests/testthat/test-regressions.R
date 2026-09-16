test_that("server Retry-After is never shortened", {
  req <- bch_api_request("indicadores", key = "test-only-retry-key")
  attempts <- 0L
  waits <- numeric()
  transport <- function(req) {
    attempts <<- attempts + 1L
    httr2::response(status_code = 429L, url = .bch_api_base_url,
      headers = list(`content-type` = "application/json", `retry-after` = "120"),
      body = charToRaw("[]"))
  }
  err <- expect_error(bch_perform(req, perform = transport,
    sleep = function(x) waits <<- c(waits, x)), class = "bch_retry_deferred")
  expect_identical(attempts, 1L)
  expect_length(waits, 0L)
  expect_equal(err$retry_after_seconds, 120)
})

test_that("only designated failures are retried", {
  expect_false(bch_classify_http(501)$retryable)
  expect_false(bch_classify_http(505)$retryable)
  req <- bch_api_request("indicadores", key = "test-only-network-key")
  count <- 0L
  transport <- function(req) {
    count <<- count + 1L
    stop(structure(list(message = "connection failed", call = NULL),
                   class = c("httr2_failure", "error", "condition")))
  }
  err <- expect_error(bch_perform(req, perform = transport,
    sleep = function(x) NULL, jitter = function(x) 0), class = "bch_network_error")
  expect_identical(count, 3L)
  expect_true(err$retryable)
})

test_that("a modified redirect policy is rejected before transport", {
  req <- bch_api_request("indicadores", key = "test-only-no-redirect")
  req <- httr2::req_options(req, followlocation = TRUE)
  expect_error(bch_perform(req, perform = function(req) stop("should not run")),
               class = "bch_unsafe_endpoint")
})

test_that("empty objects and unsafe condition fields are not accepted", {
  object <- jsonlite::fromJSON("{}", simplifyVector = FALSE)
  expect_error(.bch_parse_catalog(object), class = "bch_schema_error")
  expect_error(bch_abort("bad", request = list(headers = "secret")), "reserved")
})

test_that("large JSON integer IDs retain their exact digits", {
  response <- httr2::response(status_code = 200L, url = .bch_api_base_url,
    headers = list(`content-type` = "application/json"),
    body = charToRaw('[{"Id":9007199254740993}]'))
  expect_identical(bch_response_json(response)[[1L]]$Id, "9007199254740993")
})

test_that("timestamps cannot silently normalize invalid calendar values", {
  for (x in c("2024-03-09T24:00:00Z", "2024-03-09T12:59:60Z",
              "2023-02-29T00:00:00Z")) {
    expect_error(.bch_parse_datetime_utc(x), class = "bch_invalid_date")
  }
})

test_that("result identifiers and attempts reject invalid scalar types", {
  for (ids in list(NA_character_, "", 609L)) {
    expect_error(bch_result(metadata = tibble::tibble(indicator_id = ids)),
                 class = "bch_invalid_result")
  }
  for (attempts in c(Inf, -Inf, NaN)) {
    problems <- tibble::tibble(indicator_id = "1", endpoint = "https://example.test",
      class = "bch_network_error", message = "Request failed.", attempts = attempts)
    expect_error(bch_result(problems = problems), class = "bch_invalid_result")
  }
})

test_that("client identification and processing provenance track the package version", {
  expect_identical(.bch_client_version,
    as.character(utils::packageVersion("bchR")))
  expect_match(.bch_default_user_agent, paste0("BCH-R-client/", .bch_client_version),
               fixed = TRUE)
  spec <- .bch_resource_spec("catalog")
  item <- list(payload = list(), retrieved_at = Sys.time(), hash = strrep("a", 64L))
  result <- .bch_fetch_result(item, spec, from_cache = TRUE)
  expect_identical(result$provenance$client_version, .bch_client_version)
})

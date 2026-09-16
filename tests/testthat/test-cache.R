cache_catalog_payload <- function() {
  list(list(Id = 609L, Nombre = "IPC", Descripcion = "Inflation", Periodicidad = "Diario",
            Grupo = "PRECIOS", Correlativo = "1"))
}

cache_series_payload <- function() {
  list(list(Id = 1L, IndicadorId = 609L, Nombre = "IPC", Descripcion = "Inflation",
            Fecha = "2024-01-01T00:00:00Z", Valor = 4.5))
}

cache_response <- function(payload, req, attempts = 1L) {
  response <- httr2::response(
    status_code = 200L, url = req$url,
    headers = list(`content-type` = "application/json", authorization = "PRIVATE-SIDECAR"),
    body = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
  )
  attr(response, "bch_attempts") <- attempts
  attr(response, "private_sidecar") <- "PRIVATE-SIDECAR"
  response
}

cache_seed <- function(directory, resource = "catalog", id = NULL, age = 0) {
  spec <- .bch_resource_spec(resource, id)
  payload <- if (resource == "catalog") cache_catalog_payload() else cache_series_payload()
  raw <- charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
  .bch_cache_write(spec, directory, raw,
                   digest::digest(raw, algo = "sha256", serialize = FALSE), Sys.time() - age)
  file.path(directory, spec$filename)
}

test_that("live retrieval caches only validated public bytes and provenance", {
  directory <- withr::local_tempdir()
  secret <- "CREDENTIAL-SENTINEL-SECRET-66373"
  withr::local_envvar(BCH_API_KEY = secret)
  testthat::local_mocked_bindings(
    bch_perform = function(req) cache_response(cache_catalog_payload(), req, 2L)
  )
  result <- .bch_fetch_resource("catalog", cache_dir = directory)
  expect_equal(result$payload, cache_catalog_payload())
  expect_identical(result$attempts, 2L)
  expect_false(result$provenance$from_cache)
  expect_identical(result$provenance$source, "Fuente: Banco Central de Honduras")
  expect_identical(attr(result$provenance$retrieved_at, "tzone"), "UTC")
  expect_false(grepl("?", result$provenance$endpoint, fixed = TRUE))
  file <- file.path(directory, .bch_resource_spec("catalog")$filename)
  text <- paste(readLines(file, warn = FALSE), collapse = "")
  expect_false(grepl(secret, text, fixed = TRUE))
  expect_false(grepl("PRIVATE-SIDECAR", text, fixed = TRUE))
  envelope <- jsonlite::read_json(file)
  expect_setequal(names(envelope), .bch_cache_fields)
  expect_identical(envelope$response_hash, result$provenance$response_hash)
})

test_that("fresh cache and cache-only mode never need credentials or network", {
  directory <- withr::local_tempdir()
  withr::local_envvar(BCH_API_KEY = NA)
  testthat::local_mocked_bindings(bch_perform = function(req) stop("network invoked"))
  cache_seed(directory)
  result <- .bch_fetch_resource("catalog", cache_dir = directory)
  expect_true(result$provenance$from_cache)
  expect_false(result$provenance$stale)
  expect_identical(result$attempts, 0L)
  cache_seed(directory, age = 90000)
  result <- .bch_fetch_resource("catalog", source = "cache", cache_dir = directory)
  expect_true(result$provenance$stale)
  expect_error(.bch_fetch_resource("series", id = 609, source = "cache", cache_dir = directory),
               class = "bch_cache_miss")
  expect_error(.bch_fetch_resource("catalog", source = "cache", cache_dir = FALSE),
               class = "bch_cache_miss")
})

test_that("auto stale fallback permits missing keys and transient failures only", {
  directory <- withr::local_tempdir()
  cache_seed(directory, age = 90000)
  withr::local_envvar(BCH_API_KEY = NA)
  expect_warning(result <- .bch_fetch_resource("catalog", cache_dir = directory),
                   class = "bch_cache_stale")
  expect_true(result$provenance$stale)
  withr::local_envvar(BCH_API_KEY = "fixture-key")
  for (class in c("bch_network_error", "bch_http_timeout", "bch_http_rate_limit",
                  "bch_http_server", "bch_retry_deferred")) {
    local({
      failure <- class
      testthat::local_mocked_bindings(bch_perform = function(req) {
        bch_abort("Temporary failure.", failure, attempts = 3L, call = NULL)
      })
      expect_warning(result <- .bch_fetch_resource("catalog", cache_dir = directory),
                       class = "bch_cache_stale")
      expect_true(result$provenance$from_cache)
      expect_identical(result$attempts, 3L)
    })
  }
  testthat::local_mocked_bindings(bch_perform = function(req) {
    bch_abort("Credential rejected.", "bch_http_auth", status = 401L, call = NULL)
  })
  expect_error(.bch_fetch_resource("catalog", cache_dir = directory), class = "bch_http_auth")
  for (class in c("bch_network_error", "bch_http_server")) {
    local({
      failure <- class
      testthat::local_mocked_bindings(bch_perform = function(req) {
        bch_abort("Non-retryable failure.", failure, retryable = FALSE, call = NULL)
      })
      expect_error(.bch_fetch_resource("catalog", cache_dir = directory), class = failure)
    })
  }
})

test_that("refresh and live source bypass cache and forbid stale fallback", {
  directory <- withr::local_tempdir()
  cache_seed(directory, age = 90000)
  withr::local_envvar(BCH_API_KEY = NA)
  expect_error(.bch_fetch_resource("catalog", refresh = TRUE, cache_dir = directory),
               class = "bch_auth_missing")
  expect_error(.bch_fetch_resource("catalog", source = "live", cache_dir = directory),
               class = "bch_auth_missing")
  expect_error(.bch_fetch_resource("catalog", refresh = TRUE, source = "cache", cache_dir = directory),
               class = "bch_invalid_argument")
  withr::local_envvar(BCH_API_KEY = "fixture-key")
  calls <- 0L
  testthat::local_mocked_bindings(bch_perform = function(req) {
    calls <<- calls + 1L
    cache_response(cache_catalog_payload(), req)
  })
  result <- .bch_fetch_resource("catalog", refresh = TRUE, cache_dir = directory)
  expect_false(result$provenance$from_cache)
  expect_identical(calls, 1L)
})

test_that("corrupt and incompatible cache envelopes are ignored", {
  directory <- withr::local_tempdir()
  withr::local_envvar(BCH_API_KEY = NA)
  file <- cache_seed(directory)
  original <- jsonlite::read_json(file)
  variants <- list(
    list(response_hash = paste(rep("0", 64L), collapse = "")),
    list(schema_version = "old"),
    list(endpoint = "https://attacker.invalid/"),
    list(resource = "series"),
    list(indicator_id = "609"),
    list(request = "arbitrary-sidecar"),
    list(body_base64 = "invalid-base64%"),
    list(retrieved_at = "2999-01-01T00:00:00Z")
  )
  for (variant in variants) {
    entry <- original
    for (name in names(variant)) entry[[name]] <- variant[[name]]
    jsonlite::write_json(entry, file, auto_unbox = TRUE, null = "null")
    expect_warning(expect_error(.bch_fetch_resource("catalog", source = "cache", cache_dir = directory),
                                  class = "bch_cache_miss"), class = "bch_cache_invalid")
  }
  writeLines("{malformed-json", file)
  expect_warning(expect_error(.bch_fetch_resource("catalog", source = "cache", cache_dir = directory),
                                class = "bch_cache_miss"), class = "bch_cache_invalid")
})

test_that("invalid schemas and response credential echoes never enter cache", {
  directory <- withr::local_tempdir()
  secret <- "CREDENTIAL-SENTINEL-SECRET-32176"
  withr::local_envvar(BCH_API_KEY = secret)
  testthat::local_mocked_bindings(bch_perform = function(req) {
    cache_response(list(list(Id = 1L)), req)
  })
  expect_error(.bch_fetch_resource("catalog", cache_dir = directory), class = "bch_schema_error")
  expect_length(list.files(directory), 0L)
  testthat::local_mocked_bindings(bch_perform = function(req) {
    payload <- cache_catalog_payload()
    payload[[1L]]$Descripcion <- secret
    cache_response(payload, req)
  })
  err <- expect_error(.bch_fetch_resource("catalog", cache_dir = directory), class = "bch_response_secret")
  expect_false(grepl(secret, paste(capture.output(str(err)), collapse = ""), fixed = TRUE))
  expect_length(list.files(directory), 0L)
})

test_that("cache can be disabled or configured and series retain raw identity", {
  directory <- withr::local_tempdir()
  withr::local_options(bch.cache_dir = directory)
  withr::local_envvar(BCH_API_KEY = "fixture-key")
  testthat::local_mocked_bindings(bch_perform = function(req) cache_response(cache_series_payload(), req))
  result <- .bch_fetch_resource("series", id = 609L, cache_dir = FALSE)
  expect_identical(result$provenance$indicator_id, "609")
  expect_length(list.files(directory), 0L)
  .bch_fetch_resource("series", id = 609L)
  expect_equal(nrow(bch_cache_info()), 1L)
  expect_equal(.bch_fetch_resource("series", id = 609L, source = "cache")$payload,
                cache_series_payload())
})

test_that("cache clearing previews exact valid files and leaves unrelated files intact", {
  directory <- withr::local_tempdir()
  catalog <- cache_seed(directory, age = 200)
  series <- cache_seed(directory, "series", 609L, age = 400)
  unrelated <- file.path(directory, "user-analysis.json")
  writeLines("{}", unrelated)
  nested <- file.path(directory, "nested")
  dir.create(nested)
  nested_cache <- cache_seed(nested)
  preview <- bch_clear_cache(cache_dir = directory)
  expect_equal(nrow(preview), 2L)
  expect_false(any(preview$removed))
  expect_true(all(file.exists(c(catalog, series))))
  cleared <- bch_clear_cache(type = "series", older_than = 300, cache_dir = directory, dry_run = FALSE)
  expect_equal(nrow(cleared), 1L)
  expect_true(cleared$removed)
  expect_true(file.exists(catalog))
  expect_false(file.exists(series))
  expect_true(file.exists(unrelated))
  expect_true(file.exists(nested_cache))
  invalid <- file.path(directory, "bch-cache-prototype-1-series-710.json")
  writeLines("{}", invalid)
  expect_warning(bch_clear_cache(cache_dir = directory, dry_run = FALSE), class = "bch_cache_invalid")
  expect_true(file.exists(invalid))
})

test_that("cache timestamps preserve fractional seconds and reject invalid dates", {
  directory <- withr::local_tempdir()
  withr::local_envvar(BCH_API_KEY = NA)
  instant <- as.POSIXct("2024-01-01 23:59:59", tz = "UTC") + 0.987654
  spec <- .bch_resource_spec("catalog")
  body <- charToRaw(jsonlite::toJSON(cache_catalog_payload(), auto_unbox = TRUE))
  expect_true(.bch_cache_write(spec, directory, body,
                               digest::digest(body, "sha256", serialize = FALSE), instant))
  result <- .bch_fetch_resource("catalog", source = "cache", cache_dir = directory)
  expect_lt(abs(as.numeric(result$provenance$retrieved_at) - as.numeric(instant)),
             0.000002)
  file <- file.path(directory, spec$filename)
  entry <- jsonlite::read_json(file)
  entry$retrieved_at <- "2024-02-30T12:00:00.123456Z"
  jsonlite::write_json(entry, file, auto_unbox = TRUE, null = "null")
  expect_warning(expect_error(.bch_fetch_resource("catalog", source = "cache", cache_dir = directory),
                               class = "bch_cache_miss"), class = "bch_cache_invalid")
})

test_that("large integer identifiers remain exact after a cache round trip", {
  directory <- withr::local_tempdir()
  withr::local_envvar(BCH_API_KEY = "fixture-key")
  raw_json <- paste0('[{"Id":9007199254740993,"Nombre":"BIG",',
                      '"Descripcion":"Large id","Periodicidad":"Diario",',
                      '"Grupo":"TEST","Correlativo":"01"}]')
  testthat::local_mocked_bindings(bch_perform = function(req) {
    httr2::response(status_code = 200L, url = req$url,
                    headers = list(`content-type` = "application/json"),
                    body = charToRaw(raw_json))
  })
  live <- .bch_fetch_resource("catalog", source = "live", cache_dir = directory)
  cached <- .bch_fetch_resource("catalog", source = "cache", cache_dir = directory)
  expect_identical(live$payload[[1L]]$Id, "9007199254740993")
  expect_identical(cached$payload, live$payload)
  expect_identical(cached$provenance$response_hash, live$provenance$response_hash)
})

test_that("session-only request credentials cannot be echoed or cached", {
  directory <- withr::local_tempdir()
  withr::local_envvar(BCH_API_KEY = NA)
  secret <- "SESSION-CREDENTIAL-SENTINEL-73245"
  testthat::local_mocked_bindings(
    bch_api_key = function(key = NULL) secret,
    bch_perform = function(req) {
      payload <- cache_catalog_payload()
      payload[[1L]]$Descripcion <- secret
      cache_response(payload, req)
    }
  )
  err <- expect_error(.bch_fetch_resource("catalog", source = "live", cache_dir = directory),
                        class = "bch_response_secret")
  expect_false(grepl(secret, paste(capture.output(str(err)), collapse = ""), fixed = TRUE))
  expect_length(list.files(directory), 0L)
})

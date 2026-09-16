# Public-response cache -----------------------------------------------------

.bch_cache_schema <- "prototype-1"
.bch_cache_source <- "Fuente: Banco Central de Honduras"
.bch_cache_fields <- c(
  "schema_version", "resource", "indicator_id", "endpoint", "retrieved_at",
  "source", "response_hash", "body_base64"
)
.bch_cache_pattern <- "^bch-cache-prototype-1-(catalog|series-[0-9]+)\\.json$"

.bch_cache_location <- function(cache_dir = NULL, create = FALSE) {
  if (is.null(cache_dir)) {
    cache_dir <- getOption(
      "bch.cache_dir", tools::R_user_dir("bchR", "cache")
    )
  }
  if (identical(cache_dir, FALSE)) return(NULL)
  if (!bch_is_scalar_string(cache_dir) || !nzchar(cache_dir)) {
    bch_abort("`cache_dir` must be a directory path, NULL, or FALSE.",
              "bch_invalid_argument", call = NULL)
  }
  if (file.exists(cache_dir) && !dir.exists(cache_dir)) {
    bch_abort("`cache_dir` points to a file, not a directory.",
              "bch_invalid_argument", call = NULL)
  }
  if (create && !dir.exists(cache_dir)) {
    if (!dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)) {
      bch_abort("The BCH cache directory could not be created.",
                "bch_cache_write_error", call = NULL)
    }
  }
  normalizePath(path.expand(cache_dir), winslash = "/", mustWork = FALSE)
}

.bch_resource_spec <- function(resource, id = NULL) {
  resource <- match.arg(resource, c("catalog", "series"))
  if (resource == "catalog") {
    if (!is.null(id)) {
      bch_abort("`id` must be NULL when fetching the catalog.",
                "bch_invalid_argument", call = NULL)
    }
    id <- NA_character_
    path <- "indicadores"
    stem <- "catalog"
  } else {
    id <- .bch_id_scalar(id, "id", "request", 1L)
    if (nchar(id) > 100L) {
      bch_abort("`id` is too long for a BCH resource identifier.",
                "bch_invalid_argument", call = NULL)
    }
    path <- paste0("indicadores/", id, "/cifras")
    stem <- paste0("series-", id)
  }
  list(resource = resource, id = id, path = path,
       endpoint = paste0(.bch_api_base_url, "/", path),
       filename = paste0("bch-cache-prototype-1-", stem, ".json"))
}

.bch_cache_seconds <- function(value, name, default) {
  if (is.null(value)) return(default)
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0) {
    bch_abort(paste0(name, " must be one finite non-negative number of seconds."),
              "bch_invalid_argument", call = NULL)
  }
  as.double(value)
}

.bch_cache_time <- function(x = Sys.time()) {
  format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC", usetz = FALSE)
}

.bch_validate_public_payload <- function(payload, spec) {
  if (spec$resource == "catalog") {
    suppressWarnings(.bch_parse_catalog(payload, unknown_frequency = "keep"))
  } else {
    suppressWarnings(.bch_parse_series(
      payload, expected_id = spec$id, unknown_frequency = "keep"
    ))
  }
  invisible(payload)
}

.bch_check_payload_secret <- function(payload, body, key = NULL) {
  # Check the actual request credential as well as the environment. Internal
  # callers can construct session-only requests without setting BCH_API_KEY.
  keys <- unique(c(key, Sys.getenv(.bch_api_key_env, unset = "")))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (!length(keys)) return(invisible(NULL))
  values <- unlist(payload, recursive = TRUE, use.names = TRUE)
  values <- c(as.character(values), names(values), rawToChar(body))
  values <- values[!is.na(values)]
  contains_secret <- any(vapply(keys, function(secret) {
    any(grepl(secret, values, fixed = TRUE))
  }, logical(1)))
  if (contains_secret) {
    bch_abort("The BCH response contains a credential and was not retained.",
              "bch_response_secret", call = NULL)
  }
  invisible(NULL)
}

.bch_cache_owned_path <- function(path, directory) {
  if (!file.exists(path) || dir.exists(path)) return(FALSE)
  if (!grepl(.bch_cache_pattern, basename(path))) return(FALSE)
  link <- Sys.readlink(path)
  if (!is.na(link) && nzchar(link)) return(FALSE)
  target <- normalizePath(path, winslash = "/", mustWork = TRUE)
  parent <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  identical(dirname(target), parent)
}

.bch_cache_read <- function(spec, directory, warn = TRUE) {
  if (is.null(directory) || !dir.exists(directory)) return(NULL)
  path <- file.path(directory, spec$filename)
  if (!file.exists(path)) return(NULL)
  item <- tryCatch({
    if (!.bch_cache_owned_path(path, directory)) stop("unsafe cache path")
    size <- file.info(path)$size
    if (is.na(size) || size > 100 * 1024^2) stop("oversized cache")
    entry <- jsonlite::read_json(path, simplifyVector = FALSE)
    if (!is.list(entry) || !identical(sort(names(entry)), sort(.bch_cache_fields))) {
      stop("invalid cache envelope")
    }
    strings <- setdiff(.bch_cache_fields, "indicator_id")
    if (!all(vapply(entry[strings], bch_is_scalar_string, logical(1)))) {
      stop("invalid cache fields")
    }
    expected_id <- if (is.na(spec$id)) NULL else spec$id
    if (!identical(entry$schema_version, .bch_cache_schema) ||
        !identical(entry$resource, spec$resource) ||
        !identical(entry$indicator_id, expected_id) ||
        !identical(entry$endpoint, spec$endpoint) ||
        !identical(entry$source, .bch_cache_source) ||
        !grepl("^[a-f0-9]{64}$", entry$response_hash)) {
      stop("invalid cache identity")
    }
    time <- .bch_parse_datetime_utc(entry$retrieved_at)
    if (as.numeric(difftime(time, Sys.time(), units = "secs")) > 300) {
      stop("cache time in future")
    }
    if (!grepl("^[A-Za-z0-9+/]*={0,2}$", entry$body_base64) ||
        nchar(entry$body_base64) %% 4L != 0L) stop("invalid body encoding")
    body <- jsonlite::base64_dec(entry$body_base64)
    hash <- digest::digest(body, algo = "sha256", serialize = FALSE)
    if (!identical(hash, entry$response_hash)) stop("cache hash mismatch")
    payload <- jsonlite::fromJSON(rawToChar(body), simplifyVector = FALSE,
                                  bigint_as_char = TRUE)
    .bch_check_payload_secret(payload, body)
    .bch_validate_public_payload(payload, spec)
    list(payload = payload, body = body, hash = hash, retrieved_at = time,
         path = path, bytes = size)
  }, error = function(cnd) NULL)
  if (is.null(item) && warn) {
    bch_warn("An invalid BCH cache file was ignored.",
             "bch_cache_invalid", resource = spec$resource, call = NULL)
  }
  item
}

.bch_cache_write <- function(spec, directory, body, hash, retrieved_at) {
  if (is.null(directory)) return(invisible(FALSE))
  success <- tryCatch({
    directory <- .bch_cache_location(directory, create = TRUE)
    path <- file.path(directory, spec$filename)
    if (file.exists(path) && !.bch_cache_owned_path(path, directory)) {
      stop("unsafe cache destination")
    }
    entry <- list(
      schema_version = .bch_cache_schema,
      resource = spec$resource,
      indicator_id = if (is.na(spec$id)) NULL else spec$id,
      endpoint = spec$endpoint,
      retrieved_at = .bch_cache_time(retrieved_at),
      source = .bch_cache_source,
      response_hash = hash,
      body_base64 = gsub("[[:space:]]", "", jsonlite::base64_enc(body))
    )
    temporary <- tempfile("bch-write-", tmpdir = directory, fileext = ".tmp")
    on.exit(unlink(temporary), add = TRUE)
    jsonlite::write_json(entry, temporary, auto_unbox = TRUE, null = "null")
    # Rename a completed file in the same directory; never serialize a request,
    # response, error, arbitrary sidecar, or caller-supplied R object.
    if (!file.rename(temporary, path)) stop("cache rename failed")
    TRUE
  }, error = function(cnd) FALSE)
  if (!success) {
    bch_warn("The public BCH response was returned, but its cache could not be saved.",
             "bch_cache_write_error", resource = spec$resource, call = NULL)
  }
  invisible(success)
}

.bch_fetch_result <- function(item, spec, from_cache, attempts = 0L,
                              stale = FALSE) {
  time <- as.POSIXct(item$retrieved_at, tz = "UTC")
  attr(time, "tzone") <- "UTC"
  provenance <- tibble::tibble(
    request_id = paste(spec$resource, if (is.na(spec$id)) "all" else spec$id,
                       .bch_cache_time(time), substr(item$hash, 1L, 12L), sep = ":"),
    indicator_id = spec$id,
    retrieved_at = time,
    source = .bch_cache_source,
    endpoint = spec$endpoint,
    schema_version = .bch_cache_schema,
    from_cache = from_cache,
    client_version = .bch_client_version,
    response_hash = item$hash,
    stale = stale,
    age_seconds = max(0, as.numeric(difftime(Sys.time(), time, units = "secs")))
  )
  list(payload = item$payload, provenance = provenance,
       attempts = as.integer(attempts))
}

#' Fetch a validated BCH JSON resource
#'
#' The cache stores a versioned JSON envelope containing only the public raw
#' response body and a small, fixed set of provenance fields. Cache hits never
#' require credentials. `auto` uses a fresh cache, otherwise requests the API;
#' stale fallback is limited to a missing key or a transient network/server
#' failure. Authentication rejection never falls back. `refresh = TRUE`
#' requires a live response and disables fallback.
#'
#' @param resource Either `"catalog"` or `"series"`.
#' @param id One series identifier; NULL for the catalog.
#' @param source Either `"auto"`, `"live"`, or `"cache"`.
#' @param refresh Force live retrieval without fallback.
#' @param max_age Cache freshness in seconds. Defaults to 86400 for the catalog
#'   and 21600 for a series. `source = "cache"` permits stale entries and marks
#'   them explicitly.
#' @param cache_dir Cache directory, NULL for the `bch.cache_dir` option or
#'   `tools::R_user_dir("bchR", "cache")`, or FALSE to disable caching.
#' @return A list containing parsed `payload`, `provenance`, and `attempts`.
#' @keywords internal
.bch_fetch_resource <- function(resource = c("catalog", "series"), id = NULL,
                                source = "auto", refresh = FALSE,
                                max_age = NULL, cache_dir = NULL) {
  resource <- match.arg(resource)
  source <- match.arg(source, c("auto", "live", "cache"))
  spec <- .bch_resource_spec(resource, id)
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    bch_abort("`refresh` must be TRUE or FALSE.", "bch_invalid_argument", call = NULL)
  }
  if (refresh && source == "cache") {
    bch_abort("`refresh = TRUE` cannot be combined with `source = 'cache'`.",
              "bch_invalid_argument", call = NULL)
  }
  max_age <- .bch_cache_seconds(
    max_age, "`max_age`", if (resource == "catalog") 86400 else 21600
  )
  directory <- .bch_cache_location(cache_dir)
  cached <- if (!refresh && source != "live") .bch_cache_read(spec, directory) else NULL
  age <- if (is.null(cached)) Inf else
    max(0, as.numeric(difftime(Sys.time(), cached$retrieved_at, units = "secs")))
  if (!is.null(cached) && (source == "cache" || age <= max_age)) {
    return(.bch_fetch_result(cached, spec, from_cache = TRUE, stale = age > max_age))
  }
  if (source == "cache") {
    bch_abort("No valid BCH cache entry is available for this resource.",
              "bch_cache_miss", resource = resource, endpoint = spec$endpoint,
              call = NULL)
  }
  live <- tryCatch({
    # Resolve once so that a session-only authentication provider and the
    # response-echo guard use the same key. Redacted httr2 headers are opaque.
    request_key <- bch_api_key()
    req <- bch_api_request(spec$path, key = request_key)
    response <- bch_perform(req)
    payload <- bch_response_json(response)
    body <- httr2::resp_body_raw(response)
    .bch_check_payload_secret(payload, body, key = request_key)
    .bch_validate_public_payload(payload, spec)
    item <- list(payload = payload, body = body,
                 hash = digest::digest(body, algo = "sha256", serialize = FALSE),
                 retrieved_at = as.POSIXct(Sys.time(), tz = "UTC"))
    attempts <- attr(response, "bch_attempts", exact = TRUE)
    if (is.null(attempts)) attempts <- 1L
    list(item = item, attempts = attempts, error = NULL)
  }, error = function(cnd) list(error = cnd))
  if (!is.null(live$error)) {
    transient <- inherits(live$error, "bch_auth_missing") ||
      (inherits(live$error, c(
        "bch_network_error", "bch_http_timeout", "bch_http_rate_limit",
        "bch_http_server", "bch_retry_deferred"
      )) && !identical(live$error$retryable, FALSE))
    if (source == "auto" && !refresh && !is.null(cached) && transient) {
      bch_warn("The BCH API is unavailable; a stale cache entry was returned.",
               "bch_cache_stale", resource = resource, call = NULL)
      attempts <- live$error$attempts
      if (is.null(attempts)) attempts <- 0L
      return(.bch_fetch_result(cached, spec, from_cache = TRUE,
                               attempts = attempts, stale = TRUE))
    }
    stop(live$error)
  }
  .bch_cache_write(spec, directory, live$item$body, live$item$hash,
                   live$item$retrieved_at)
  .bch_fetch_result(live$item, spec, from_cache = FALSE, attempts = live$attempts)
}

.bch_empty_cache_info <- function() {
  tibble::tibble(path = character(), resource = character(),
                 indicator_id = character(),
                 retrieved_at = as.POSIXct(character(), tz = "UTC"),
                 age_seconds = double(), response_hash = character(), bytes = double())
}

#' Inspect valid BCH cache entries without contacting the API
#'
#' Only this client's exact, versioned filenames are inspected. Corrupt or
#' incompatible files are ignored with a warning. Neither function traverses
#' subdirectories, and cache clearing never removes arbitrary files.
#'
#' @param cache_dir Cache directory, NULL for the configured default, or FALSE.
#' @return A tibble containing valid cache files and retrieval provenance.
#' @export
bch_cache_info <- function(cache_dir = NULL) {
  directory <- .bch_cache_location(cache_dir)
  if (is.null(directory) || !dir.exists(directory)) return(.bch_empty_cache_info())
  paths <- list.files(directory, pattern = .bch_cache_pattern, full.names = TRUE,
                       recursive = FALSE, all.files = FALSE)
  rows <- lapply(paths, function(path) {
    filename <- basename(path)
    catalog <- identical(filename, "bch-cache-prototype-1-catalog.json")
    id <- if (catalog) NULL else sub(
      "^bch-cache-prototype-1-series-([0-9]+)\\.json$", "\\1", filename
    )
    spec <- .bch_resource_spec(if (catalog) "catalog" else "series", id)
    item <- .bch_cache_read(spec, directory)
    if (is.null(item)) return(NULL)
    tibble::tibble(path = path, resource = spec$resource, indicator_id = spec$id,
                   retrieved_at = item$retrieved_at,
                   age_seconds = max(0, as.numeric(difftime(
                     Sys.time(), item$retrieved_at, units = "secs"
                   ))), response_hash = item$hash, bytes = as.double(item$bytes))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) .bch_empty_cache_info() else do.call(rbind, rows)
}

#' Clear selected valid BCH cache files
#'
#' The default is a dry run. Set `dry_run = FALSE` to delete precisely the valid
#' files listed in the returned table; unrelated files, invalid cache files,
#' symbolic links and subdirectories are retained.
#'
#' @param type Either `"all"`, `"catalog"`, or `"series"`.
#' @param older_than Minimum age in seconds; NULL selects all ages.
#' @param cache_dir Cache directory, NULL for the configured default, or FALSE.
#' @param dry_run If TRUE, only report the selected files.
#' @return A tibble of selected entries with a logical `removed` column.
#' @export
bch_clear_cache <- function(type = c("all", "catalog", "series"),
                            older_than = NULL, cache_dir = NULL, dry_run = TRUE) {
  type <- match.arg(type)
  older_than <- .bch_cache_seconds(older_than, "`older_than`", 0)
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    bch_abort("`dry_run` must be TRUE or FALSE.", "bch_invalid_argument", call = NULL)
  }
  directory <- .bch_cache_location(cache_dir)
  info <- bch_cache_info(cache_dir)
  keep <- info$age_seconds >= older_than
  if (type != "all") keep <- keep & info$resource == type
  info <- info[keep, , drop = FALSE]
  info$removed <- rep(FALSE, nrow(info))
  if (!dry_run) {
    for (i in seq_len(nrow(info))) {
      # Recheck the resolved, exact target immediately before each deletion.
      if (.bch_cache_owned_path(info$path[[i]], directory)) {
        info$removed[[i]] <- file.remove(info$path[[i]])
      }
    }
  }
  info
}

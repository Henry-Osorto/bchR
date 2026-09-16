# Synthetic records only; the scoped fixture replaces the fetch boundary.
# Each fixture clones the public closures into an isolated environment, so it
# also works when R files are sourced without installing a package.
.public_fixture <- function(catalog = NULL, series = NULL, failures = list()) {
  if (is.null(catalog)) {
    catalog <- list(
      list(Id = "001", Nombre = "SYN-A", Descripcion = "Inflaci\u00f3n sint\u00e9tica [A]",
           Periodicidad = "Diario", Grupo = "SYN", Correlativo = "01"),
      list(Id = "2", Nombre = "SYN-B", Descripcion = "Producci\u00f3n sint\u00e9tica",
           Periodicidad = "Diario", Grupo = "SYN", Correlativo = "02"),
      list(Id = "3", Nombre = "SYN-C", Descripcion = "Otra serie sint\u00e9tica",
           Periodicidad = "Unverified-fixture-frequency", Grupo = "OTHER", Correlativo = "01")
    )
  }
  observation <- function(id, row, day, value) {
    metadata <- Filter(function(x) identical(x$Id, id), catalog)[[1L]]
    list(Id = as.character(row), IndicadorId = id, Nombre = metadata$Nombre,
         Descripcion = metadata$Descripcion, Fecha = paste0(day, "T23:30:00Z"), Valor = value)
  }
  if (is.null(series)) {
    series <- list(
      "001" = list(observation("001", 1, "2024-01-01", 10),
                   observation("001", 2, "2024-01-02", 11)),
      "2" = list(observation("2", 3, "2024-01-02", 20),
                 observation("2", 4, "2024-01-03", NULL)),
      "3" = list(observation("3", 5, "2024-01-02", 30))
    )
  }
  api <- new.env(parent = environment(bch_get_data))
  api$calls <- list()
  api$catalog <- .bch_parse_catalog(catalog)
  for (name in c("bch_get_data", "bch_indicators", "bch_search_indicators")) {
    fn <- get(name, envir = api, inherits = TRUE)
    environment(fn) <- api
    assign(name, fn, envir = api)
  }
  api$.bch_fetch_resource <- function(resource, id = NULL, source = "auto",
                                     refresh = FALSE, max_age = NULL, cache_dir = NULL) {
    api$calls[[length(api$calls) + 1L]] <- list(
      resource = resource, id = id, source = source, refresh = refresh,
      max_age = max_age, cache_dir = cache_dir
    )
    if (!is.null(id) && id %in% names(failures)) stop(failures[[id]])
    endpoint <- paste0(.bch_api_base_url, "/indicadores",
                       if (resource == "series") paste0("/", id, "/cifras") else "")
    cached <- source == "cache"
    list(
      payload = if (resource == "catalog") catalog else series[[id]],
      attempts = 1L,
      provenance = tibble::tibble(
        request_id = paste0("synthetic-", resource, if (is.null(id)) "" else id),
        indicator_id = if (is.null(id)) NA_character_ else id,
        retrieved_at = as.POSIXct("2024-01-04", tz = "UTC"),
        source = "synthetic fixture", endpoint = endpoint,
        schema_version = "fixture-v1", from_cache = cached,
        response_hash = NA_character_
      )
    )
  }
  api
}

testthat::test_that("catalog filters intersect and preserve IDs and provenance", {
  api <- .public_fixture()
  result <- api$bch_indicators(id = "001", group = "SYN", frequency = "daily",
                               source = "cache", max_age = 60, cache_dir = "fixture-cache")
  testthat::expect_s3_class(result, "bch_result")
  testthat::expect_identical(result$data$indicator_id, "001")
  testthat::expect_identical(result$data, result$metadata)
  testthat::expect_true(result$provenance$from_cache)
  testthat::expect_identical(api$calls[[1L]]$max_age, 60)
  testthat::expect_identical(api$calls[[1L]]$cache_dir, "fixture-cache")
  testthat::expect_identical(nrow(result$problems), 0L)
  testthat::expect_identical(nrow(api$bch_indicators(id = "001", group = "OTHER")$data), 0L)
  testthat::expect_error(api$bch_indicators(id = "999"), class = "bch_unknown_indicator")
  testthat::expect_error(api$bch_indicators(group = "NONEXISTENT"), class = "bch_unknown_indicator")
  testthat::expect_error(api$bch_indicators(frequency = "made-up"), class = "bch_unknown_indicator")
})

testthat::test_that("search is local, literal, accent-aware, and combines terms explicitly", {
  api <- .public_fixture()
  input <- api$bch_indicators()
  before <- length(api$calls)
  result <- api$bch_search_indicators(input, c("inflacion", "[A]"))
  testthat::expect_identical(result$data$indicator_id, "001")
  testthat::expect_identical(result$provenance, input$provenance)
  testthat::expect_identical(api$bch_search_indicators(input, c("inflacion", "produccion"),
                                                     match = "any")$data$indicator_id, c("001", "2"))
  testthat::expect_identical(nrow(api$bch_search_indicators(input, c("inflacion", "produccion"))$data), 0L)
  testthat::expect_identical(nrow(api$bch_search_indicators(input, "inflacion", ignore_accents = FALSE)$data), 0L)
  testthat::expect_identical(nrow(api$bch_search_indicators(input, "inflaci\u00f3n", ignore_case = FALSE)$data), 0L)
  testthat::expect_identical(api$bch_search_indicators(api$catalog, "SYN-B", fields = "indicator_code")$data$indicator_id, "2")
  testthat::expect_identical(nrow(api$bch_search_indicators(input, ".*")$data), 0L)
  testthat::expect_identical(length(api$calls), before)
  testthat::expect_error(api$bch_search_indicators(input, character()), class = "bch_invalid_argument")
  testthat::expect_error(api$bch_search_indicators(input, "x", fields = "extra"), class = "bch_invalid_argument")
  testthat::expect_error(api$bch_search_indicators(input, "x", fields = "no-column"), class = "bch_invalid_argument")
})

testthat::test_that("supplied catalog prevents retrieval and series preserve requested order", {
  api <- .public_fixture()
  result <- api$bch_get_data(id = c(2, 2), catalog = api$catalog, progress = FALSE)
  testthat::expect_length(api$calls, 1L)
  testthat::expect_identical(api$calls[[1L]]$resource, "series")
  testthat::expect_identical(result$metadata$indicator_id, "2")
  testthat::expect_identical(result$data$value, c(20, NA_real_))
  testthat::expect_false(any(result$data$is_transformed))
  result <- api$bch_get_data(id = c("2", "001"), catalog = api$catalog, progress = FALSE)
  testthat::expect_identical(result$metadata$indicator_id, c("2", "001"))
  testthat::expect_identical(unique(result$data$indicator_id), c("2", "001"))
  result <- api$bch_get_data(group = "SYN", catalog = api$catalog, source = "cache",
                             refresh = FALSE, max_age = 123, progress = FALSE)
  testthat::expect_identical(result$metadata$indicator_id, c("001", "2"))
  testthat::expect_true(all(result$provenance$from_cache))
  testthat::expect_identical(tail(api$calls, 1)[[1]]$max_age, 123)
})

testthat::test_that("IDs, guards, and dates fail before resources are retrieved", {
  for (id in list(" 2", "2 ", "2/3", "2?clave=x", "2e2", -1, 1.5, NA, Inf,
                  2^53, character(), factor("2"))) {
    api <- .public_fixture()
    testthat::expect_error(api$bch_get_data(id = id, catalog = api$catalog), class = "bch_invalid_argument")
    testthat::expect_length(api$calls, 0L)
  }
  api <- .public_fixture()
  testthat::expect_error(api$bch_get_data(), class = "bch_invalid_argument")
  testthat::expect_error(api$bch_get_data(id = "2", group = "SYN"), class = "bch_invalid_argument")
  testthat::expect_error(api$bch_get_data(id = c("2", "001"), max_series = 1), class = "bch_series_limit")
  testthat::expect_error(api$bch_get_data(group = "SYN", catalog = api$catalog, max_series = 1), class = "bch_series_limit")
  for (date in list("2024-02-30", "2024-1-01", "2024-01-01T00:00:00Z", 1, NA, c("2024-01-01", "2024-01-02"))) {
    testthat::expect_error(api$bch_get_data(id = "2", start = date), class = "bch_invalid_argument")
  }
  testthat::expect_error(api$bch_get_data(id = "2", start = "2024-01-02", end = "2024-01-01"), class = "bch_invalid_argument")
  testthat::expect_length(api$calls, 0L)
})

testthat::test_that("date limits include UTC boundary observations and preserve empty schema", {
  api <- .public_fixture()
  result <- api$bch_get_data(group = "SYN", catalog = api$catalog,
                             start = as.Date("2024-01-02"), end = "2024-01-02", progress = FALSE)
  testthat::expect_identical(result$data$value, c(11, 20))
  testthat::expect_identical(unique(result$data$reference_date), as.Date("2024-01-02"))
  testthat::expect_identical(attr(result$data$reference_datetime, "tzone"), "UTC")
  empty <- api$bch_get_data(id = "2", catalog = api$catalog,
                            start = "2030-01-01", progress = FALSE)
  testthat::expect_identical(nrow(empty$data), 0L)
  testthat::expect_identical(names(empty$data), names(.bch_series_empty()))
  testthat::expect_identical(empty$metadata$indicator_id, "2")
  testthat::expect_identical(nrow(empty$provenance), 1L)
})

testthat::test_that("wide output aligns periods without aggregation and records column meaning", {
  api <- .public_fixture()
  result <- api$bch_get_data(group = "SYN", catalog = api$catalog, shape = "wide", progress = FALSE)
  testthat::expect_identical(result$data$period_start, as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")))
  testthat::expect_identical(result$data$id_001, c(10, 11, NA_real_))
  testthat::expect_identical(result$data$id_2, c(NA_real_, 20, NA_real_))
  testthat::expect_identical(result$column_map$output_column, c("id_001", "id_2"))
  testthat::expect_identical(result$column_map$indicator_id, c("001", "2"))
  testthat::expect_identical(names(result$data), c("period_start", "period_end", "period_label", "frequency", "id_001", "id_2"))
  empty <- api$bch_get_data(id = "2", catalog = api$catalog, shape = "wide",
                            start = "2030-01-01", progress = FALSE)
  testthat::expect_identical(nrow(empty$data), 0L)
  testthat::expect_identical(empty$data$id_2, numeric())
})

testthat::test_that("unknown periods are kept only explicitly and cannot be filtered or widened", {
  api <- .public_fixture()
  testthat::expect_error(api$bch_get_data(id = "3", catalog = api$catalog, progress = FALSE), class = "bch_unknown_frequency")
  kept <- api$bch_get_data(id = "3", catalog = api$catalog, unknown_frequency = "keep", progress = FALSE)
  testthat::expect_identical(kept$data$frequency, "unknown")
  testthat::expect_true(is.na(kept$data$period_start))
  testthat::expect_error(api$bch_get_data(id = "3", catalog = api$catalog,
                                        start = "2024-01-01", unknown_frequency = "keep", progress = FALSE),
                         class = "bch_unknown_frequency")
  before <- length(api$calls)
  testthat::expect_error(api$bch_get_data(id = "3", catalog = api$catalog, shape = "wide"), class = "bch_unknown_frequency")
  testthat::expect_error(api$bch_get_data(id = c("2", "3"), catalog = api$catalog, shape = "wide"), class = "bch_mixed_frequency")
  testthat::expect_identical(length(api$calls), before)
})

testthat::test_that("partial failure policy is atomic by default and explicit when collecting", {
  condition <- structure(list(message = "Synthetic unavailable series", call = NULL, attempts = 3L),
                          class = c("bch_http_error", "bch_error", "error", "condition"))
  api <- .public_fixture(failures = list("2" = condition))
  testthat::expect_error(api$bch_get_data(group = "SYN", catalog = api$catalog, progress = FALSE), class = "bch_http_error")
  result <- api$bch_get_data(group = "SYN", catalog = api$catalog, on_error = "collect", progress = FALSE)
  testthat::expect_identical(unique(result$data$indicator_id), "001")
  testthat::expect_identical(result$metadata$indicator_id, c("001", "2"))
  testthat::expect_identical(result$problems$indicator_id, "2")
  testthat::expect_identical(result$problems$attempts, 3L)
  testthat::expect_false(any(grepl("[?&]", result$problems$endpoint)))
  testthat::expect_identical(result$problems$class, "bch_http_error")
  all_failed <- api$bch_get_data(id = "2", catalog = api$catalog, on_error = "collect",
                                 shape = "wide", progress = FALSE)
  testthat::expect_identical(nrow(all_failed$data), 0L)
  testthat::expect_identical(nrow(all_failed$column_map), 0L)
  testthat::expect_identical(nrow(all_failed$problems), 1L)
})

testthat::test_that("parse failures retain retrieval provenance and unexpected errors are sanitized", {
  api <- .public_fixture()
  api$catalog$frequency <- rep("monthly", nrow(api$catalog))
  testthat::expect_error(api$bch_get_data(id = "2", catalog = api$catalog), class = "bch_invalid_frequency")
  testthat::expect_length(api$calls, 0L)
  api <- .public_fixture(series = list("2" = list(list(bad = "schema"))))
  result <- api$bch_get_data(id = "2", catalog = api$catalog, on_error = "collect", progress = FALSE)
  testthat::expect_identical(nrow(result$provenance), 1L)
  testthat::expect_identical(nrow(result$problems), 1L)
  secret_error <- simpleError("https://bad.example?clave=SENSITIVE")
  api <- .public_fixture(failures = list("2" = secret_error))
  result <- api$bch_get_data(id = "2", catalog = api$catalog, on_error = "collect", progress = FALSE)
  testthat::expect_false(any(grepl("SENSITIVE", unlist(result$problems))))
  stopped <- tryCatch(
    api$bch_get_data(id = "2", catalog = api$catalog, progress = FALSE),
    error = identity
  )
  testthat::expect_s3_class(stopped, "bch_unexpected_error")
  testthat::expect_false(any(grepl("SENSITIVE", unlist(stopped))))
  testthat::expect_null(stopped$call)
  testthat::expect_null(stopped$parent)
})

testthat::test_that("valid optional provenance fields survive heterogeneous bindings", {
  api <- .public_fixture()
  catalog <- api$bch_indicators()
  catalog$provenance <- catalog$provenance[
    c("retrieved_at", "source", "endpoint", "schema_version", "from_cache")
  ]
  catalog$provenance$catalog_note <- "Locally reviewed catalog"
  testthat::expect_invisible(validate_bch_result(catalog))
  before <- length(api$calls)
  result <- api$bch_get_data(id = "2", catalog = catalog, progress = FALSE)
  testthat::expect_length(api$calls, before + 1L)
  testthat::expect_identical(nrow(result$provenance), 2L)
  testthat::expect_identical(result$provenance$catalog_note,
                            c("Locally reviewed catalog", NA_character_))
  testthat::expect_identical(result$provenance$indicator_id, c(NA_character_, "2"))
  testthat::expect_s3_class(result$provenance$retrieved_at, "POSIXct")
  testthat::expect_invisible(validate_bch_result(result))
})

testthat::test_that("default preflight cap applies to 26 unique series only", {
  api <- .public_fixture()
  testthat::expect_error(api$bch_get_data(id = as.character(seq_len(26))),
                         class = "bch_series_limit")
  testthat::expect_length(api$calls, 0L)
  result <- api$bch_get_data(id = rep("2", 30), catalog = api$catalog, progress = FALSE)
  testthat::expect_length(api$calls, 1L)
  testthat::expect_identical(result$metadata$indicator_id, "2")
})

testthat::test_that("collected parse failures retain successful wide column meaning", {
  api <- .public_fixture(series = list(
    "001" = list(list(Id = "1", IndicadorId = "001", Nombre = "SYN-A",
                       Descripcion = "Synthetic", Fecha = "2024-01-01T00:00:00Z", Valor = 10)),
    "2" = list(list(bad = "schema"))
  ))
  result <- api$bch_get_data(group = "SYN", catalog = api$catalog, shape = "wide",
                             on_error = "collect", progress = FALSE)
  testthat::expect_identical(result$column_map$indicator_id, "001")
  testthat::expect_identical(result$metadata$indicator_id, c("001", "2"))
  testthat::expect_identical(result$problems$indicator_id, "2")
  testthat::expect_identical(result$provenance$indicator_id, c("001", "2"))
  testthat::expect_identical(result$data$id_001, 10)
  testthat::expect_false("id_2" %in% names(result$data))
})

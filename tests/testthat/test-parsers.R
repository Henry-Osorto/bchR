catalog_record <- function(...) {
  defaults <- list(
    Id = 97,
    Nombre = "EC-TCR-01",
    Descripcion = "Estadisticas Cambiarias - Tipo de Cambio de Referencia",
    Periodicidad = "Diario",
    Grupo = "EC-TCR",
    Correlativo = "01"
  )
  utils::modifyList(defaults, list(...))
}

series_record <- function(...) {
  defaults <- list(
    Id = 166472,
    IndicadorId = 97,
    Nombre = "EC-TCR-01",
    Descripcion = "Estadisticas Cambiarias - Tipo de Cambio de Referencia",
    Fecha = "2024-03-09T00:00:00Z",
    Valor = 24.6746
  )
  utils::modifyList(defaults, list(...), keep.null = TRUE)
}

testthat::test_that("only verified BCH frequencies are normalized", {
  testthat::expect_identical(
    .bch_normalize_frequency("Diario"),
    "daily"
  )
  testthat::expect_identical(
    .bch_normalize_frequency(c("Diario", "Mensual"), unknown = "keep"),
    c("daily", "unknown")
  )
  testthat::expect_error(
    .bch_normalize_frequency("Mensual"),
    class = "bch_unknown_frequency"
  )
  testthat::expect_error(
    .bch_normalize_frequency(1),
    class = "bch_invalid_frequency"
  )
})

testthat::test_that("Fecha parsing is strict ISO 8601 UTC", {
  parsed <- .bch_parse_datetime_utc(c(
    "2024-02-29T00:00:00Z",
    "2024-03-09T12:30:45.125Z"
  ))

  testthat::expect_s3_class(parsed, "POSIXct")
  testthat::expect_identical(attr(parsed, "tzone"), "UTC")
  testthat::expect_identical(
    as.Date(parsed, tz = "UTC"),
    as.Date(c("2024-02-29", "2024-03-09"))
  )

  testthat::expect_error(
    .bch_parse_datetime_utc("2024-02-30T00:00:00Z"),
    class = "bch_invalid_date"
  )
  testthat::expect_error(
    .bch_parse_datetime_utc("2024-03-09T00:00:00-06:00"),
    class = "bch_invalid_date"
  )
})

testthat::test_that("daily period fields use the UTC reference date", {
  dates <- as.Date(c("2024-02-29", "2024-03-09"))
  periods <- .bch_period_fields(dates, "daily")

  testthat::expect_identical(periods$period_start, dates)
  testthat::expect_identical(periods$period_end, dates)
  testthat::expect_identical(
    periods$period_label,
    c("2024-02-29", "2024-03-09")
  )

  unknown <- .bch_period_fields(dates[[1L]], "unknown")
  testthat::expect_true(is.na(unknown$period_start[[1L]]))
  testthat::expect_true(is.na(unknown$period_end[[1L]]))
  testthat::expect_true(is.na(unknown$period_label[[1L]]))
})

testthat::test_that("catalog parser preserves values and additional fields", {
  payload <- list(catalog_record(Id = "00097", CampoNuevo = "conservar"))
  parsed <- .bch_parse_catalog(payload)

  testthat::expect_s3_class(parsed, "tbl_df")
  testthat::expect_identical(parsed$indicator_id, "00097")
  testthat::expect_identical(parsed$indicator_code, "EC-TCR-01")
  testthat::expect_identical(parsed$frequency_raw, "Diario")
  testthat::expect_identical(parsed$frequency, "daily")
  testthat::expect_identical(parsed$extra[[1L]]$CampoNuevo, "conservar")
})

testthat::test_that("catalog parser has a stable empty schema", {
  parsed <- .bch_parse_catalog(list())

  testthat::expect_s3_class(parsed, "tbl_df")
  testthat::expect_equal(nrow(parsed), 0L)
  testthat::expect_named(
    parsed,
    c(
      "indicator_id",
      "indicator_code",
      "indicator_description",
      "frequency_raw",
      "frequency",
      "group_code",
      "correlative",
      "extra"
    )
  )
})

testthat::test_that("catalog schema and duplicate conflicts fail explicitly", {
  missing_id <- catalog_record()
  missing_id$Id <- NULL

  testthat::expect_error(
    .bch_parse_catalog(list(missing_id)),
    class = "bch_schema_error"
  )
  testthat::expect_error(
    .bch_parse_catalog(list(catalog_record(Id = -1))),
    class = "bch_schema_error"
  )
  testthat::expect_error(
    .bch_parse_catalog(
      list(
        catalog_record(),
        catalog_record(Descripcion = "Conflicto")
      )
    ),
    class = "bch_conflicting_duplicate"
  )
})

testthat::test_that("catalog unknown frequencies are kept or rejected explicitly", {
  kept <- .bch_parse_catalog(list(catalog_record(Periodicidad = "Mensual")))
  testthat::expect_identical(kept$frequency, "unknown")
  testthat::expect_identical(kept$frequency_raw, "Mensual")

  testthat::expect_error(
    .bch_parse_catalog(
      list(catalog_record(Periodicidad = "Mensual")),
      unknown_frequency = "error"
    ),
    class = "bch_unknown_frequency"
  )
})

testthat::test_that("series parser returns ordered typed daily observations", {
  payload <- list(
    series_record(
      Id = "166473",
      Fecha = "2024-03-10T00:00:00Z",
      Valor = "-1.25e1",
      NotaNueva = "preliminar"
    ),
    series_record(Id = 166472, Fecha = "2024-03-09T00:00:00Z")
  )

  parsed <- .bch_parse_series(
    payload,
    expected_id = "97",
    frequency_raw = "Diario"
  )

  testthat::expect_s3_class(parsed, "tbl_df")
  testthat::expect_identical(parsed$observation_id, c("166472", "166473"))
  testthat::expect_identical(parsed$indicator_id, c("97", "97"))
  testthat::expect_identical(
    parsed$reference_date,
    as.Date(c("2024-03-09", "2024-03-10"))
  )
  testthat::expect_equal(parsed$value, c(24.6746, -12.5))
  testthat::expect_identical(parsed$value_raw[[2L]], "-1.25e1")
  testthat::expect_identical(parsed$extra[[2L]]$NotaNueva, "preliminar")
  testthat::expect_false(any(parsed$is_transformed))
})

testthat::test_that("series parser retains null values as explicit missing data", {
  parsed <- .bch_parse_series(
    list(series_record(Valor = NULL)),
    frequency_raw = "Diario"
  )

  testthat::expect_true(is.na(parsed$value[[1L]]))
  testthat::expect_true(is.na(parsed$value_raw[[1L]]))
})

testthat::test_that("series decimal grammar does not guess locale conventions", {
  invalid <- c("24,6746", " 24.6746 ", "Inf", "--")

  for (value in invalid) {
    testthat::expect_error(
      .bch_parse_series(
        list(series_record(Valor = value)),
        frequency_raw = "Diario"
      ),
      class = "bch_invalid_value"
    )
  }
})

testthat::test_that("series dates and requested indicator are validated", {
  testthat::expect_error(
    .bch_parse_series(
      list(series_record(Fecha = "09/03/2024")),
      frequency_raw = "Diario"
    ),
    class = "bch_invalid_date"
  )
  testthat::expect_error(
    .bch_parse_series(
      list(series_record(IndicadorId = 98)),
      expected_id = "97",
      frequency_raw = "Diario"
    ),
    class = "bch_indicator_mismatch"
  )
})

testthat::test_that("unverified series frequency has strict and raw modes", {
  testthat::expect_error(
    .bch_parse_series(
      list(series_record()),
      frequency_raw = "Mensual"
    ),
    class = "bch_unknown_frequency"
  )

  kept <- .bch_parse_series(
    list(series_record()),
    frequency_raw = "Mensual",
    unknown_frequency = "keep"
  )
  testthat::expect_identical(kept$frequency, "unknown")
  testthat::expect_true(is.na(kept$period_start[[1L]]))
  testthat::expect_identical(kept$date_raw, "2024-03-09T00:00:00Z")
})

testthat::test_that("identical duplicates collapse and conflicting values abort", {
  duplicate <- list(
    series_record(Id = 1, Valor = 24.6746),
    series_record(Id = 2, Valor = 24.6746)
  )
  testthat::expect_warning(
    parsed <- .bch_parse_series(duplicate, frequency_raw = "Diario"),
    class = "bch_duplicate_collapsed"
  )
  testthat::expect_equal(nrow(parsed), 1L)

  conflict <- list(
    series_record(Id = 1, Valor = 24.6746),
    series_record(Id = 2, Valor = 25)
  )
  testthat::expect_error(
    .bch_parse_series(conflict, frequency_raw = "Diario"),
    class = "bch_conflicting_duplicate"
  )
})

testthat::test_that("catalog and payload frequencies must agree without rewriting raw labels", {
  matched <- .bch_parse_series(
    list(series_record(Periodicidad = "Diario")), frequency_raw = "Diario"
  )
  testthat::expect_identical(matched$frequency_raw, "Diario")
  testthat::expect_identical(matched$frequency, "daily")

  condition <- testthat::expect_error(
    .bch_parse_series(
      list(series_record(Periodicidad = "Mensual")), frequency_raw = "Diario"
    ),
    class = "bch_frequency_mismatch"
  )
  testthat::expect_identical(condition$frequency_raw, "Mensual")
  testthat::expect_identical(condition$catalog_frequency_raw, "Diario")
  testthat::expect_identical(condition$rows, 1L)
  testthat::expect_error(
    .bch_parse_series(
      list(series_record(Periodicidad = "diario")), frequency_raw = "Diario",
      unknown_frequency = "keep"
    ),
    class = "bch_frequency_mismatch"
  )
  testthat::expect_error(
    .bch_parse_series(list(series_record(Periodicidad = "Diario"),
                           series_record(Id = 2)), frequency_raw = "Diario"),
    class = "bch_schema_error"
  )
  from_payload <- .bch_parse_series(list(series_record(Periodicidad = "Diario")))
  testthat::expect_identical(from_payload$frequency_raw, "Diario")
})

testthat::test_that("unknown timestamps retain fractional precision independently of options", {
  payload <- list(
    series_record(Id = 1, Fecha = "2024-03-09T00:00:00.125Z"),
    series_record(Id = 2, Fecha = "2024-03-09T00:00:00.875Z")
  )
  withr::local_options(digits.secs = 0L)
  zero_digits <- .bch_parse_series(payload, frequency_raw = "Unverified",
                                    unknown_frequency = "keep")
  testthat::expect_identical(nrow(zero_digits), 2L)
  testthat::expect_identical(zero_digits$date_raw, vapply(payload, `[[`, "", "Fecha"))
  options(digits.secs = 3L)
  three_digits <- .bch_parse_series(payload, frequency_raw = "Unverified",
                                     unknown_frequency = "keep")
  testthat::expect_identical(zero_digits, three_digits)

  payload[[1L]]$Fecha <- "2024-03-09T00:00:00.100000001Z"
  payload[[2L]]$Fecha <- "2024-03-09T00:00:00.100000002Z"
  subprecision <- .bch_parse_series(payload, frequency_raw = "Unverified",
                                      unknown_frequency = "keep")
  testthat::expect_identical(nrow(subprecision), 2L)
  testthat::expect_identical(subprecision$date_raw, vapply(payload, `[[`, "", "Fecha"))
  payload[[1L]]$Id <- 2
  payload[[2L]]$Id <- 1
  reversed_ids <- .bch_parse_series(rev(payload), frequency_raw = "Unverified",
                                      unknown_frequency = "keep")
  testthat::expect_identical(reversed_ids$date_raw, subprecision$date_raw)
})

testthat::test_that("equivalent fractional spellings collapse without losing the retained raw text", {
  for (dates in list(c("2024-03-09T00:00:00.1Z", "2024-03-09T00:00:00.100Z"),
                    c("2024-03-09T00:00:10Z", "2024-03-09T00:00:10.000Z"))) {
    payload <- list(series_record(Id = 1, Fecha = dates[[1L]]),
                    series_record(Id = 2, Fecha = dates[[2L]]))
    testthat::expect_warning(
      parsed <- .bch_parse_series(payload, frequency_raw = "Unverified",
                                  unknown_frequency = "keep"),
      class = "bch_duplicate_collapsed"
    )
    testthat::expect_identical(nrow(parsed), 1L)
    testthat::expect_identical(parsed$date_raw, dates[[1L]])
    payload[[2L]]$Valor <- 99
    testthat::expect_error(
      .bch_parse_series(payload, frequency_raw = "Unverified", unknown_frequency = "keep"),
      class = "bch_conflicting_duplicate"
    )
  }
})

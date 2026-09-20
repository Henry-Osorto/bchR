testthat::test_that("indicator_id accepts appropriate numeric and character IDs", {
  testthat::expect_identical(.bch_validate_indicator_id(609), "609")
  testthat::expect_identical(.bch_validate_indicator_id(609L), "609")
  testthat::expect_identical(.bch_validate_indicator_id("609"), "609")
  testthat::expect_identical(.bch_validate_indicator_id(" 00609 "), "609")
})


testthat::test_that("indicator_id rejects invalid values", {
  invalid <- list(
    NA,
    NA_character_,
    numeric(),
    c(609, 610),
    0,
    -1,
    609.5,
    Inf,
    "",
    "   ",
    "abc",
    "609.0",
    "6e2",
    "-609",
    "0",
    TRUE
  )

  for (value in invalid) {
    testthat::expect_error(
      .bch_validate_indicator_id(value),
      "indicator_id must be a single positive whole-number BCH indicator ID",
      fixed = TRUE
    )
  }
})


testthat::test_that("bch_get_data validates progress", {
  testthat::expect_error(
    bch_get_data(609, progress = NA),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )

  testthat::expect_error(
    bch_get_data(609, progress = c(TRUE, FALSE)),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )

  testthat::expect_error(
    bch_get_data(609, progress = 1),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )
})


testthat::test_that("valid indicator data are typed, cleaned, and chronologically ordered", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records()
        )
      }
    )

    out <- bch_get_data(609, progress = FALSE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "Id",
        "IndicadorId",
        "Nombre",
        "Descripcion",
        "Fecha",
        "Valor"
      )
    )
    testthat::expect_s3_class(out$Fecha, "Date")
    testthat::expect_type(out$Valor, "double")

    testthat::expect_identical(
      as.character(out$Fecha),
      c("2024-01-01", "2024-02-01", "2024-03-01")
    )
    testthat::expect_equal(out$Valor, c(1.25, 2.25, 3.25))

    # Leading/trailing whitespace is removed without changing official text.
    testthat::expect_identical(
      out$Nombre,
      rep("TEST INDICATOR", 3L)
    )
    testthat::expect_identical(
      out$Descripcion,
      rep("Official test description", 3L)
    )

    testthat::expect_s3_class(attr(out, "retrieved_at"), "POSIXct")
    testthat::expect_true(is.character(attr(out, "package_version")))
    testthat::expect_identical(attr(out, "indicator_id"), "609")

    endpoint <- attr(out, "api_endpoint")
    testthat::expect_identical(
      endpoint,
      "https://bchapi-am.azure-api.net/api/v1/indicadores/609/cifras?formato=Json"
    )
    testthat::expect_false(grepl("clave=", endpoint, fixed = TRUE))
    testthat::expect_false(grepl(secret, endpoint, fixed = TRUE))
  })
})


testthat::test_that("character indicator IDs reach the same canonical endpoint", {
  .bchr_with_test_key({
    captured_url <- NULL

    httr2::local_mocked_responses(
      function(req) {
        captured_url <<- httr2::req_get_url(req)

        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records()
        )
      }
    )

    out <- bch_get_data(" 00609 ", progress = FALSE)

    testthat::expect_match(
      captured_url,
      "/api/v1/indicadores/609/cifras",
      fixed = TRUE
    )
    testthat::expect_identical(attr(out, "indicator_id"), "609")
  })
})


testthat::test_that("bch_get_data can be silent when progress is FALSE", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records()
        )
      }
    )

    testthat::expect_silent(
      bch_get_data(609, progress = FALSE)
    )
  })
})


testthat::test_that("bch_get_data reports progress without changing row counts", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records()
        )
      }
    )

    messages <- capture.output(
      invisible(bch_get_data(609, progress = TRUE)),
      type = "message"
    )

    testthat::expect_true(any(grepl(
      "Downloading BCH indicator 609...",
      messages,
      fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "Processing dates and values...",
      messages,
      fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "Finished downloading 3 observations.",
      messages,
      fixed = TRUE
    )))
  })
})


testthat::test_that("a valid empty JSON array becomes an empty-data error", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      list(
        httr2::response_json(
          status_code = 200L,
          body = list()
        )
      )
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      "returned no observations",
      class = "bchR_empty_data_error"
    )
  })
})


testthat::test_that("malformed Fecha values fail instead of becoming NA silently", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records(malformed_date = TRUE)
        )
      }
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      "malformed non-missing date",
      class = "bchR_data_date_error"
    )
  })
})


testthat::test_that("missing Valor values are preserved as numeric NA", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records(missing_value = TRUE)
        )
      }
    )

    out <- bch_get_data(609, progress = FALSE)

    testthat::expect_type(out$Valor, "double")
    testthat::expect_true(is.na(out$Valor[[2L]]))
    testthat::expect_equal(sum(is.na(out$Valor)), 1L)
  })
})


testthat::test_that("substantive duplicates are warned about and preserved", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records(include_duplicate = TRUE)
        )
      }
    )

    duplicate_warning <- NULL

    out <- withCallingHandlers(
      bch_get_data(609, progress = FALSE),
      warning = function(w) {
        if (inherits(w, "bchR_data_duplicate_warning")) {
          duplicate_warning <<- w
          invokeRestart("muffleWarning")
        }
      }
    )

    testthat::expect_s3_class(
      duplicate_warning,
      "bchR_data_duplicate_warning"
    )
    testthat::expect_match(
      conditionMessage(duplicate_warning),
      "1 substantive duplicate observation(s)",
      fixed = TRUE
    )
    testthat::expect_match(
      conditionMessage(duplicate_warning),
      "All 4 row(s) were preserved",
      fixed = TRUE
    )
    testthat::expect_equal(nrow(out), 4L)

    february <- out[out$Fecha == as.Date("2024-02-01"), , drop = FALSE]
    testthat::expect_equal(nrow(february), 2L)
    testthat::expect_identical(february$Id, c(102L, 202L))
    testthat::expect_equal(february$Valor, c(2.25, 2.25))
  })
})


testthat::test_that("new response fields are preserved and reported", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records(include_extra = TRUE)
        )
      }
    )

    schema_warning <- NULL

    out <- withCallingHandlers(
      bch_get_data(609, progress = FALSE),
      warning = function(w) {
        if (inherits(w, "bchR_data_schema_warning")) {
          schema_warning <<- w
          invokeRestart("muffleWarning")
        }
      }
    )

    testthat::expect_s3_class(schema_warning, "bchR_data_schema_warning")
    testthat::expect_match(
      conditionMessage(schema_warning),
      "UnidadPrueba",
      fixed = TRUE
    )
    testthat::expect_true("UnidadPrueba" %in% names(out))
    testthat::expect_identical(tail(names(out), 1L), "UnidadPrueba")
  })
})


testthat::test_that("missing required response fields are schema errors", {
  .bchr_with_test_key({
    records <- .bchr_data_records()
    records <- lapply(
      records,
      function(x) {
        x$Valor <- NULL
        x
      }
    )

    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = records
        )
      }
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      "Missing column(s): Valor",
      class = "bchR_data_schema_error",
      fixed = TRUE
    )
  })
})


testthat::test_that("a response for the wrong indicator is rejected", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_data_records(mismatch_indicator = TRUE)
        )
      }
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      "different from the requested indicator 609",
      class = "bchR_data_indicator_mismatch_error"
    )
  })
})


testthat::test_that("missing API key is propagated before network access", {
  .bchr_with_no_test_key({
    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      "BCH_API_KEY is not configured",
      fixed = TRUE
    )
  })
})


testthat::test_that("HTTP authentication errors remain distinguishable", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      list(httr2::response(status_code = 401L))
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      class = "bchR_authentication_error"
    )
  })
})


testthat::test_that("HTTP timeout errors propagate through bch_get_data", {
  .bchr_with_test_key({
    timeout_condition <- structure(
      list(
        message = "Operation timed out",
        call = NULL
      ),
      class = c("httr2_failure", "httr2_error", "error", "condition")
    )

    testthat::local_mocked_bindings(
      .bch_perform_request = function(request) {
        stop(timeout_condition)
      },
      .package = "bchR"
    )

    testthat::expect_error(
      bch_get_data(609, progress = FALSE),
      class = "bchR_timeout_error"
    )
  })
})


testthat::test_that("unsimplified list-of-records responses can be converted", {
  raw <- .bchr_data_records()

  out <- .bch_data_as_data_frame(raw)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_true(all(.bch_indicator_data_columns %in% names(out)))
})


testthat::test_that("character Valor values are converted only when safe", {
  data <- data.frame(
    Id = 1:3,
    IndicadorId = rep(609, 3),
    Nombre = rep("A", 3),
    Descripcion = rep("B", 3),
    Fecha = c(
      "2024-01-01T00:00:00",
      "2024-02-01T00:00:00",
      "2024-03-01T00:00:00"
    ),
    Valor = c(" 1.5 ", NA_character_, "2e1"),
    stringsAsFactors = FALSE
  )

  out <- .bch_clean_indicator_data(data, "609")
  testthat::expect_equal(out$Valor, c(1.5, NA_real_, 20))

  data$Valor[[2L]] <- "not-a-number"
  testthat::expect_error(
    .bch_clean_indicator_data(data, "609"),
    class = "bchR_data_value_error"
  )
})

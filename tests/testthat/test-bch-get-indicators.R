testthat::test_that("bch_get_indicators validates progress", {
  testthat::expect_error(
    bch_get_indicators(progress = NA),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )

  testthat::expect_error(
    bch_get_indicators(progress = c(TRUE, FALSE)),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )

  testthat::expect_error(
    bch_get_indicators(progress = 1),
    "progress must be TRUE or FALSE",
    fixed = TRUE
  )
})


testthat::test_that("bch_get_indicators returns the observed BCH schema", {
  .bchr_with_test_key({
    secret <- Sys.getenv("BCH_API_KEY")

    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_indicator_records()
        )
      }
    )

    out <- bch_get_indicators(progress = FALSE)

    testthat::expect_s3_class(out, "data.frame")
    testthat::expect_identical(
      names(out),
      c(
        "Id",
        "Nombre",
        "Descripcion",
        "Periodicidad",
        "Grupo",
        "CorrelativoGrupo"
      )
    )
    testthat::expect_equal(nrow(out), 2L)

    # The current API order is preserved; bchR does not impose an unverified
    # catalogue ordering of its own.
    testthat::expect_identical(out$Id, c(2L, 1L))

    # Character fields are trimmed, but official text is otherwise unchanged.
    testthat::expect_identical(out$Nombre, c("TEST-IND-2", "TEST-IND-1"))
    testthat::expect_identical(
      out$Descripcion,
      c("Second indicator description", "First indicator description")
    )
    testthat::expect_identical(out$Periodicidad, c("Mensual", "Anual"))
    testthat::expect_identical(out$Grupo, c("GROUP-A", "GROUP-A"))
    testthat::expect_type(out$CorrelativoGrupo, "character")
    testthat::expect_identical(out$CorrelativoGrupo, c("A2", "1"))

    testthat::expect_s3_class(attr(out, "retrieved_at"), "POSIXct")
    testthat::expect_true(is.character(attr(out, "package_version")))

    endpoint <- attr(out, "api_endpoint")
    testthat::expect_identical(
      endpoint,
      "https://bchapi-am.azure-api.net/api/v1/indicadores?formato=Json"
    )
    testthat::expect_false(grepl("clave=", endpoint, fixed = TRUE))
    testthat::expect_false(grepl(secret, endpoint, fixed = TRUE))
  })
})


testthat::test_that("bch_get_indicators can be silent when progress is FALSE", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_indicator_records()
        )
      }
    )

    testthat::expect_silent(
      bch_get_indicators(progress = FALSE)
    )
  })
})


testthat::test_that("bch_get_indicators reports its progress messages", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_indicator_records()
        )
      }
    )

    messages <- capture.output(
      invisible(bch_get_indicators(progress = TRUE)),
      type = "message"
    )

    testthat::expect_true(any(grepl(
      "Downloading BCH indicator catalogue...",
      messages,
      fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "Processing BCH indicator catalogue...",
      messages,
      fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "Finished downloading 2 indicators.",
      messages,
      fixed = TRUE
    )))
  })
})


testthat::test_that("an empty JSON array becomes an empty-catalogue error", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      httr2::response_json(
        status_code = 200L,
        body = list()
      )
    )

    testthat::expect_error(
      bch_get_indicators(progress = FALSE),
      "returned no indicators",
      class = "bchR_empty_catalogue_error"
    )
  })
})


testthat::test_that("missing expected fields are treated as a schema error", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_indicator_records(missing_correlative = TRUE)
        )
      }
    )

    condition <- tryCatch(
      bch_get_indicators(progress = FALSE),
      error = identity
    )

    testthat::expect_s3_class(condition, "bchR_catalogue_schema_error")
    testthat::expect_match(
      conditionMessage(condition),
      "CorrelativoGrupo",
      fixed = TRUE
    )
    testthat::expect_match(
      conditionMessage(condition),
      "schema may have changed",
      fixed = TRUE
    )
  })
})


testthat::test_that("new API fields are preserved and reported", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      function(req) {
        httr2::response_json(
          status_code = 200L,
          body = .bchr_indicator_records(include_extra = TRUE)
        )
      }
    )

    schema_warning <- NULL

    out <- withCallingHandlers(
      bch_get_indicators(progress = FALSE),
      warning = function(w) {
        if (inherits(w, "bchR_catalogue_schema_warning")) {
          schema_warning <<- w
        }
        invokeRestart("muffleWarning")
      }
    )

    testthat::expect_s3_class(
      schema_warning,
      "bchR_catalogue_schema_warning"
    )
    testthat::expect_match(
      conditionMessage(schema_warning),
      "NuevaColumna",
      fixed = TRUE
    )
    testthat::expect_true("NuevaColumna" %in% names(out))
    testthat::expect_identical(
      tail(names(out), 1L),
      "NuevaColumna"
    )
    testthat::expect_identical(
      out$NuevaColumna,
      c("new-value", "new-value")
    )
  })
})


testthat::test_that("missing API key is propagated before network access", {
  .bchr_with_no_test_key({
    testthat::expect_error(
      bch_get_indicators(progress = FALSE),
      "BCH_API_KEY is not configured",
      fixed = TRUE
    )
  })
})


testthat::test_that("HTTP-layer errors retain their dedicated class", {
  .bchr_with_test_key({
    httr2::local_mocked_responses(
      httr2::response(status_code = 401L)
    )

    testthat::expect_error(
      bch_get_indicators(progress = FALSE),
      class = "bchR_authentication_error"
    )
  })
})


testthat::test_that("list-of-records responses can be converted safely", {
  raw <- .bchr_indicator_records()

  out <- .bch_catalogue_as_data_frame(raw)

  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(.bch_indicator_catalogue_columns %in% names(out)))
})


testthat::test_that("missing values are preserved and CorrelativoGrupo stays character", {
  data <- data.frame(
    Id = c(1L, 2L),
    Nombre = c(" A ", NA_character_),
    Descripcion = c(NA_character_, " B "),
    Periodicidad = c("Mensual", NA_character_),
    Grupo = c(NA_character_, " G "),
    CorrelativoGrupo = c(1, NA_real_),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  out <- .bch_clean_indicator_catalogue(data)

  testthat::expect_identical(out$Nombre, c("A", NA_character_))
  testthat::expect_identical(out$Descripcion, c(NA_character_, "B"))
  testthat::expect_identical(out$Grupo, c(NA_character_, "G"))
  testthat::expect_identical(out$CorrelativoGrupo, c("1", NA_character_))
})
